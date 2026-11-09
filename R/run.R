# package-level state: the run currently inside with_run(), so that log_msg()
# can record lines without the run object being passed around
.bot <- new.env(parent = emptyenv())

#' Start a run
#'
#' Creates the run object that collects log lines, issues, warnings, summary
#' lines and the new-data flag. Pass it to [with_run()], [run_step()],
#' [run_summary()] and [new_data()].
#'
#' @param name short run name, used in email subjects, e.g. "davcne-platform"
#' @param maintainer email(s) that get failures, issues and heartbeats
#' @param recipients email(s) of users who get the "data updated" notice.
#'   May be empty.
#' @param heartbeat logical. If TRUE (default) the maintainer also gets a short
#'   email after every successful run, so a run that never happened is noticed.
#'   Use FALSE for daily jobs.
#' @param user_intro HTML text that opens the users' email.
#'
#' @return a `bot_run` object (an environment)
#' @export
run_start <- function(name, maintainer, recipients = character(),
                      heartbeat = TRUE, user_intro = NULL) {
  stopifnot(is.character(name), length(name) == 1,
            is.character(maintainer), length(maintainer) >= 1,
            is.character(recipients), is.logical(heartbeat))
  run <- new.env(parent = emptyenv())
  run$name <- name
  run$maintainer <- maintainer
  run$recipients <- recipients
  run$heartbeat <- heartbeat
  run$user_intro <- user_intro
  run$start <- Sys.time()
  run$end <- NULL
  run$lines <- character()
  run$issues <- character()
  run$warnings <- character()
  run$summary <- character()
  run$new_data <- FALSE
  run$error <- NULL
  class(run) <- "bot_run"
  .bot$current <- run
  log_msg("Run started: %s", name)
  run
}

#' @export
print.bot_run <- function(x, ...) {
  cat("<bot_run> ", x$name, " | status: ", run_status(x),
      " | issues: ", length(x$issues), " | new data: ", x$new_data, "\n", sep = "")
  invisible(x)
}

#' Timestamped log line
#'
#' Writes `[YYYY-mm-dd HH:MM:SS] text` to stderr (which the .bat redirect
#' captures) and records it in the current run, if there is one.
#'
#' @param ... passed to [sprintf()]; a single string is used as is
#' @return the line, invisibly
#' @export
log_msg <- function(...) {
  args <- list(...)
  text <- if (length(args) == 1) as.character(args[[1]]) else sprintf(...)
  line <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), text)
  base::message(line)
  if (!is.null(.bot$current)) .bot$current$lines <- c(.bot$current$lines, line)
  invisible(line)
}

#' Run one step of a runner
#'
#' Evaluates `expr` and logs the outcome. A failing critical step stops the
#' run (the error is caught by [with_run()]); a failing non-critical step is
#' recorded as an issue and returns `fallback`, so the run continues.
#'
#' @param run a `bot_run`
#' @param label short description, used in the log and in emails
#' @param expr the code to run
#' @param critical logical, default TRUE
#' @param fallback value returned when a non-critical step fails
#' @return the value of `expr`, or `fallback`
#' @export
run_step <- function(run, label, expr, critical = TRUE, fallback = NULL) {
  tryCatch({
    out <- expr
    log_msg("OK: %s", label)
    out
  }, error = function(e) {
    msg <- sprintf("%s -- %s", label, conditionMessage(e))
    log_msg("FAILED: %s", msg)
    if (critical) {
      stop(structure(class = c("bot_step_error", "error", "condition"),
                     list(message = msg, call = NULL)))
    }
    run$issues <- c(run$issues, msg)
    fallback
  })
}

#' Add a line to the run summary
#'
#' Summary lines go into every email of the run.
#'
#' @param run a `bot_run`
#' @param ... passed to [sprintf()]
#' @return the run, invisibly
#' @export
run_summary <- function(run, ...) {
  line <- if (length(list(...)) == 1) as.character(..1) else sprintf(...)
  run$summary <- c(run$summary, line)
  log_msg(line)
  invisible(run)
}

#' Flag whether the run produced new data
#'
#' Only runs with new data send the users' notice.
#'
#' @param run a `bot_run`
#' @param flag logical
#' @return the run, invisibly
#' @export
new_data <- function(run, flag = TRUE) {
  stopifnot(is.logical(flag), length(flag) == 1, !is.na(flag))
  run$new_data <- flag
  invisible(run)
}

#' Run status
#'
#' @param run a `bot_run`
#' @return "failed" (uncaught or critical error), "issues" (non-critical
#'   steps failed) or "ok"
#' @export
run_status <- function(run) {
  if (!is.null(run$error)) "failed"
  else if (length(run$issues) > 0) "issues"
  else "ok"
}

#' Wrap a whole runner
#'
#' Evaluates the runner body, catches any error, logs warnings without
#' failing, then routes and sends the emails. In a non-interactive session a
#' failed run ends R with exit status 1, so the .bat file and Task Scheduler
#' see the failure.
#'
#' @param run a `bot_run`
#' @param expr the runner body
#' @param send logical, send emails (FALSE for testing)
#' @param quit_on_failure logical, default `!interactive()`
#' @return the run status, invisibly
#' @export
with_run <- function(run, expr, send = TRUE, quit_on_failure = !interactive()) {
  .bot$current <- run
  on.exit(.bot$current <- NULL, add = TRUE)

  tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      msg <- conditionMessage(w)
      run$warnings <- c(run$warnings, msg)
      log_msg("WARNING: %s", msg)
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      run$error <- conditionMessage(e)
      if (!inherits(e, "bot_step_error")) log_msg("FAILED: %s", run$error)
    })

  run$end <- Sys.time()
  status <- run_status(run)
  log_msg("Run finished: %s (%d issue(s), %d warning(s))",
          status, length(run$issues), length(run$warnings))

  msgs <- route_notifications(run)
  if (send && length(msgs) > 0) {
    tryCatch({
      bot_auth()
      for (m in msgs) send_bot_email(m$to, m$subject, m$body)
      log_msg("Sent %d email(s).", length(msgs))
    }, error = function(e) log_msg("FAILED to send email: %s", conditionMessage(e)))
  }

  if (status == "failed" && quit_on_failure) quit(save = "no", status = 1)
  invisible(status)
}
