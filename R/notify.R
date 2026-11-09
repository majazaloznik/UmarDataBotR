#' Decide who gets which email
#'
#' Pure function (no sending), so the routing rule is testable:
#'
#' | status | maintainer | users |
#' |---|---|---|
#' | failed | error, issues, full log | nothing |
#' | issues | issue list, full log | notice with warnings, if new data |
#' | ok, new data | heartbeat (if `heartbeat`) | notice |
#' | ok, no new data | heartbeat (if `heartbeat`) | nothing |
#'
#' @param run a `bot_run`
#' @return list of messages, each `list(to, subject, body)`
#' @export
route_notifications <- function(run) {
  status <- run_status(run)
  msgs <- list()
  sign <- "<br><br>Tvoj Umar Data Bot &#129302;"

  summary_html <- if (length(run$summary) > 0)
    paste0("<br><br>", paste(html_escape(run$summary), collapse = "<br>")) else ""
  issues_html <- if (length(run$issues) > 0)
    paste0("<br><br><b>Opozorila (", length(run$issues), "):</b><br>",
           paste(html_escape(run$issues), collapse = "<br>")) else ""
  warnings_html <- if (length(run$warnings) > 0)
    paste0("<br><br><b>R warnings (", length(run$warnings), "):</b><br>",
           paste(html_escape(unique(run$warnings)), collapse = "<br>")) else ""
  log_html <- paste0("<br><br><b>Log:</b><pre style=\"font-size:11px\">",
                     paste(html_escape(run$lines), collapse = "\n"), "</pre>")

  # maintainer
  if (status == "failed") {
    msgs[[length(msgs) + 1]] <- list(
      to = run$maintainer,
      subject = sprintf("[FAILED] %s", run$name),
      body = paste0("<b>Napaka:</b> ", html_escape(run$error),
                    summary_html, issues_html, warnings_html, log_html, sign))
  } else if (status == "issues") {
    msgs[[length(msgs) + 1]] <- list(
      to = run$maintainer,
      subject = sprintf("[ISSUES] %s", run$name),
      body = paste0("Tek se je zaklju\u010dil z opozorili.",
                    summary_html, issues_html, warnings_html, log_html, sign))
  } else if (run$heartbeat) {
    msgs[[length(msgs) + 1]] <- list(
      to = run$maintainer,
      subject = sprintf("[OK] %s%s", run$name,
                        if (run$new_data) "" else " - ni novih podatkov"),
      body = paste0("Tek uspe\u0161no zaklju\u010den.", summary_html, warnings_html, sign))
  }

  # users
  if (status != "failed" && run$new_data && length(run$recipients) > 0) {
    intro <- if (is.null(run$user_intro))
      sprintf("To je avtomatsko generirano sporo\u010dilo o posodobitvi podatkov (%s).",
              html_escape(run$name)) else run$user_intro
    msgs[[length(msgs) + 1]] <- list(
      to = run$recipients,
      subject = sprintf("Posodobitev: %s", run$name),
      body = paste0(intro, summary_html, issues_html, sign))
  }
  msgs
}

#' Authenticate the bot's Gmail account
#'
#' One place for credentials, configured by environment variables so that no
#' runner carries its own copy:
#' * `UMAR_BOT_CREDENTIALS` - path to the OAuth client JSON (required)
#' * `UMAR_BOT_EMAIL` - default "umar.data.bot@gmail.com"
#' * `UMAR_BOT_CACHE` - token cache directory, default ".secret"
#'
#' @return nothing useful
#' @export
bot_auth <- function() {
  cred <- Sys.getenv("UMAR_BOT_CREDENTIALS")
  if (!nzchar(cred) || !file.exists(cred))
    stop("UMAR_BOT_CREDENTIALS is not set or the file does not exist: '", cred, "'")
  gmailr::gm_auth_configure(path = cred)
  gmailr::gm_auth(email = Sys.getenv("UMAR_BOT_EMAIL", "umar.data.bot@gmail.com"),
                  cache = Sys.getenv("UMAR_BOT_CACHE", ".secret"))
}

#' Send one email from the bot account
#'
#' Recipients go in BCC.
#'
#' @param to character vector of addresses
#' @param subject subject line
#' @param body HTML body
#' @return the gmailr response, invisibly
#' @export
send_bot_email <- function(to, subject, body) {
  msg <- gmailr::gm_mime() |>
    gmailr::gm_bcc(to) |>
    gmailr::gm_subject(subject) |>
    gmailr::gm_html_body(body)
  invisible(gmailr::gm_send_message(msg))
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
