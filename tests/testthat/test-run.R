quiet <- function(expr) suppressMessages(expr)

test_that("log_msg timestamps and records into the current run", {
  run <- quiet(run_start("t", "m@x.si"))
  line <- quiet(log_msg("value %d", 5L))
  expect_match(line, "^\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\] value 5$")
  expect_true(line %in% run$lines)
  expect_match(quiet(log_msg("100%")), "100%$")   # single string is not sprintf-ed
})

test_that("successful run with new data", {
  run <- quiet(run_start("t", "m@x.si", recipients = "u@x.si"))
  status <- quiet(with_run(run, {
    x <- run_step(run, "add", 1 + 1)
    run_summary(run, "Imported %d points", 2L)
    new_data(run)
  }, send = FALSE, quit_on_failure = FALSE))
  expect_equal(status, "ok")
  msgs <- route_notifications(run)
  expect_length(msgs, 2)
  expect_equal(msgs[[1]]$subject, "[OK] t")
  expect_equal(msgs[[2]]$to, "u@x.si")
  expect_match(msgs[[2]]$body, "Imported 2 points")
})

test_that("no new data: heartbeat only, and none for daily jobs", {
  run <- quiet(run_start("t", "m@x.si", recipients = "u@x.si"))
  quiet(with_run(run, NULL, send = FALSE, quit_on_failure = FALSE))
  msgs <- route_notifications(run)
  expect_length(msgs, 1)
  expect_match(msgs[[1]]$subject, "ni novih podatkov")

  daily <- quiet(run_start("d", "m@x.si", recipients = "u@x.si", heartbeat = FALSE))
  quiet(with_run(daily, NULL, send = FALSE, quit_on_failure = FALSE))
  expect_length(route_notifications(daily), 0)
})

test_that("non-critical step failure is an issue; run continues", {
  run <- quiet(run_start("t", "m@x.si", recipients = "u@x.si"))
  status <- quiet(with_run(run, {
    a <- run_step(run, "bad", stop("boom"), critical = FALSE, fallback = NA)
    run_step(run, "after", expect_true(is.na(a)))
    new_data(run)
  }, send = FALSE, quit_on_failure = FALSE))
  expect_equal(status, "issues")
  expect_equal(run$issues, "bad -- boom")
  msgs <- route_notifications(run)
  expect_equal(msgs[[1]]$subject, "[ISSUES] t")
  expect_match(msgs[[2]]$body, "bad -- boom")      # users see the warnings too
})

test_that("critical step failure stops the run, maintainer only", {
  run <- quiet(run_start("t", "m@x.si", recipients = "u@x.si"))
  reached <- FALSE
  status <- quiet(with_run(run, {
    run_step(run, "read", stop("no db"))
    reached <- TRUE
  }, send = FALSE, quit_on_failure = FALSE))
  expect_equal(status, "failed")
  expect_false(reached)
  expect_equal(run$error, "read -- no db")
  expect_equal(sum(grepl("FAILED", run$lines)), 1)  # not logged twice
  msgs <- route_notifications(run)
  expect_length(msgs, 1)
  expect_equal(msgs[[1]]$to, "m@x.si")
  expect_match(msgs[[1]]$subject, "^\\[FAILED\\]")
  expect_match(msgs[[1]]$body, "<pre", fixed = TRUE)
})

test_that("uncaught errors outside steps are caught too", {
  run <- quiet(run_start("t", "m@x.si"))
  status <- quiet(with_run(run, stop("raw"), send = FALSE, quit_on_failure = FALSE))
  expect_equal(status, "failed")
  expect_equal(run$error, "raw")
})

test_that("warnings are logged but do not fail the run", {
  run <- quiet(run_start("t", "m@x.si"))
  status <- quiet(with_run(run, {
    warning("careful")
    run_step(run, "s", 1)
  }, send = FALSE, quit_on_failure = FALSE))
  expect_equal(status, "ok")
  expect_equal(run$warnings, "careful")
  expect_match(route_notifications(run)[[1]]$body, "careful")
})

test_that("html is escaped", {
  run <- quiet(run_start("t", "m@x.si"))
  quiet(with_run(run, stop("a < b & c"), send = FALSE, quit_on_failure = FALSE))
  expect_match(route_notifications(run)[[1]]$body, "a &lt; b &amp; c", fixed = TRUE)
})

test_that("bot_auth fails clearly without credentials", {
  withr_env <- Sys.getenv("UMAR_BOT_CREDENTIALS")
  Sys.setenv(UMAR_BOT_CREDENTIALS = "")
  on.exit(Sys.setenv(UMAR_BOT_CREDENTIALS = withr_env))
  expect_error(bot_auth(), "UMAR_BOT_CREDENTIALS")
})
