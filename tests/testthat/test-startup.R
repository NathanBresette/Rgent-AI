test_that("updates are checked at most once a day, after the pane opens", {
  old_cfg <- Sys.getenv("R_USER_CONFIG_DIR"); tmp <- tempfile("cfg")
  Sys.setenv(R_USER_CONFIG_DIR = tmp)
  had <- exists(".rgent_update_checked", envir = .GlobalEnv)
  if (had) old_flag <- get(".rgent_update_checked", envir = .GlobalEnv)
  on.exit({
    Sys.setenv(R_USER_CONFIG_DIR = old_cfg); unlink(tmp, recursive = TRUE)
    if (had) assign(".rgent_update_checked", old_flag, envir = .GlobalEnv) else suppressWarnings(rm(".rgent_update_checked", envir = .GlobalEnv))
  }, add = TRUE)
  suppressWarnings(rm(".rgent_update_checked", envir = .GlobalEnv))

  calls <- 0L
  local_mocked_bindings(check_and_update_package = function(...) { calls <<- calls + 1L; FALSE }, .package = "rstudioai")
  expect_true(.rgent_update_due())
  expect_true(.rgent_schedule_update_check(delay = 0))
  if (requireNamespace("later", quietly = TRUE)) later::run_now(1)
  expect_equal(calls, 1L)
  expect_false(.rgent_update_due())                       # stamped for 24 hours
  expect_false(.rgent_schedule_update_check(delay = 0))   # same session: not again
  rm(".rgent_update_checked", envir = .GlobalEnv)
  expect_false(.rgent_schedule_update_check(delay = 0))   # new session, same day: not again
  writeLines(as.character(as.numeric(Sys.time()) - 25 * 3600), .rgent_update_stamp())
  expect_true(.rgent_update_due())                        # a day later: due
})

test_that("loading the package doesn't contact GitHub", {
  src <- deparse(body(rstudioai:::.onLoad)); att <- deparse(body(rstudioai:::.onAttach))
  expect_false(any(grepl("check_for_updates|GET\\(|install_github", c(src, att))))
})
