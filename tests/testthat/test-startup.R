test_that("updates are checked at most once a day, after the pane opens", {
  old_cfg <- Sys.getenv("R_USER_CONFIG_DIR"); tmp <- tempfile("cfg")
  Sys.setenv(R_USER_CONFIG_DIR = tmp)
  st <- rstudioai:::.rgent_state
  old_flag <- st$update_checked
  on.exit({
    Sys.setenv(R_USER_CONFIG_DIR = old_cfg); unlink(tmp, recursive = TRUE)
    st$update_checked <- old_flag
  }, add = TRUE)
  st$update_checked <- NULL

  calls <- 0L
  local_mocked_bindings(.rgent_update_in_background = function(...) { calls <<- calls + 1L; invisible(NULL) }, .package = "rstudioai")
  expect_true(.rgent_update_due())
  expect_true(.rgent_schedule_update_check(delay = 0))
  if (requireNamespace("later", quietly = TRUE)) later::run_now(1)
  expect_equal(calls, 1L)
  expect_false(.rgent_update_due())                       # stamped for 24 hours
  expect_false(.rgent_schedule_update_check(delay = 0))   # same session: not again
  st$update_checked <- NULL
  expect_false(.rgent_schedule_update_check(delay = 0))   # new session, same day: not again
  writeLines(as.character(as.numeric(Sys.time()) - 25 * 3600), .rgent_update_stamp())
  expect_true(.rgent_update_due())                        # a day later: due
})

test_that("loading the package doesn't contact GitHub", {
  src <- deparse(body(rstudioai:::.onLoad)); att <- deparse(body(rstudioai:::.onAttach))
  expect_false(any(grepl("check_for_updates|GET\\(|install_github", c(src, att))))
})

test_that("the update runs in another R process, and the session only reads its status file", {
  st <- rstudioai:::.rgent_state
  # the script it runs: checks the version, installs only when newer, into the package's library
  sc <- rstudioai:::.rgent_update_script("2.4.12", "/some/lib", "/tmp/status.txt")
  expect_true(grepl("compareVersion(v, \"2.4.12\")", sc, fixed = TRUE))
  expect_true(grepl("lib = \"/some/lib\"", sc, fixed = TRUE))
  expect_true(grepl("upgrade = 'never'", sc, fixed = TRUE))
  expect_silent(parse(text = sc))
  # the watcher: no file yet means wait; "installed" announces it once
  status <- tempfile(); sent <- 0L
  local_mocked_bindings(.rgent_send_version_status = function(...) sent <<- sent + 1L, .package = "rstudioai")
  expect_null(rstudioai:::.rgent_update_watch(status, tempfile(), Sys.time(), poll = 60, give_up = 0))
  writeLines("installed", status)
  out <- capture.output(r <- rstudioai:::.rgent_update_watch(status, tempfile(), Sys.time(), poll = 60, give_up = 60))
  expect_equal(r, "installed"); expect_equal(sent, 1L); expect_true(any(grepl("Restart R", out)))
  expect_false(file.exists(status))
  writeLines("current", status)
  expect_silent(rstudioai:::.rgent_update_watch(status, tempfile(), Sys.time(), poll = 60, give_up = 60))
  expect_equal(sent, 1L)
})

test_that("a real background check against a fake GitHub doesn't block and reports back", {
  skip_on_cran()
  st <- rstudioai:::.rgent_state
  # Run the generated script with the URL pointed at a local DESCRIPTION saying the same version
  d <- tempfile(); writeLines(c("Package: rstudioai", "Version: 0.0.1"), d)
  status <- tempfile()
  sc <- sub("https://raw.githubusercontent.com/NathanBresette/Rgent-AI/master/DESCRIPTION", paste0("file://", d),
            rstudioai:::.rgent_update_script("9.9.9", tempdir(), status), fixed = TRUE)
  f <- tempfile(fileext = ".R"); writeLines(sc, f)
  t0 <- Sys.time()
  system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(f)), wait = FALSE)
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)   # returned at once
  for (i in 1:60) { if (file.exists(status)) break; Sys.sleep(0.25) }
  expect_equal(readLines(status), "current")
})
