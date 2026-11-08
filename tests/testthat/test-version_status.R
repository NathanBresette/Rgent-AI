# After an update, R keeps running the old code until it restarts (2.4.7)

test_that("the running and installed versions are compared", {
  st <- rstudioai:::.rgent_version_status()
  expect_equal(st$action, "version_status")
  expect_equal(st$running, as.character(getNamespaceVersion("rstudioai")))
  expect_false(st$stale)                        # this test runs the installed build
  local_mocked_bindings(.rgent_installed_version = function() "99.0.0", .package = "rstudioai")
  st2 <- rstudioai:::.rgent_version_status()
  expect_true(st2$stale)
  expect_equal(st2$installed, "99.0.0")
  local_mocked_bindings(.rgent_installed_version = function() "0.0.1", .package = "rstudioai")
  expect_false(rstudioai:::.rgent_version_status()$stale)     # older on disk: nothing to restart for
  local_mocked_bindings(.rgent_installed_version = function() NULL, .package = "rstudioai")
  expect_false(rstudioai:::.rgent_version_status()$stale)
})

test_that("the page served is the one loaded with this code, not the file on disk", {
  ld <- rstudioai:::.rgent_loaded
  old <- ld$page
  on.exit(ld$page <- old, add = TRUE)
  # .onLoad keeps the installed page
  lib <- tempfile("lib"); dir.create(file.path(lib, "pkg", "public"), recursive = TRUE)
  writeLines("<html>loaded with the code</html>", file.path(lib, "pkg", "public", "websocket_chat.html"))
  rstudioai:::.rgent_remember_page(lib, "pkg")
  expect_equal(rawToChar(ld$page), "<html>loaded with the code</html>\n")
  # an update then replaces the file on disk; the pane still gets the loaded copy
  shown <- NULL
  local_mocked_bindings(viewer = function(url, ...) shown <<- url, .package = "rstudioapi")
  rstudioai:::launch_html_interface()
  expect_equal(readLines(shown), "<html>loaded with the code</html>")
  # nothing kept (couldn't be read): the file on disk, as before
  ld$page <- NULL
  rstudioai:::launch_html_interface()
  expect_true(any(grepl("update-banner", readLines(shown, warn = FALSE), fixed = TRUE)))
})

test_that("Restart R restarts and reopens Rgent once", {
  cmd <- NULL
  local_mocked_bindings(restartSession = function(command = "", clean = FALSE) cmd <<- command, .package = "rstudioapi")
  rstudioai:::.rgent_restart_r()
  expect_match(cmd, "rstudioai::run_rgent()", fixed = TRUE)
  # .Rprofile's auto-start already opened it (rgent.started set): the command does nothing
  ran <- FALSE
  withr::local_options(rgent.started = TRUE)
  eval(parse(text = sub("rstudioai::run_rgent()", "ran <- TRUE", cmd, fixed = TRUE)))
  expect_false(ran)
  withr::local_options(rgent.started = NULL)
  eval(parse(text = sub("rstudioai::run_rgent()", "ran <- TRUE", cmd, fixed = TRUE)))
  expect_true(ran)
})
