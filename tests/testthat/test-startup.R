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

test_that("the update installs from r-universe when it has the new version, else falls back to GitHub", {
  skip_on_cran()
  # a fake GitHub DESCRIPTION and a fake r-universe (a local CRAN-style repo with a tiny rstudioai 9.9.10)
  root <- tempfile("ru"); dir.create(root)
  pkg <- file.path(root, "src", "rstudioai"); dir.create(file.path(pkg, "R"), recursive = TRUE)
  writeLines(c("Package: rstudioai", "Version: 9.9.10", "Title: Fake", "Description: Fake.", "License: MIT",
               "Authors@R: person('A', 'B', email = 'a@b.org', role = c('aut', 'cre'))"), file.path(pkg, "DESCRIPTION"))
  writeLines("", file.path(pkg, "NAMESPACE")); writeLines("f <- function() 1", file.path(pkg, "R", "f.R"))
  contrib <- file.path(root, "repo", "src", "contrib"); dir.create(contrib, recursive = TRUE)
  old <- setwd(contrib); on.exit(setwd(old), add = TRUE)
  system2(file.path(R.home("bin"), "R"), c("CMD", "build", "--no-build-vignettes", shQuote(pkg)), stdout = FALSE, stderr = FALSE)
  setwd(old)
  tools::write_PACKAGES(contrib, type = "source")
  gh <- tempfile(); writeLines(c("Package: rstudioai", "Version: 9.9.10"), gh)
  run <- function(repo_version_ok = TRUE) {
    lib <- tempfile("lib"); dir.create(lib); status <- tempfile()
    sc <- rstudioai:::.rgent_update_script("9.9.9", lib, status, runiverse = paste0("file://", file.path(root, "repo")))
    sc <- sub("https://raw.githubusercontent.com/NathanBresette/Rgent-AI/master/DESCRIPTION", paste0("file://", gh), sc, fixed = TRUE)
    sc <- sub("remotes::install_github(", "stop('fallback to GitHub'); invisible(", sc, fixed = TRUE)   # never touch the network
    sc <- sub("requireNamespace('remotes', quietly = TRUE)", "TRUE", sc, fixed = TRUE)          # whether or not remotes is installed
    sc <- sub("'https://cloud.r-project.org'", "NULL", sc, fixed = TRUE)
    if (!repo_version_ok) writeLines(c("Package: rstudioai", "Version: 9.9.11"), gh)                # GitHub ahead of r-universe
    f <- tempfile(fileext = ".R"); writeLines(c("options(pkgType = 'source')", sc), f)
    system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(f)), stdout = FALSE, stderr = FALSE)
    list(status = readLines(status), lib = lib)
  }
  a <- run()
  expect_equal(a$status, "installed")
  expect_equal(as.character(utils::packageVersion("rstudioai", lib.loc = a$lib)), "9.9.10")
  b <- run(repo_version_ok = FALSE)   # r-universe hasn't built 9.9.11 yet: GitHub instead
  expect_match(b$status, "fallback to GitHub")
})

test_that("auto-start starts Rgent once per R session (Restart R fired it twice and the pane reset at the first code run)", {
  st <- rstudioai:::.rgent_state
  old <- st$started; on.exit(st$started <- old, add = TRUE)
  st$started <- NULL
  runs <- 0L
  local_mocked_bindings(run_rgent = function() { runs <<- runs + 1L; st$started <- TRUE; invisible(TRUE) },
                        .package = "rstudioai")
  local_mocked_bindings(isAvailable = function(...) TRUE, .package = "rstudioapi")
  expect_true(auto_start_rgent())   # .Rprofile, right away
  expect_true(auto_start_rgent())   # the sessionInit hook / .onLoad's callback at the first command
  expect_true(auto_start_rgent())
  expect_equal(runs, 1L)
})

test_that("check_and_update_package(auto_update = TRUE) installs from r-universe, else GitHub (no remotes needed for r-universe)", {
  skip_on_cran()
  root <- tempfile("ru2"); dir.create(root)
  pkg <- file.path(root, "src", "rstudioai"); dir.create(file.path(pkg, "R"), recursive = TRUE)
  writeLines(c("Package: rstudioai", "Version: 9.9.10", "Title: Fake", "Description: Fake.", "License: MIT",
               "Authors@R: person('A', 'B', email = 'a@b.org', role = c('aut', 'cre'))"), file.path(pkg, "DESCRIPTION"))
  writeLines("", file.path(pkg, "NAMESPACE")); writeLines("f <- function() 1", file.path(pkg, "R", "f.R"))
  contrib <- file.path(root, "repo", "src", "contrib"); dir.create(contrib, recursive = TRUE)
  old <- setwd(contrib); on.exit(setwd(old), add = TRUE)
  system2(file.path(R.home("bin"), "R"), c("CMD", "build", "--no-build-vignettes", shQuote(pkg)), stdout = FALSE, stderr = FALSE)
  setwd(old)
  tools::write_PACKAGES(contrib, type = "source")
  repo <- paste0("file://", file.path(root, "repo"))
  withr_opts <- options(pkgType = "source", repos = c(CRAN = repo)); on.exit(options(withr_opts), add = TRUE)

  lib <- tempfile("lib"); dir.create(lib)
  suppressMessages(rstudioai:::.rgent_install_update("9.9.10", lib = lib, runiverse = repo))
  expect_equal(as.character(utils::packageVersion("rstudioai", lib.loc = lib)), "9.9.10")

  # r-universe hasn't built 9.9.11 yet: GitHub (remotes) instead
  skip_if_not_installed("remotes")
  called <- NULL
  local_mocked_bindings(install_github = function(repo, ...) called <<- repo, .package = "remotes")
  rstudioai:::.rgent_install_update("9.9.11", lib = tempfile(), runiverse = repo)
  expect_equal(called, "NathanBresette/Rgent-AI")
})
