# The daily update runs in a separate R process (2.4.12).
#
# check_and_update_package(auto_update = TRUE) ran in the user's session 3 s
# after the pane opened: the version download and install_github() kept the
# Console busy (and printed install logs) while the user was starting to work.
# Now an Rscript does both and writes one word to a status file; the session
# looks at that file every few seconds (later) and, when an update was
# installed, says so and shows the Restart R banner (version_status).

# r-universe builds the package from Rgent-AI about an hour after each push and serves
# ready-built binaries (no GitHub API, no remotes, nothing compiled), so updates come
# from there. Until it has the new version (just after a release), or if it can't be
# reached, the update falls back to install_github.
.rgent_runiverse <- "https://nathanbresette.r-universe.dev"

.rgent_update_script <- function(installed, lib, status, runiverse = .rgent_runiverse) {
  q <- function(x) deparse(x)
  paste0(
    "status <- tryCatch({\n",
    "  d <- readLines(url('https://raw.githubusercontent.com/NathanBresette/Rgent-AI/master/DESCRIPTION'), warn = FALSE)\n",
    "  v <- trimws(sub('^Version:', '', grep('^Version:', d, value = TRUE)[1]))\n",
    "  if (is.na(v) || utils::compareVersion(v, ", q(installed), ") <= 0) 'current'\n",
    "  else {\n",
    "    ru <- ", q(runiverse), "\n",
    "    ap <- tryCatch(utils::available.packages(repos = ru), error = function(e) NULL)\n",
    "    rv <- if (!is.null(ap) && 'rstudioai' %in% rownames(ap)) ap['rstudioai', 'Version'] else NA\n",
    "    if (!is.na(rv) && utils::compareVersion(rv, v) >= 0) {\n",
    "      utils::install.packages('rstudioai', repos = c(ru, 'https://cloud.r-project.org'), lib = ", q(lib), ", quiet = TRUE)\n",
    "    } else if (requireNamespace('remotes', quietly = TRUE)) {\n",
    "      remotes::install_github('NathanBresette/Rgent-AI', upgrade = 'never', force = TRUE, lib = ", q(lib), ", quiet = TRUE)\n",
    "    } else stop('the new version is not on r-universe yet and remotes is not installed')\n",
    "    now <- as.character(utils::packageVersion('rstudioai', lib.loc = ", q(lib), "))\n",
    "    if (utils::compareVersion(now, ", q(installed), ") <= 0) stop('the install did not take')\n",
    "    'installed'\n",
    "  }\n",
    "}, error = function(e) paste('error:', conditionMessage(e)))\n",
    "writeLines(status, ", q(status), ")\n"
  )
}

.rgent_update_in_background <- function(poll = 5, give_up = 15 * 60) {
  installed <- .rgent_installed_version() %||% as.character(utils::packageVersion("rstudioai"))
  lib <- dirname(find.package("rstudioai"))
  status <- tempfile("rgent-update-", fileext = ".txt")
  script <- tempfile("rgent-update-", fileext = ".R")
  writeLines(.rgent_update_script(installed, lib, status), script)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  withr_env <- c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  old <- Sys.getenv(names(withr_env), unset = NA)
  do.call(Sys.setenv, as.list(withr_env))
  on.exit(if (is.na(old)) Sys.unsetenv(names(withr_env)) else do.call(Sys.setenv, as.list(stats::setNames(old, names(withr_env)))), add = TRUE)
  system2(rscript, c("--vanilla", shQuote(script)), wait = FALSE, stdout = FALSE, stderr = FALSE)
  .rgent_update_watch(status, script, started = Sys.time(), poll = poll, give_up = give_up)
  invisible(status)
}

# Looks at the status file every few seconds; never blocks
.rgent_update_watch <- function(status, script, started, poll, give_up) {
  if (!file.exists(status)) {
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) < give_up && requireNamespace("later", quietly = TRUE)) {
      later::later(function() .rgent_update_watch(status, script, started, poll, give_up), poll)
    }
    return(invisible(NULL))
  }
  result <- tryCatch(readLines(status, n = 1, warn = FALSE), error = function(e) "")
  unlink(c(status, script))
  .rgent_state$last_update_result <- result
  if (identical(result, "installed")) {
    cat("\nRgent was updated. Restart R (Session > Restart R) to use the new version.\n")
    tryCatch(.rgent_send_version_status(), error = function(e) NULL)   # the chat shows the Restart R banner
  }
  invisible(result)
}
