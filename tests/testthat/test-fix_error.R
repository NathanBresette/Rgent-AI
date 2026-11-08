reset_errs <- function() {
  e <- rstudioai:::.rgent_errs
  rm(list = ls(e, all.names = TRUE), envir = e)
}

test_that("Fix error picks a Console error, not the code the chat ran before it", {
  reset_errs(); on.exit(reset_errs())
  e <- rstudioai:::.rgent_errs
  e$after <- "old error from before Rgent\n"
  # the chat ran plot code successfully
  rstudioai:::.rgent_note_chat_run("plot(cars)", list(success = TRUE))
  # then the user ran something in the Console that failed (seen at the next page message)
  try(stop("object 'regin' not found"), silent = TRUE)
  rstudioai:::.rgent_track_errors(function(...) NULL, NULL, FALSE, "{}")
  local_mocked_bindings(.rgent_console_history = function(n = 6) c("x <- 1", 'sales %>% filter(regin == "North")'),
                        .package = "rstudioai")
  err <- rstudioai:::.rgent_last_error()
  expect_equal(err$source, "console")
  expect_equal(err$code, 'sales %>% filter(regin == "North")')
  expect_match(err$error, "regin", fixed = TRUE)
  expect_false(grepl("plot(cars)", err$code, fixed = TRUE))
})

test_that("Fix error ignores old errors and Rgent's own", {
  reset_errs(); on.exit(reset_errs())
  try(stop("an old error"), silent = TRUE)
  rstudioai:::.rgent_install_error_tracker()          # errors from before tracking are old
  rstudioai:::.rgent_track_errors(function(...) try(stop("internal"), silent = TRUE), NULL, FALSE, "{}")  # Rgent's own try()
  rstudioai:::.rgent_track_errors(function(...) NULL, NULL, FALSE, "{}")
  expect_null(rstudioai:::.rgent_last_error())
  expect_null(rstudioai:::.rgent_error_note())
})

test_that("Fix error uses chat-run code when that failed last, and clears an error once its fix ran", {
  reset_errs(); on.exit(reset_errs())
  e <- rstudioai:::.rgent_errs
  e$after <- ""
  rstudioai:::.rgent_note_chat_run("summary(fitt)", list(success = FALSE, error = "object 'fitt' not found"))
  err <- rstudioai:::.rgent_last_error()
  expect_equal(err$source, "chat")
  expect_equal(err$code, "summary(fitt)")
  local_mocked_bindings(build_fix_context = function(...) "Similar names that exist: fit", .package = "rstudioai")
  note <- rstudioai:::.rgent_error_note(err)
  expect_match(note, "Similar names that exist: fit", fixed = TRUE)
  expect_match(note, "code the chat ran", fixed = TRUE)
  rstudioai:::.rgent_note_chat_run("summary(fit)", list(success = TRUE))   # the corrected code worked
  expect_null(rstudioai:::.rgent_last_error())
})
