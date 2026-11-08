# Fix error: which error, which code, and what Claude is told.
#
# "Fix error" is an ordinary chat turn (it streams, auto-runs the corrected
# code and lands in History like any reply). The page sends `fix_error = TRUE`
# and chat_with_ai adds .rgent_error_note() to the prompt.
#
# Finding the right error: geterrmessage() keeps the last error forever, and
# Rgent's own try() calls change it too. So the error is tracked:
# - An error that appeared *between* two messages from the chat page came
#   from the Console (the user ran something). .rgent_track_errors() wraps
#   every message from the page and compares the message before and after.
# - An error in code the chat ran is recorded by the execute_code handler
#   (.rgent_note_chat_run()).
# - A Console command that succeeds clears the Console error (a task
#   callback, which R runs only after successful top-level commands).
# The newest of the two is the one to fix.

.rgent_errs <- new.env(parent = emptyenv())

.rgent_errmsg <- function() {
  m <- tryCatch(geterrmessage(), error = function(e) "")
  if (length(m) == 0 || is.na(m[1])) "" else m[1]
}

.rgent_install_error_tracker <- function() {
  if (is.null(.rgent_errs$after)) .rgent_errs$after <- .rgent_errmsg()   # errors from before Rgent are old
  if (!isTRUE(.rgent_errs$callback)) {
    ok <- tryCatch({
      addTaskCallback(function(...) {
        .rgent_errs$console <- NULL          # the last Console command worked
        .rgent_errs$after <- .rgent_errmsg()
        TRUE
      }, name = "rgent_error_tracker")
      TRUE
    }, error = function(e) FALSE)
    .rgent_errs$callback <- ok
  }
  invisible(TRUE)
}

# Wraps each message from the chat page (see start_websocket_server)
.rgent_track_errors <- function(handler, ws, isBinary, data) {
  now <- .rgent_errmsg()
  if (!is.null(.rgent_errs$after) && nzchar(now) && !identical(now, .rgent_errs$after)) {
    .rgent_errs$console <- list(error = now, time = Sys.time())
  }
  on.exit(.rgent_errs$after <- .rgent_errmsg(), add = TRUE)
  handler(ws, isBinary, data)
}

# Called by execute_code after the chat ran code
.rgent_note_chat_run <- function(code, result) {
  failed <- !is.null(result) && !isTRUE(result$success) && length(result$error) &&
    is.character(result$error) && nzchar(result$error[1])
  .rgent_errs$chat <- if (failed) {
    list(error = result$error[1], code = paste(code, collapse = "\n"), time = Sys.time())
  } else NULL
  # The corrected code from a Fix error turn ran: that error is fixed
  if (!failed && isTRUE(.rgent_errs$fixing)) .rgent_errs$console <- NULL
  if (!failed) .rgent_errs$fixing <- FALSE
}

# The Console commands the user ran last (RStudio's history)
.rgent_console_history <- function(n = 6) {
  f <- tempfile("rgent-history", fileext = ".R")
  on.exit(unlink(f), add = TRUE)
  lines <- tryCatch({ utils::savehistory(f); readLines(f, warn = FALSE) }, error = function(e) character(0))
  lines <- lines[nzchar(trimws(lines))]
  # Rgent's own calls aren't the user's code
  lines <- lines[!grepl("^\\s*(rstudioai::|library\\(rstudioai\\)|run_rgent\\()", lines)]
  utils::tail(lines, n)
}

#' The error to fix: list(error, code, source, recent) or NULL when there is none
#' @keywords internal
.rgent_last_error <- function() {
  con <- .rgent_errs$console
  chat <- .rgent_errs$chat
  pick <- if (!is.null(con) && !is.null(chat)) {
    if (difftime(chat$time, con$time, units = "secs") > 0) "chat" else "console"
  } else if (!is.null(chat)) "chat" else if (!is.null(con)) "console" else NULL
  if (is.null(pick)) {
    # Rgent wasn't tracking yet (just started): a Console error only if it's
    # not the one that was there when tracking started
    now <- .rgent_errmsg()
    if (is.null(.rgent_errs$after) && nzchar(now)) con <- list(error = now, time = Sys.time()) else return(NULL)
    pick <- "console"
  }
  if (pick == "chat") {
    return(list(error = trimws(chat$error), code = chat$code, source = "chat", recent = character(0)))
  }
  hist <- .rgent_console_history()
  list(error = trimws(con$error), code = if (length(hist)) hist[length(hist)] else "",
       source = "console", recent = utils::head(hist, -1))
}

#' What Claude is told when the user clicks Fix error (NULL: no error to fix)
#' @keywords internal
.rgent_error_note <- function(err = .rgent_last_error()) {
  if (is.null(err)) return(NULL)
  .rgent_errs$fixing <- TRUE
  ctx <- tryCatch(build_fix_context(err$code, err$error,
                                    doc = tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)),
                  error = function(e) "")
  where <- if (identical(err$source, "chat")) "code the chat ran" else "the Console"
  paste0(
    "\n\n=== THE ERROR TO FIX ===\n",
    "Code that failed (from ", where, "):\n```\n", err$code, "\n```\n",
    "Error:\n", substr(err$error, 1, 1500), "\n",
    if (length(err$recent)) paste0("Console commands just before it:\n", paste(err$recent, collapse = "\n"), "\n") else "",
    if (nzchar(ctx)) paste0("\n", ctx, "\n") else "",
    "\nFix it. Say what went wrong in one or two plain sentences, then give the corrected code in a single ```r block. ",
    "It may run straight away, so make it complete (not just the changed line) and use only objects that exist. ",
    "If the fix belongs in the open file, also say which line to change. No headings, no numbered sections."
  )
}
