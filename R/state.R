# Rgent's own state lives here, inside the package (2.4.12).
#
# It used to live in the user's global environment: the access code, the chat
# history (questions, replies, code), the server handle and the workspace index.
# So saving the workspace wrote the access code and the chat into .RData (a file
# people share and commit), and `rm(list = ls())` deleted them: every question
# then went out with the fallback code "DEMO123" and was refused until R restarted.
.rgent_state <- new.env(parent = emptyenv())
.rgent_state$conversation_history <- list()

# Objects earlier versions left in the user's global environment (and in .RData
# files saved since): the access code above all. Removed when Rgent starts, but
# only when they look like Rgent's own, so a user's object of the same name stays.
.rgent_clear_old_globals <- function(env = globalenv()) {
  ours <- list(
    current_access_code = function(x) is.character(x) && length(x) == 1,
    conversation_history = function(x) is.list(x) && all(vapply(x, function(m) is.list(m) && !is.null(m$role) && !is.null(m$message), logical(1))),
    websocket_server = function(x) inherits(x, c("WebServer", "R6")) || is.character(x),
    workspace_index = function(x) is.list(x) && all(c("objects", "data_frames") %in% names(x)),
    operation_stopped = function(x) is.logical(x) && length(x) == 1,
    auto_capture_active = function(x) is.logical(x) && length(x) == 1,
    last_error_message = function(x) is.character(x) && length(x) <= 1,
    last_error_context = function(x) is.list(x) && !is.null(x$error),
    currentStepCode = function(x) is.character(x),
    last_plot_command = function(x) is.character(x),
    last_plot_timestamp = function(x) inherits(x, "POSIXct"),
    .rgent_update_checked = function(x) isTRUE(x)
  )
  for (name in names(ours)) {
    if (exists(name, envir = env, inherits = FALSE) &&
        isTRUE(tryCatch(ours[[name]](get(name, envir = env)), error = function(e) FALSE))) {
      if (name == "current_access_code" && is.null(.rgent_state$current_access_code)) {
        .rgent_state$current_access_code <- get(name, envir = env)
      }
      rm(list = name, envir = env)
    }
  }
  invisible(TRUE)
}
