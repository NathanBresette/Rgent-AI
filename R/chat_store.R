# Saved conversations
#
# Every chat the user starts is saved as one small JSON file in
# tools::R_user_dir("rstudioai", "data")/chats, so it can be reopened from the
# History list. Only the text of the user's messages and Claude's replies is
# kept (no plots, no data), and hard limits keep the folder small:
#   - at most .rgent_chat_limits$max_chats chats (oldest removed first)
#   - at most max_total_bytes for the whole folder
#   - each message cut at max_message_chars, each chat at max_messages
# Nothing is sent anywhere; the files stay on the user's computer.

.rgent_chat_limits <- list(max_chats = 100L, max_total_bytes = 5 * 1024^2,
                           max_message_chars = 10000L, max_messages = 100L)

.rgent_chat <- new.env(parent = emptyenv())
.rgent_chat$current <- NULL

.rgent_chat_dir <- function(create = FALSE) {
  d <- file.path(tools::R_user_dir("rstudioai", "data"), "chats")
  if (create && !dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.rgent_chat_file <- function(id) file.path(.rgent_chat_dir(), paste0(id, ".json"))

.rgent_chat_valid_id <- function(id) is.character(id) && length(id) == 1 && grepl("^[0-9]{8}-[0-9]{6}-[0-9a-f]{4}$", id)

.rgent_chat_clean <- function(text) {
  text <- paste(as.character(text), collapse = "\n")
  # Inline images (plots) would make files huge
  text <- gsub("data:[a-z]+/[a-z0-9.+-]+;base64,[A-Za-z0-9+/=]+", "[image]", text, perl = TRUE)
  max <- .rgent_chat_limits$max_message_chars
  if (nchar(text) > max) text <- paste0(substr(text, 1, max), "\n\n[... cut to save space]")
  text
}

.rgent_chat_title <- function(text) {
  t <- trimws(gsub("\\s+", " ", text))
  if (nchar(t) > 70) t <- paste0(substr(t, 1, 67), "...")
  if (!nzchar(t)) "Untitled chat" else t
}

.rgent_chat_write <- function(chat) {
  dir <- .rgent_chat_dir(create = TRUE)
  path <- .rgent_chat_file(chat$id)
  tmp <- paste0(path, ".tmp")
  writeLines(jsonlite::toJSON(chat, auto_unbox = TRUE, null = "null"), tmp, useBytes = TRUE)
  file.rename(tmp, path)
  .rgent_chat_prune()
  invisible(path)
}

# Keep the folder within its limits, removing the oldest chats first (never
# the open one)
.rgent_chat_prune <- function() {
  dir <- .rgent_chat_dir()
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  stale <- list.files(dir, pattern = "\\.json\\.tmp$", full.names = TRUE)
  if (length(stale)) unlink(stale)
  if (!length(files)) return(invisible(0L))
  info <- file.info(files)
  files <- files[order(info$mtime, decreasing = TRUE)]
  sizes <- file.info(files)$size
  keep_id <- if (!is.null(.rgent_chat$current)) .rgent_chat$current$id else ""
  is_current <- sub("\\.json$", "", basename(files)) == keep_id
  # newest first; the open chat always counts as kept
  ord <- c(which(is_current), which(!is_current))
  files <- files[ord]; sizes <- sizes[ord]
  keep <- seq_along(files) <= .rgent_chat_limits$max_chats &
    cumsum(sizes) <= .rgent_chat_limits$max_total_bytes
  keep[1] <- TRUE
  if (any(!keep)) unlink(files[!keep])
  invisible(sum(!keep))
}

#' Start a new saved chat (the next user message creates its file)
#' @keywords internal
chat_store_new <- function() {
  .rgent_chat$current <- NULL
  invisible(NULL)
}

#' Record a message in the open chat and save it
#'
#' A chat is created by the user's first message; Claude's greeting before
#' that isn't saved.
#' @keywords internal
chat_store_record <- function(role, message) {
  role <- if (identical(role, "user")) "user" else "ai"
  cur <- .rgent_chat$current
  if (is.null(cur)) {
    if (role != "user") return(invisible(NULL))
    now <- Sys.time()
    cur <- list(id = paste0(format(now, "%Y%m%d-%H%M%S"), "-", sprintf("%04x", sample.int(65535L, 1))),
                title = .rgent_chat_title(message),
                created = format(now, "%Y-%m-%dT%H:%M:%S%z"),
                messages = list())
  }
  cur$updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  cur$messages <- c(cur$messages, list(list(role = role, text = .rgent_chat_clean(message))))
  n <- length(cur$messages)
  if (n > .rgent_chat_limits$max_messages) cur$messages <- cur$messages[(n - .rgent_chat_limits$max_messages + 1):n]
  .rgent_chat$current <- cur
  tryCatch(.rgent_chat_write(cur), error = function(e) NULL)
  invisible(cur$id)
}

.rgent_chat_read <- function(id) {
  if (!.rgent_chat_valid_id(id)) return(NULL)
  path <- .rgent_chat_file(id)
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

#' List saved chats, newest first, with the folder's size
#' @keywords internal
chat_store_list <- function() {
  dir <- .rgent_chat_dir()
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  info <- file.info(files)
  files <- files[order(info$mtime, decreasing = TRUE)]
  chats <- lapply(files, function(f) {
    ch <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(ch) || is.null(ch$id)) return(NULL)
    list(id = ch$id, title = ch$title %||% "Untitled chat", updated = ch$updated %||% ch$created %||% "",
         messages = length(ch$messages))
  })
  chats <- Filter(Negate(is.null), chats)
  list(chats = chats, bytes = sum(info$size, na.rm = TRUE),
       current = if (!is.null(.rgent_chat$current)) .rgent_chat$current$id else NULL,
       limits = .rgent_chat_limits)
}

#' Reopen a saved chat: it becomes the open chat (new messages are added to
#' it) and its last messages become Claude's context again
#' @keywords internal
chat_store_open <- function(id) {
  ch <- .rgent_chat_read(id)
  if (is.null(ch)) return(NULL)
  .rgent_chat$current <- ch
  msgs <- ch$messages
  last <- utils::tail(msgs, .rgent_history_n)
  .rgent_state$conversation_history <- lapply(last, function(m) list(role = m$role, message = m$text, timestamp = Sys.time()))
  ch
}

#' @keywords internal
chat_store_delete <- function(id = NULL, all = FALSE) {
  if (isTRUE(all)) {
    unlink(list.files(.rgent_chat_dir(), pattern = "\\.json(\\.tmp)?$", full.names = TRUE))
    .rgent_chat$current <- NULL
    return(invisible(TRUE))
  }
  if (!.rgent_chat_valid_id(id)) return(invisible(FALSE))
  unlink(.rgent_chat_file(id))
  if (!is.null(.rgent_chat$current) && identical(.rgent_chat$current$id, id)) .rgent_chat$current <- NULL
  invisible(TRUE)
}

#' A saved chat as Markdown
#' @keywords internal
chat_store_markdown <- function(id) {
  ch <- .rgent_chat_read(id)
  if (is.null(ch)) return(NULL)
  parts <- vapply(ch$messages, function(m) {
    paste0(if (identical(m$role, "user")) "## You\n\n" else "## Rgent\n\n", m$text)
  }, "")
  paste0("# ", ch$title, "\n\n_Saved ", ch$updated %||% ch$created, "_\n\n", paste(parts, collapse = "\n\n"), "\n")
}
