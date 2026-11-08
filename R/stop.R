# The chat's Stop button (2.4.12).
#
# R handles one page message at a time, and it is busy while a reply streams
# (httr::POST blocks) or code runs. Before 2.4.12 the page's `stop_response`
# was only read after the whole reply had arrived: the page deleted what it had
# shown, Claude kept writing to the end (billed, and R stayed busy, so the next
# question waited), and the hidden reply still went into the history.
#
# Now, while a reply streams or code runs, R looks at the page's messages
# itself (.rgent_poll_messages: curl's progress callback, about 10 times a
# second, and between the top-level statements of code it runs). A
# `stop_response` sets the stop flag; any other message waits in a queue and is
# handled, in order, once the current one is done, so nothing else runs in the
# middle of a stream or of the user's code.
#
# A stream ends at its next chunk: the write callback raises `rgent_stopped`.
# (Raising an error from curl's progress callback once data flows escapes
# tryCatch and halts R, so that callback only aborts before the first byte, by
# returning FALSE.) Closing the connection makes the backend stop Claude; what
# was written is still billed and recorded (backend `generate_stream`).

.rgent_stream_state <- new.env(parent = emptyenv())
.rgent_stream_state$active <- FALSE   # a reply is streaming or code is running
.rgent_stream_state$stop <- FALSE     # the page asked to stop it
.rgent_stream_state$queue <- list()   # messages that came in meanwhile

# The page messages that can be stopped
.rgent_stoppable_actions <- c("chat_with_ai", "execute_code")

.rgent_stop_requested <- function() isTRUE(.rgent_stream_state$stop)

.rgent_stopped_condition <- function() {
  structure(class = c("rgent_stopped", "error", "condition"),
            list(message = "Stopped by the user", call = NULL))
}

# Handle the page messages waiting in httpuv (they come back through
# .rgent_dispatch_message, which only records a stop while this runs)
.rgent_poll_messages <- function() {
  if (!isTRUE(.rgent_stream_state$active) || !requireNamespace("later", quietly = TRUE)) return(invisible(FALSE))
  tryCatch(later::run_now(0), error = function(e) NULL)
  invisible(.rgent_stop_requested())
}

# Every message from the chat page comes through here (start_websocket_server)
.rgent_dispatch_message <- function(handler, ws, isBinary, data) {
  st <- .rgent_stream_state
  request <- tryCatch(jsonlite::fromJSON(data, simplifyVector = FALSE), error = function(e) NULL)
  action <- if (is.list(request) && is.character(request$action)) request$action[[1]] else ""
  if (isTRUE(st$active)) {
    if (identical(action, "stop_response")) st$stop <- TRUE
    # the Data Explorer asks every 2.5 s whether the data changed: one is enough
    else if (!(identical(action, "explorer_check_changed") &&
               any(vapply(st$queue, function(m) identical(m$data, data), logical(1)))))
      st$queue[[length(st$queue) + 1L]] <- list(ws = ws, isBinary = isBinary, data = data)
    return(invisible())
  }
  if (action %in% .rgent_stoppable_actions) {
    st$active <- TRUE
    st$stop <- FALSE
    on.exit({ st$active <- FALSE; st$stop <- FALSE }, add = TRUE)
    # Everything sent for this reply carries the page's reply_id, so the page
    # can drop what was still on its way from a reply it stopped
    rid <- suppressWarnings(as.integer(request$reply_id %||% NA))
    if (length(rid) == 1 && !is.na(rid)) ws <- .rgent_reply_ws(ws, rid)
  }
  # Esc in the Console interrupts R itself (the one way to stop a statement
  # that's already running). R then abandoned this message, and the chat
  # waited forever on "Running your code" or a reply that never ended.
  tryCatch(.rgent_track_errors(handler, ws, isBinary, data), interrupt = function(i) {
    msg <- if (identical(action, "execute_code")) {
      list(action = "executed", status = "error", interrupted = TRUE,
           error = "Stopped: the code was interrupted (Esc in the Console).")
    } else if (identical(action, "chat_with_ai")) list(action = "streaming_stopped", interrupted = TRUE)
    if (!is.null(msg)) try(ws$send(jsonlite::toJSON(msg, auto_unbox = TRUE)), silent = TRUE)
  })
  if (action %in% .rgent_stoppable_actions) {
    st$active <- FALSE
    st$stop <- FALSE
    # The messages that came in meanwhile, in order
    while (length(st$queue)) {
      m <- st$queue[[1]]
      st$queue[[1]] <- NULL
      tryCatch(.rgent_dispatch_message(handler, m$ws, m$isBinary, m$data), error = function(e) NULL)
    }
  }
  invisible()
}

# A stand-in for the websocket whose send() adds "reply_id" to each JSON object
.rgent_reply_ws <- function(ws, rid) {
  p <- new.env(parent = emptyenv())
  p$send <- function(msg) {
    txt <- paste(as.character(msg), collapse = "")
    if (startsWith(txt, "{")) txt <- paste0('{"reply_id":', rid, if (identical(txt, "{}")) "" else ",", substring(txt, 2L))
    ws$send(txt)
  }
  p$close <- function(...) ws$close(...)
  p$request <- tryCatch(ws$request, error = function(e) NULL)
  p
}

# What's kept of a stopped reply in the history, so a follow-up ("go on")
# knows where it ended
.rgent_stopped_reply <- function(text) {
  text <- sub("\\s+$", "", text)
  if (!nzchar(text)) return("")
  paste0(text, "\n\n[The user stopped this reply here.]")
}

# How long one streamed reply may take, start to end (curl's total time). It
# was 60 s, which cut off long Sonnet replies; a reply is at most 2,600 tokens.
.rgent_stream_timeout <- 300

# A reply that failed part way (the connection dropped, the time limit): keep
# what arrived, close it on the page, and say so plainly. Nothing is resent
# (that would bill and count the question again and show the text twice).
.rgent_failed_reply <- function(ws, full_response, is_fix_request, stream_result) {
  timed_out <- grepl("timed? ?out|Timeout", stream_result$error %||% "", ignore.case = TRUE)
  status <- stream_result$status_code
  if (nzchar(trimws(full_response))) {
    if (!is_fix_request) add_to_conversation_history("ai", paste0(sub("\\s+$", "", .rgent_strip_read(full_response)),
                                                               "\n\n[This reply was cut off here.]"))
    ws$send(jsonlite::toJSON(list(action = "ai_response", streaming = TRUE, finished = TRUE, incomplete = TRUE), auto_unbox = TRUE))
    return(list(action = "ai_response", message = if (timed_out)
      "The reply was cut off: it took longer than 5 minutes. Ask again, or ask for a shorter answer."
      else "The reply was cut off: the connection to Rgent's server dropped. Ask again to get the rest."))
  }
  list(action = "ai_response", message = if (!is.null(status) && status %in% c(401L, 403L))
    "Rgent's server refused this request: your access code isn't active or your free trial has run out. See the Usage tab."
    else if (!is.null(status) && status == 429L) "You've reached your plan's request limit. See the Usage tab."
    else if (!is.null(status) && length(status) && status >= 500) paste0("Rgent's server had a problem (", status, "). Try again in a moment.")
    else "Couldn't reach Rgent's server. Check your internet connection and ask again; nothing in your session changed.")
}
