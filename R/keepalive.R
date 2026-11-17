# Keep-alive pings (2.4.20). Claude's prompt cache keeps an entry 5 minutes after
# it was last used; the next question after a longer pause pays 1.25x to write the
# whole prompt again (the file, the workspace, the history). 27% of real questions
# came 5+ minutes after the previous one (2026-11). So while the pane sits idle
# after a question, this sends that question's request body again to
# /chat/keepalive about every 3 minutes; the backend rebuilds the same prompt and
# sends it to Claude with max_tokens 0: a cache read (0.05x on Sonnet), no text.
#
# The backend decides (main.py KEEPALIVE_*, keepalive_decision()): only within 25
# minutes of a counted question, at most 6 pings per question, never when the
# last request is old enough that the entry may have expired (a ping would then
# rewrite it all), never again after a ping that missed. This side only follows
# its answers, and stops on its own after 6 pings or 25 minutes, on an error, on
# a 404 (an older backend), on New chat, Log out, a new access code and when the
# server stops. The body (it holds the access code, as every request does) is
# kept only in this environment, never on disk or in the global environment.
# Pings run in the background (curl's multi interface, polled with later), so
# they never hold up the Console. options(rgent.keepalive = FALSE) turns them off.

.rgent_keepalive <- new.env(parent = emptyenv())
.rgent_keepalive$gen <- 0L
.rgent_keepalive_max <- 6L
.rgent_keepalive_window <- 25 * 60

.rgent_keepalive_interval <- function() {
  i <- suppressWarnings(as.numeric(getOption("rgent.keepalive_interval", 170)))
  if (length(i) != 1 || !is.finite(i) || i <= 0) 170 else i
}

.rgent_keepalive_on <- function() {
  !isFALSE(getOption("rgent.keepalive", TRUE)) && requireNamespace("curl", quietly = TRUE)
}

# Forget the saved question: no more pings until the next one
.rgent_keepalive_clear <- function() {
  .rgent_keepalive$body <- NULL
  .rgent_keepalive$gen <- .rgent_keepalive$gen + 1L
  invisible()
}

# Called after every request to /chat/stream (stream_httr_post): any request
# restarts the timer (the cache was just used); a plain question (not a tool
# round, lookup, automatic fix or the message after a run, which all set
# `continuation` or `tool_rounds`) becomes the body to ping with.
.rgent_keepalive_after <- function(url, body, result) {
  ka <- .rgent_keepalive
  ka$gen <- ka$gen + 1L
  if (!is.character(url) || !grepl("/chat/stream$", url)) return(invisible())
  if (is.list(body) && !isTRUE(body$continuation) && is.null(body$tool_rounds)) {
    if (isTRUE(result$success) || isTRUE(result$stopped)) {
      # the same encoding httr used for the question
      ka$body <- tryCatch(as.character(jsonlite::toJSON(body, auto_unbox = TRUE)), error = function(e) NULL)
      ka$url <- sub("/chat/stream$", "/chat/keepalive", url)
      ka$since <- Sys.time()
      ka$sent <- 0L
    } else {
      ka$body <- NULL
    }
  }
  if (!is.null(ka$body) && .rgent_keepalive_on()) .rgent_keepalive_schedule(.rgent_keepalive_interval())
  invisible()
}

.rgent_keepalive_schedule <- function(delay) {
  gen <- .rgent_keepalive$gen
  # a ping must never put an error into the user's session
  later::later(function() tryCatch(.rgent_keepalive_ping(gen), error = function(e) .rgent_keepalive$body <- NULL), delay)
  invisible()
}

.rgent_keepalive_ping <- function(gen) {
  ka <- .rgent_keepalive
  # a newer request (or New chat, Log out) replaced this timer
  if (!identical(gen, ka$gen) || is.null(ka$body) || !.rgent_keepalive_on()) return(invisible())
  # a reply is streaming or code is running: that request restarts the timer itself
  if (isTRUE(.rgent_stream_state$active)) return(invisible())
  if (ka$sent >= .rgent_keepalive_max ||
      as.numeric(difftime(Sys.time(), ka$since, units = "secs")) > .rgent_keepalive_window) {
    ka$body <- NULL
    return(invisible())
  }
  ka$sent <- ka$sent + 1L
  ok <- tryCatch({
    h <- curl::new_handle()
    # no redirects: the body holds the access code, and goes only to the URL the
    # question went to (rgent.onrender.com, or localhost via options(rgent.backend));
    # curl checks the TLS certificate by default
    curl::handle_setopt(h, post = TRUE, postfields = ka$body, timeout = 30L, followlocation = FALSE)
    curl::handle_setheaders(h, "Content-Type" = "application/json")
    pool <- curl::new_pool()
    curl::curl_fetch_multi(ka$url, handle = h, pool = pool,
                           done = function(res) tryCatch(.rgent_keepalive_done(gen, res), error = function(e) ka$body <- NULL),
                           fail = function(msg) if (identical(gen, ka$gen)) ka$body <- NULL)
    .rgent_keepalive_pump(pool)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) ka$body <- NULL
  invisible()
}

# Lets curl make progress without waiting, then checks again shortly
.rgent_keepalive_pump <- function(pool) {
  st <- tryCatch(curl::multi_run(timeout = 0, poll = TRUE, pool = pool), error = function(e) list(pending = 0))
  if (isTRUE((st$pending %||% 0) > 0)) later::later(function() tryCatch(.rgent_keepalive_pump(pool), error = function(e) NULL), 0.1)
  invisible()
}

# The backend's answer: {"stop": true, ...} (refused), or one SSE event
# data: {"keepalive": {"stop": false, "next_in": 170, ...}}
.rgent_keepalive_answer <- function(text) {
  text <- trimws(text %||% "")
  if (!nzchar(text)) return(NULL)
  if (startsWith(text, "{")) return(tryCatch(jsonlite::fromJSON(text), error = function(e) NULL))
  line <- grep("^data: ", strsplit(text, "\n", fixed = TRUE)[[1]], value = TRUE)
  if (!length(line)) return(NULL)
  tryCatch(jsonlite::fromJSON(substring(line[length(line)], 7))$keepalive, error = function(e) NULL)
}

.rgent_keepalive_done <- function(gen, res) {
  ka <- .rgent_keepalive
  if (!identical(gen, ka$gen)) return(invisible())
  ans <- if (identical(as.integer(res$status_code), 200L)) .rgent_keepalive_answer(rawToChar(res$content)) else NULL
  if (is.null(ans) || !isFALSE(ans$stop)) {
    ka$body <- NULL   # refused, missed, an error, or an older backend (404)
    return(invisible())
  }
  nxt <- suppressWarnings(as.numeric(ans$next_in))
  if (length(nxt) != 1 || !is.finite(nxt)) nxt <- .rgent_keepalive_interval()
  # never sooner than a minute (tests shorten the interval), never past the
  # backend's age limit
  .rgent_keepalive_schedule(min(max(nxt, min(60, .rgent_keepalive_interval())), 200))
  invisible()
}
