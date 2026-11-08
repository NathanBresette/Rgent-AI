# Who may connect to the chat page's websocket.
#
# The server listens on 127.0.0.1, which every web page open in the user's
# browser can also reach: a page can open ws://127.0.0.1:8888 like any site.
# Without a check, any website could run R code in the session (execute_code),
# edit the open file, and receive the saved access code (sent on connect).
# So a connection must carry this session's random token, which R writes into
# the copy of the chat page it opens (`__RGENT_WS_TOKEN__`), and when the
# browser says which page it comes from (Origin), that page must be on this
# computer. RStudio's Viewer serves the page from localhost / 127.0.0.1; test
# pages loaded from a file send "null".

# Random hex from the OS; R's RNG (reseeded from the clock and process id,
# leaving the user's seed alone) where /dev/urandom doesn't exist (Windows).
.rgent_random_hex <- function(n = 24L) {
  read_urandom <- function() {
    if (!file.exists("/dev/urandom")) return(NULL)
    con <- file("/dev/urandom", "rb", raw = TRUE)
    on.exit(close(con))
    readBin(con, "raw", n)
  }
  b <- tryCatch(read_urandom(), error = function(e) NULL)
  if (length(b) != n) {
    genv <- globalenv()
    had <- exists(".Random.seed", envir = genv, inherits = FALSE)
    old <- if (had) get(".Random.seed", envir = genv) else NULL
    on.exit(if (had) assign(".Random.seed", old, envir = genv)
            else if (exists(".Random.seed", envir = genv, inherits = FALSE)) rm(".Random.seed", envir = genv))
    set.seed(NULL)
    b <- as.raw(sample.int(256L, n, replace = TRUE) - 1L)
  }
  paste(sprintf("%02x", as.integer(b)), collapse = "")
}

# One token per R session. RGENT_WS_TOKEN overrides it (test harnesses).
.rgent_ws_token <- function() {
  env <- Sys.getenv("RGENT_WS_TOKEN")
  if (nzchar(env)) return(env)
  if (is.null(.rgent_loaded$ws_token)) .rgent_loaded$ws_token <- .rgent_random_hex()
  .rgent_loaded$ws_token
}

# The chat page with this session's token in place of the placeholder
.rgent_page_with_token <- function(page) {
  txt <- if (is.raw(page)) rawToChar(page) else paste(page, collapse = "\n")
  charToRaw(gsub("__RGENT_WS_TOKEN__", .rgent_ws_token(), txt, fixed = TRUE, useBytes = TRUE))
}

# TRUE when the browser's Origin (if any) is a page on this computer
.rgent_origin_local <- function(origin) {
  if (is.null(origin) || !length(origin)) return(TRUE)
  origin <- as.character(origin)[[1]]
  # "null" / file://: a page opened from a file (no website can send these)
  if (!nzchar(origin) || identical(origin, "null") || startsWith(tolower(origin), "file://")) return(TRUE)
  host <- tolower(sub("^[a-z][a-z0-9+.-]*://", "", origin))
  host <- sub("/.*$", "", host)
  host <- if (startsWith(host, "[")) sub("^(\\[[^]]*\\]).*$", "\\1", host) else sub(":[0-9]*$", "", host)
  host %in% c("localhost", "127.0.0.1", "[::1]") || grepl("\\.localhost$", host)
}

# The token from a websocket request's query string ("?t=...")
.rgent_ws_request_token <- function(req) {
  q <- tryCatch(as.character(req$QUERY_STRING), error = function(e) character(0))
  if (!length(q) || is.na(q[[1]])) return("")
  parts <- strsplit(sub("^\\?", "", q[[1]]), "&", fixed = TRUE)[[1]]
  t <- parts[startsWith(parts, "t=")]
  if (!length(t)) return("")
  tryCatch(utils::URLdecode(substring(t[[1]], 3L)), error = function(e) "")
}

.rgent_ws_allowed <- function(req) {
  tok <- .rgent_ws_request_token(req)
  expected <- .rgent_ws_token()
  nzchar(tok) && nchar(tok) == nchar(expected) && identical(tok, expected) &&
    .rgent_origin_local(tryCatch(req$HTTP_ORIGIN, error = function(e) NULL))
}

# Close connections that fail the check before anything is sent to them
.rgent_ws_reject <- function(ws) {
  if (.rgent_ws_allowed(ws$request)) return(FALSE)
  message("Rgent: refused a connection to its local server from ",
          tryCatch(ws$request$HTTP_ORIGIN %||% "an unknown page", error = function(e) "an unknown page"),
          " (it did not come from the Rgent pane).")
  try(ws$close(), silent = TRUE)
  TRUE
}
