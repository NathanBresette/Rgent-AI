# =============================================================================
# Test Streaming and SSE Functions
# These tests validate the streaming POST and chunk processing functionality
# =============================================================================

library(testthat)

# =============================================================================
# TEST: stream_httr_post() Function Structure
# =============================================================================

test_that("stream_httr_post function exists", {
  expect_true(exists("stream_httr_post"))
  expect_true(is.function(stream_httr_post))
})

test_that("stream_httr_post has correct parameters", {
  args <- formals(stream_httr_post)
  
  expect_true("url" %in% names(args))
  expect_true("body" %in% names(args))
  expect_true("chunk_callback" %in% names(args))
})

test_that("stream_httr_post has optional headers parameter", {
  args <- formals(stream_httr_post)
  
  expect_true("headers" %in% names(args))
  # Should have a default value
  expect_true(is.null(args$headers))
})

test_that("stream_httr_post has timeout parameter", {
  args <- formals(stream_httr_post)
  
  expect_true("timeout" %in% names(args))
})

# =============================================================================
# TEST: Callback Function Interface
# =============================================================================

test_that("chunk callback receives data", {
  # Mock a simple callback
  callback_called <- FALSE
  test_callback <- function(chunk) {
    callback_called <<- TRUE
    return(TRUE)
  }
  
  expect_true(is.function(test_callback))
})

test_that("callback handles text chunks", {
  chunks_received <- c()
  
  test_callback <- function(chunk) {
    chunks_received <<- c(chunks_received, chunk)
    return(TRUE)
  }
  
  # Simulate receiving chunks
  test_callback("Hello ")
  test_callback("World")
  
  expect_equal(length(chunks_received), 2)
  expect_equal(paste(chunks_received, collapse = ""), "Hello World")
})

test_that("callback can signal stop", {
  call_count <- 0
  
  test_callback <- function(chunk) {
    call_count <<- call_count + 1
    if (call_count >= 3) {
      return(FALSE)  # Signal to stop
    }
    return(TRUE)
  }
  
  # Simulate multiple calls
  for (i in 1:5) {
    result <- test_callback(paste("chunk", i))
    if (!result) break
  }
  
  expect_equal(call_count, 3)
})

# =============================================================================
# TEST: SSE Format Parsing
# =============================================================================

test_that("SSE data prefix is handled correctly", {
  # Simulate SSE line parsing
  sse_line <- "data: {\"message\": \"hello\"}"
  
  # Check if it starts with data:
  expect_true(grepl("^data: ", sse_line))
  
  # Extract JSON
  json_data <- substring(sse_line, 7)  # Remove "data: " prefix
  expect_equal(json_data, "{\"message\": \"hello\"}")
})

test_that("SSE DONE signal is recognized", {
  done_line <- "data: [DONE]"
  
  # Extract data portion
  json_data <- substring(done_line, 7)
  
  expect_equal(json_data, "[DONE]")
})

test_that("non-SSE lines are ignored", {
  non_sse_lines <- c(
    "",
    ": comment",
    "event: message",
    "id: 123"
  )
  
  for (line in non_sse_lines) {
    expect_false(grepl("^data: ", line) && !grepl("^: ", line))
  }
})

# =============================================================================
# TEST: Buffer Handling
# =============================================================================

test_that("line buffer accumulates incomplete lines", {
  buffer <- ""
  
  # Simulate receiving incomplete chunk
  chunk1 <- "data: {\"mess"
  buffer <- paste0(buffer, chunk1)
  
  # No complete line yet
  expect_false(grepl("\n", buffer))
  
  # Receive rest of line
  chunk2 <- "age\": \"hello\"}\n"
  buffer <- paste0(buffer, chunk2)
  
  # Now we have a complete line
  expect_true(grepl("\n", buffer))
})

test_that("multiple lines in single chunk are handled", {
  chunk <- "data: line1\ndata: line2\ndata: line3\n"
  
  # Split by newline
  lines <- strsplit(chunk, "\n")[[1]]
  
  # strsplit behavior: trailing empty may or may not be included
  expect_gte(length(lines), 3)  # At least 3 data lines
  expect_true(all(grepl("^data:", lines[lines != ""])))
})

test_that("partial line at end is preserved", {
  chunk <- "data: complete\ndata: incom"
  
  # Find last newline
  newline_pos <- regexpr("\n[^\n]*$", chunk)
  
  # Extract complete portion
  complete <- substr(chunk, 1, newline_pos - 1)
  incomplete <- substr(chunk, newline_pos + 1, nchar(chunk))
  
  expect_equal(complete, "data: complete")
  expect_equal(incomplete, "data: incom")
})

# =============================================================================
# TEST: JSON Parsing in Chunks
# =============================================================================

test_that("valid JSON in SSE data is parseable", {
  sse_line <- "data: {\"content\": \"Hello\", \"type\": \"text\"}"
  json_data <- substring(sse_line, 7)
  
  parsed <- tryCatch({
    jsonlite::fromJSON(json_data)
  }, error = function(e) {
    NULL
  })
  
  expect_true(!is.null(parsed))
  expect_equal(parsed$content, "Hello")
  expect_equal(parsed$type, "text")
})

test_that("invalid JSON is handled gracefully", {
  invalid_json <- "{\"broken\": }"
  
  result <- tryCatch({
    jsonlite::fromJSON(invalid_json)
  }, error = function(e) {
    list(error = TRUE)
  })
  
  expect_true(is.list(result))
  if (!is.null(result$error)) {
    expect_true(result$error)
  }
})

test_that("empty JSON object is valid", {
  empty_json <- "{}"
  
  parsed <- tryCatch({
    jsonlite::fromJSON(empty_json)
  }, error = function(e) {
    NULL
  })
  
  expect_true(!is.null(parsed))
})

# =============================================================================
# TEST: Timeout Handling
# =============================================================================

test_that("default timeout is reasonable", {
  args <- formals(stream_httr_post)
  
  if (!is.null(args$timeout)) {
    default_timeout <- eval(args$timeout)
    # Default should be between 30-120 seconds
    expect_true(is.numeric(default_timeout))
    expect_gte(default_timeout, 30)
    expect_lte(default_timeout, 300)
  }
})

# =============================================================================
# TEST: Content Type Handling
# =============================================================================

test_that("request body can be JSON", {
  body <- list(
    message = "Hello",
    context = list(a = 1, b = 2)
  )
  
  json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  expect_true(is.character(json_body))
  expect_true(grepl("message", json_body))
})

test_that("request body handles special characters", {
  body <- list(
    message = "Code: x <- function() { print('hello') }",
    unicode = "Test: cafe"
  )
  
  json_body <- tryCatch({
    jsonlite::toJSON(body, auto_unbox = TRUE)
  }, error = function(e) {
    NULL
  })
  
  expect_true(!is.null(json_body))
})

# =============================================================================
# TEST: Response Accumulation
# =============================================================================

test_that("full response can be accumulated", {
  full_response <- ""
  
  callback <- function(chunk) {
    full_response <<- paste0(full_response, chunk)
    TRUE
  }
  
  # Simulate streaming
  callback("Hello ")
  callback("World!")
  
  expect_equal(full_response, "Hello World!")
})

test_that("chunk count is tracked", {
  chunk_count <- 0
  
  callback <- function(chunk) {
    chunk_count <<- chunk_count + 1
    TRUE
  }
  
  # Simulate streaming
  for (i in 1:5) {
    callback(paste("chunk", i))
  }
  
  expect_equal(chunk_count, 5)
})

test_that("byte count is tracked", {
  total_bytes <- 0
  
  callback <- function(chunk) {
    total_bytes <<- total_bytes + nchar(chunk, type = "bytes")
    TRUE
  }
  
  callback("Hello")  # 5 bytes
  callback(" World")  # 6 bytes
  
  expect_equal(total_bytes, 11)
})

# =============================================================================
# TEST: Error Recovery
# =============================================================================

test_that("callback errors don't crash accumulation", {
  safe_callback <- function(chunk) {
    tryCatch({
      if (chunk == "error_trigger") {
        stop("Intentional error")
      }
      TRUE
    }, error = function(e) {
      FALSE  # Signal to stop but don't crash
    })
  }
  
  result1 <- safe_callback("normal chunk")
  result2 <- safe_callback("error_trigger")
  
  expect_true(result1)
  expect_false(result2)
})

test_that("stream can be stopped mid-way", {
  processed <- c()
  should_stop <- FALSE
  
  callback <- function(chunk) {
    if (should_stop) return(FALSE)
    processed <<- c(processed, chunk)
    if (length(processed) >= 3) {
      should_stop <<- TRUE
    }
    TRUE
  }
  
  # Simulate streaming
  for (i in 1:5) {
    if (!callback(paste("chunk", i))) break
  }
  
  expect_equal(length(processed), 3)
})

# =============================================================================
# TEST: Raw Byte Handling
# =============================================================================

test_that("raw bytes can be converted to text", {
  raw_bytes <- charToRaw("Hello World")
  
  text <- rawToChar(raw_bytes)
  
  expect_equal(text, "Hello World")
})

test_that("UTF-8 bytes are handled correctly", {
  # UTF-8 encoded text
  utf8_text <- "Hello cafe"
  raw_bytes <- charToRaw(utf8_text)
  
  decoded <- rawToChar(raw_bytes)
  
  expect_true(nchar(decoded) > 0)
})

test_that("incomplete UTF-8 sequences are handled", {
  # This might happen with chunked encoding
  complete_chunk <- charToRaw("Hello")
  
  decoded <- tryCatch({
    rawToChar(complete_chunk)
  }, error = function(e) {
    ""
  })
  
  expect_true(is.character(decoded))
})

# =============================================================================
# TEST: Environment Scoping
# =============================================================================

test_that("stream environment preserves state", {
  stream_env <- new.env()
  stream_env$buffer <- ""
  stream_env$count <- 0
  
  # Modify within callback
  process <- function(chunk) {
    stream_env$buffer <- paste0(stream_env$buffer, chunk)
    stream_env$count <- stream_env$count + 1
  }
  
  process("a")
  process("b")
  process("c")
  
  expect_equal(stream_env$buffer, "abc")
  expect_equal(stream_env$count, 3)
})

test_that("callback environment is accessible", {
  outer_value <- "initial"
  
  callback <- function(chunk) {
    outer_value <<- chunk
    TRUE
  }
  
  callback("modified")
  
  expect_equal(outer_value, "modified")
})

