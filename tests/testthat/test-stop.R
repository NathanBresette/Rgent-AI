# The Stop button (R/stop.R)

test_that("everything sent for a reply carries its reply_id", {
  sent <- character(0)
  ws <- list(send = function(x) sent <<- c(sent, x), close = function() NULL)
  p <- rstudioai:::.rgent_reply_ws(ws, 7L)
  p$send(jsonlite::toJSON(list(action = "ai_response", chunk = "hi"), auto_unbox = TRUE))
  p$send("{}")
  p$send("not json")
  expect_equal(jsonlite::fromJSON(sent[1])$reply_id, 7)
  expect_equal(jsonlite::fromJSON(sent[1])$chunk, "hi")
  expect_equal(sent[2], '{"reply_id":7}')
  expect_equal(sent[3], "not json")
})

test_that("while a reply streams, Stop is recorded and other messages wait their turn", {
  st <- rstudioai:::.rgent_stream_state
  handled <- character(0)
  inner <- NULL
  handler <- function(ws, isBinary, data) {
    a <- jsonlite::fromJSON(data)$action
    handled <<- c(handled, a)
    if (a == "chat_with_ai") {
      # messages that arrive in the middle of the stream
      rstudioai:::.rgent_dispatch_message(handler, ws, FALSE, '{"action":"chat_list"}')
      rstudioai:::.rgent_dispatch_message(handler, ws, FALSE, '{"action":"stop_response"}')
      inner <<- list(stop = rstudioai:::.rgent_stop_requested(), handled = handled)
    }
    NULL
  }
  ws <- list(send = function(x) NULL)
  rstudioai:::.rgent_dispatch_message(handler, ws, FALSE, '{"action":"chat_with_ai","reply_id":3}')
  expect_true(inner$stop)
  expect_equal(inner$handled, "chat_with_ai")            # chat_list didn't run inside it
  expect_equal(handled, c("chat_with_ai", "chat_list"))  # it ran right after
  expect_false(st$active)
  expect_false(rstudioai:::.rgent_stop_requested())      # a stale Stop doesn't carry over
  expect_length(st$queue, 0)
})

test_that("a stopped reply is kept in the history, marked", {
  expect_equal(rstudioai:::.rgent_stopped_reply("Part 1. Part 2.  \n"), "Part 1. Part 2.\n\n[The user stopped this reply here.]")
  expect_equal(rstudioai:::.rgent_stopped_reply("  "), "")
})

test_that("code stops between statements when Stop was pressed", {
  st <- rstudioai:::.rgent_stream_state
  st$active <- TRUE; st$stop <- TRUE; st$run_stopped <- NULL
  on.exit({ st$active <- FALSE; st$stop <- FALSE; st$run_stopped <- NULL })
  env_before <- exists("rgent_stop_t2", envir = globalenv())
  r <- rstudioai:::execute_code_in_session("rgent_stop_t1 <- 1\nrgent_stop_t2 <- 2",
                                           settings = list(print_to_console = FALSE, log_to_file = FALSE))
  expect_true(r$success)
  expect_equal(st$run_stopped, list(before = 2L, of = 2L))
  expect_true(exists("rgent_stop_t1", envir = globalenv()))
  expect_false(exists("rgent_stop_t2", envir = globalenv()))
  suppressWarnings(rm(list = c("rgent_stop_t1", "rgent_stop_t2"), envir = globalenv()))
})
