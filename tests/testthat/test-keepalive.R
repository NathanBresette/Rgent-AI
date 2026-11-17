# Keep-alive pings (keepalive.R): sent in the background after a question, with
# that question's exact body, following the backend's answers, within hard limits.

ka_server <- function() {
  srv <- new.env()
  srv$bodies <- character(0); srv$mode <- "ok"; srv$paths <- character(0)
  app <- list(call = function(req) {
    body <- rawToChar(req$rook.input$read())
    srv$paths <- c(srv$paths, req$PATH_INFO)
    srv$bodies <- c(srv$bodies, body)
    switch(srv$mode,
      ok = list(status = 200L, headers = list("Content-Type" = "text/event-stream"),
                body = 'data: {"keepalive": {"stop": false, "next_in": 0.2, "read": 9000, "wrote": 0}}\n\n'),
      stop = list(status = 200L, headers = list("Content-Type" = "application/json"), body = '{"stop": true, "reason": "too old"}'),
      missed = list(status = 200L, headers = list("Content-Type" = "text/event-stream"),
                    body = 'data: {"keepalive": {"stop": true, "next_in": 170, "read": 0, "wrote": 9000}}\n\n'),
      notfound = list(status = 404L, headers = list("Content-Type" = "application/json"), body = '{"detail": "Not Found"}'),
      garbage = list(status = 200L, headers = list("Content-Type" = "text/plain"), body = "<html>oops</html>"))
  })
  port <- httpuv::randomPort()
  srv$handle <- httpuv::startServer("127.0.0.1", port, app)
  srv$url <- paste0("http://127.0.0.1:", port, "/chat/stream")
  srv
}

pump <- function(seconds) {
  end <- Sys.time() + seconds
  while (Sys.time() < end) later::run_now(0.05)
}

# Waits for a condition instead of a fixed time: check machines (r-universe's
# Windows, macOS, Linux) are much slower and more uneven than a laptop
pump_until <- function(cond, max = 15) {
  end <- Sys.time() + max
  while (!isTRUE(cond()) && Sys.time() < end) later::run_now(0.05)
  isTRUE(cond())
}

question <- list(access_code = "CODE", prompt = "USER REQUEST: plot it", tools = 1,
                 history = list("USER: hi", "AI: hello"),
                 context_data = list(file_info = list(file_path = "a.R", file_contents = list("x <- 1", "y <- 2"))))

setup_ka <- function() {
  ns <- asNamespace("rstudioai")
  ns$.rgent_keepalive_clear()
  options(rgent.keepalive_interval = 0.2, rgent.keepalive = TRUE)
}

test_that("after a question, pings carry its exact body to /chat/keepalive and follow the answers", {
  skip_if_not_installed("curl")
  ns <- asNamespace("rstudioai"); setup_ka(); srv <- ka_server(); on.exit(httpuv::stopServer(srv$handle))
  ns$.rgent_keepalive_after(srv$url, question, list(success = TRUE))
  expect_true(pump_until(function() length(srv$bodies) >= 2))
  srv$mode <- "stop"; n <- length(srv$bodies)     # well under the cap of 6
  expect_true(all(srv$paths == "/chat/keepalive"))
  expect_identical(srv$bodies[1], as.character(jsonlite::toJSON(question, auto_unbox = TRUE)))
  expect_true(pump_until(function() is.null(ns$.rgent_keepalive$body)))
  pump(0.6)
  expect_equal(length(srv$bodies), n + 1)          # the refused one, then nothing
  expect_null(ns$.rgent_keepalive$body)
})

test_that("never more than 6 pings per question, whatever the backend says", {
  skip_if_not_installed("curl")
  ns <- asNamespace("rstudioai"); setup_ka(); srv <- ka_server(); on.exit(httpuv::stopServer(srv$handle))
  ns$.rgent_keepalive_after(srv$url, question, list(success = TRUE))
  expect_true(pump_until(function() is.null(ns$.rgent_keepalive$body), max = 30))
  pump(0.6)
  expect_equal(length(srv$bodies), 6)
})

test_that("a missed ping, a 404 (older backend) or a garbled answer stops them", {
  skip_if_not_installed("curl")
  ns <- asNamespace("rstudioai")
  for (m in c("missed", "notfound", "garbage")) {
    setup_ka(); srv <- ka_server()
    srv$mode <- m
    ns$.rgent_keepalive_after(srv$url, question, list(success = TRUE))
    pump_until(function() is.null(ns$.rgent_keepalive$body))
    pump(0.4)
    httpuv::stopServer(srv$handle)
    expect_equal(length(srv$bodies), 1, info = m)
    expect_null(ns$.rgent_keepalive$body)
  }
})

test_that("tool rounds, lookups, fixes and the message after a run restart the timer but aren't pinged", {
  skip_if_not_installed("curl")
  ns <- asNamespace("rstudioai"); setup_ka(); srv <- ka_server(); on.exit(httpuv::stopServer(srv$handle))
  ns$.rgent_keepalive_after(srv$url, question, list(success = TRUE))
  ns$.rgent_keepalive_after(srv$url, c(question, list(tool_rounds = list(list(assistant = "[]")))), list(success = TRUE))
  ns$.rgent_keepalive_after(srv$url, modifyList(question, list(continuation = TRUE, prompt = "fix")), list(success = TRUE))
  expect_true(pump_until(function() length(srv$bodies) >= 1))
  expect_true(all(srv$bodies == as.character(jsonlite::toJSON(question, auto_unbox = TRUE))))
})

test_that("a newer request replaces the waiting ping; New chat / Log out / the option stop them", {
  skip_if_not_installed("curl")
  # Calls the timer's function directly instead of racing real timers
  ns <- asNamespace("rstudioai"); setup_ka(); srv <- ka_server(); on.exit(httpuv::stopServer(srv$handle))
  options(rgent.keepalive_interval = 600)          # no real timer fires during the test
  ns$.rgent_keepalive_after(srv$url, question, list(success = TRUE))
  old <- ns$.rgent_keepalive$gen
  ns$.rgent_keepalive_after(srv$url, modifyList(question, list(continuation = TRUE)), list(success = TRUE))
  expect_false(is.null(ns$.rgent_keepalive$body)) # the question is still the one to ping with
  ns$.rgent_keepalive_ping(old)                    # the first timer fires: replaced, nothing sent
  pump(0.5)
  expect_equal(length(srv$bodies), 0)
  now <- ns$.rgent_keepalive$gen
  ns$.rgent_keepalive_clear()
  ns$.rgent_keepalive_ping(now); ns$.rgent_keepalive_ping(ns$.rgent_keepalive$gen)
  pump(0.5)
  expect_equal(length(srv$bodies), 0)              # New chat / Log out
  options(rgent.keepalive = FALSE)
  ns$.rgent_keepalive_after(srv$url, question, list(success = TRUE))
  ns$.rgent_keepalive_ping(ns$.rgent_keepalive$gen)
  pump(0.5)
  expect_equal(length(srv$bodies), 0)              # turned off
  options(rgent.keepalive = TRUE)
  ns$.rgent_keepalive_ping(ns$.rgent_keepalive$gen) # the check: the same call does send when on
  expect_true(pump_until(function() length(srv$bodies) == 1))
  ns$.rgent_keepalive_clear()
})

test_that("no ping while a reply streams or code runs, and none after a failed question", {
  skip_if_not_installed("curl")
  ns <- asNamespace("rstudioai"); setup_ka(); srv <- ka_server(); on.exit(httpuv::stopServer(srv$handle))
  st <- ns$.rgent_stream_state; old <- st$active
  st$active <- TRUE
  ns$.rgent_keepalive_after(srv$url, question, list(success = TRUE))
  pump(0.5)
  st$active <- old
  expect_equal(length(srv$bodies), 0)
  ns$.rgent_keepalive_after(srv$url, question, list(success = FALSE, error = "Timeout"))
  expect_null(ns$.rgent_keepalive$body)
  pump(0.4)
  expect_equal(length(srv$bodies), 0)
})

test_that("pings only go where the question went (never another host), and never block the Console", {
  skip_if_not_installed("curl")
  ns <- asNamespace("rstudioai"); setup_ka()
  ns$.rgent_keepalive_after("https://rgent.onrender.com/chat/stream", question, list(success = TRUE))
  expect_identical(ns$.rgent_keepalive$url, "https://rgent.onrender.com/chat/keepalive")
  ns$.rgent_keepalive_clear()
  # options(rgent.backend) only accepts localhost (websocket_addin.R)
  options(rgent.backend = "http://evil.example.com")
  expect_identical(ns$.rgent_backend_url("https://rgent.onrender.com/chat/stream"), "https://rgent.onrender.com/chat/stream")
  options(rgent.backend = NULL)
  # an unreachable backend: the ping is in the background, so this returns at once
  setup_ka()
  options(rgent.keepalive_interval = 600)
  ns$.rgent_keepalive_after("http://127.0.0.1:9/chat/stream", question, list(success = TRUE))
  t <- system.time(ns$.rgent_keepalive_ping(ns$.rgent_keepalive$gen))[["elapsed"]]
  expect_lt(t, 1)                                  # sending returns at once
  # (Windows takes ~2 s to give up on a refused connection)
  expect_true(pump_until(function() is.null(ns$.rgent_keepalive$body), max = 30))  # the failure stopped them
})
