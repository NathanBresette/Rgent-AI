test_that("the socket token is random, stable per session, and leaves the user's seed alone", {
  old_env <- Sys.getenv("RGENT_WS_TOKEN"); Sys.setenv(RGENT_WS_TOKEN = "")
  on.exit(Sys.setenv(RGENT_WS_TOKEN = old_env), add = TRUE)
  env <- rstudioai:::.rgent_loaded
  old <- env$ws_token
  on.exit(env$ws_token <- old, add = TRUE)
  env$ws_token <- NULL
  set.seed(42); before <- runif(1); set.seed(42)
  tok <- rstudioai:::.rgent_ws_token()
  expect_match(tok, "^[0-9a-f]{48}$")
  expect_identical(rstudioai:::.rgent_ws_token(), tok)
  expect_identical(runif(1), before)
  expect_false(identical(rstudioai:::.rgent_random_hex(), rstudioai:::.rgent_random_hex()))
})

test_that("the page gets the token in place of the placeholder", {
  old_env <- Sys.getenv("RGENT_WS_TOKEN"); Sys.setenv(RGENT_WS_TOKEN = "tok123")
  on.exit(Sys.setenv(RGENT_WS_TOKEN = old_env), add = TRUE)
  page <- charToRaw("<script>const RGENT_WS_TOKEN = '__RGENT_WS_TOKEN__'; // é</script>")
  out <- rawToChar(rstudioai:::.rgent_page_with_token(page))
  expect_match(out, "'tok123'", fixed = TRUE)
  expect_false(grepl("__RGENT_WS_TOKEN__", out, fixed = TRUE))
})

test_that("connections need the token and a local origin", {
  old_env <- Sys.getenv("RGENT_WS_TOKEN"); Sys.setenv(RGENT_WS_TOKEN = "secret-token")
  on.exit(Sys.setenv(RGENT_WS_TOKEN = old_env), add = TRUE)
  req <- function(q, origin = NULL) list(QUERY_STRING = q, HTTP_ORIGIN = origin)
  ok <- rstudioai:::.rgent_ws_allowed
  expect_true(ok(req("?t=secret-token")))
  expect_true(ok(req("t=secret-token", "http://127.0.0.1:43123")))
  expect_true(ok(req("?t=secret-token", "http://localhost:5000")))
  expect_true(ok(req("?t=secret-token", "null")))           # page loaded from a file
  expect_true(ok(req("?t=secret-token", "file://")))
  expect_true(ok(req("?x=1&t=secret-token", "http://[::1]:8080")))
  expect_false(ok(req("")))                                  # no token
  expect_false(ok(req(NULL)))
  expect_false(ok(req("?t=")))
  expect_false(ok(req("?t=secret-tokem")))
  expect_false(ok(req("?t=secret-token-and-more")))
  expect_false(ok(req("?t=secret-token", "https://evil.example")))
  expect_false(ok(req("?t=secret-token", "http://127.0.0.1.evil.example")))
  expect_false(ok(req("?t=secret-token", "http://localhost.evil.example:8888")))
})
