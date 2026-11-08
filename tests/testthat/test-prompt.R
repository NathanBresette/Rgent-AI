test_that("the access code and other package globals never reach the prompt", {
  old <- if (exists("current_access_code", globalenv())) get("current_access_code", globalenv())
  assign("current_access_code", "SECRET-TEST-CODE", envir = globalenv())
  assign("user_df", data.frame(a = 1), envir = globalenv())
  on.exit({ rm("user_df", envir = globalenv())
            if (is.null(old)) rm("current_access_code", envir = globalenv()) else assign("current_access_code", old, envir = globalenv()) })
  expect_false("current_access_code" %in% safe_ls(globalenv()))
  expect_true("user_df" %in% safe_ls(globalenv()))
  ctx <- capture_context_smart("x")
  expect_false(grepl("SECRET-TEST-CODE", jsonlite::toJSON(ctx, auto_unbox = TRUE, force = TRUE)))
})

test_that("older replies are compacted in the history; the latest stays whole", {
  old <- rstudioai:::.rgent_state$conversation_history
  on.exit(assign("conversation_history", if (is.null(old)) list() else old, envir = rstudioai:::.rgent_state))
  code <- paste(c("Here:", "```r", sprintf("x%d <- %d", 1:40, 1:40), "```"), collapse = "\n")
  assign("conversation_history", list(list(role = "user", message = "first"), list(role = "ai", message = code),
                                      list(role = "user", message = "second"), list(role = "ai", message = code)), envir = rstudioai:::.rgent_state)
  h <- get_conversation_history()
  expect_match(h, "[r block, 40 lines, starting `x1 <- 1`]", fixed = TRUE)
  expect_equal(lengths(regmatches(h, gregexpr("x40 <- 40", h))), 1L)   # only the latest reply keeps its code
})

test_that("history keeps six exchanges, compacts older replies and stays under the size cap", {
  old <- rstudioai:::.rgent_state$conversation_history
  on.exit(assign("conversation_history", if (is.null(old)) list() else old, envir = rstudioai:::.rgent_state))
  assign("conversation_history", list(), envir = rstudioai:::.rgent_state)
  for (i in 1:8) {
    add_to_conversation_history("user", sprintf("question %d", i))
    add_to_conversation_history("ai", paste0(sprintf("answer %d ", i), strrep("word ", 300)))
  }
  hist <- rstudioai:::.rgent_state$conversation_history
  expect_length(hist, 12)
  expect_equal(hist[[1]]$message, "question 3")
  h <- get_conversation_history()
  expect_match(h, "question 3", fixed = TRUE)            # decisions from 6 exchanges ago are still there
  expect_match(h, paste0("answer 8 ", strrep("word ", 300)), fixed = TRUE)   # latest reply whole
  expect_lte(nchar(h), 9000 + 100)
  # Very long messages: the oldest are dropped, the latest exchange is kept
  assign("conversation_history", list(), envir = rstudioai:::.rgent_state)
  for (i in 1:6) { add_to_conversation_history("user", strrep("q", 3000)); add_to_conversation_history("ai", strrep("a", 5000)) }
  add_to_conversation_history("user", "last question")
  h <- get_conversation_history()
  expect_match(h, "last question", fixed = TRUE)
  expect_lte(nchar(h), 9000 + 100)
})

test_that("tests write chats to a temp folder, not the real History", {
  expect_true(startsWith(normalizePath(tools::R_user_dir("rstudioai", "data"), mustWork = FALSE),
                         normalizePath(tempdir())))
})
