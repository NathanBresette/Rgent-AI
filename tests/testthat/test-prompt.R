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
  old <- if (exists("conversation_history", globalenv())) get("conversation_history", globalenv())
  on.exit(assign("conversation_history", if (is.null(old)) list() else old, envir = globalenv()))
  code <- paste(c("Here:", "```r", sprintf("x%d <- %d", 1:40, 1:40), "```"), collapse = "\n")
  assign("conversation_history", list(list(role = "user", message = "first"), list(role = "ai", message = code),
                                      list(role = "user", message = "second"), list(role = "ai", message = code)), envir = globalenv())
  h <- get_conversation_history()
  expect_match(h, "[r block, 40 lines, starting `x1 <- 1`]", fixed = TRUE)
  expect_equal(lengths(regmatches(h, gregexpr("x40 <- 40", h))), 1L)   # only the latest reply keeps its code
})
