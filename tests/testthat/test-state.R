# Rgent's state lives in the package, not the user's global environment (R/state.R)

test_that("rm(list = ls()) leaves Rgent's access code and chat alone", {
  st <- rstudioai:::.rgent_state
  old <- as.list(st)
  on.exit({ st$current_access_code <- old$current_access_code; st$conversation_history <- old$conversation_history })
  st$current_access_code <- "CODE-1"
  st$conversation_history <- list()
  rstudioai:::add_to_conversation_history("user", "hello")
  e <- globalenv()
  saved <- mget(ls(e), envir = e)
  rm(list = ls(e), envir = e)                    # the user's rm(list = ls())
  on.exit(list2env(saved, envir = e), add = TRUE)
  expect_false(any(c("current_access_code", "conversation_history") %in% ls(e)))
  expect_equal(st$current_access_code, "CODE-1")
  expect_length(st$conversation_history, 1)
})

test_that("leftovers from older versions are cleared from the global environment, users' own objects kept", {
  e <- new.env()
  st <- rstudioai:::.rgent_state
  old_code <- st$current_access_code
  on.exit(st$current_access_code <- old_code)
  st$current_access_code <- NULL
  assign("current_access_code", "OLD-CODE", envir = e)
  assign("conversation_history", list(list(role = "user", message = "q", timestamp = Sys.time())), envir = e)
  assign("workspace_index", data.frame(objects = 1), envir = e)   # someone's own: wrong shape, kept
  rstudioai:::.rgent_clear_old_globals(e)
  expect_false(exists("current_access_code", envir = e, inherits = FALSE))
  expect_false(exists("conversation_history", envir = e, inherits = FALSE))
  expect_true(exists("workspace_index", envir = e, inherits = FALSE))
  expect_equal(st$current_access_code, "OLD-CODE")   # still signed in
})
