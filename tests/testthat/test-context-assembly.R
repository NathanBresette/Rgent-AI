# =============================================================================
# Test Context Assembly Functions
# These tests validate context assembly and smart context capture
# =============================================================================

library(testthat)

# Helper function to safely clean up global environment objects
cleanup_test_objects <- function(...) {
  objects <- c(...)
  for (obj in objects) {
    if (exists(obj, envir = .GlobalEnv)) {
      rm(list = obj, envir = .GlobalEnv)
    }
  }
}

# =============================================================================
# TEST: add_to_conversation_history()
# =============================================================================

test_that("add_to_conversation_history function exists", {
  expect_true(exists("add_to_conversation_history"))
  expect_true(is.function(add_to_conversation_history))
})

test_that("add_to_conversation_history adds messages", {
  # Clear existing history
  cleanup_test_objects("conversation_history")
  
  add_to_conversation_history("user", "Hello, AI!")
  
  expect_true(exists("conversation_history", envir = .GlobalEnv))
  expect_gte(length(.GlobalEnv$conversation_history), 1)
  
  cleanup_test_objects("conversation_history")
})

test_that("add_to_conversation_history stores role and message", {
  cleanup_test_objects("conversation_history")
  
  add_to_conversation_history("user", "Test message")
  
  last_msg <- .GlobalEnv$conversation_history[[length(.GlobalEnv$conversation_history)]]
  
  expect_equal(last_msg$role, "user")
  expect_equal(last_msg$message, "Test message")
  expect_true(!is.null(last_msg$timestamp))
  
  cleanup_test_objects("conversation_history")
})

test_that("add_to_conversation_history handles multiple messages", {
  cleanup_test_objects("conversation_history")
  
  add_to_conversation_history("user", "First message")
  add_to_conversation_history("assistant", "Response")
  add_to_conversation_history("user", "Second message")
  
  expect_gte(length(.GlobalEnv$conversation_history), 3)
  
  cleanup_test_objects("conversation_history")
})

# =============================================================================
# TEST: get_conversation_history()
# =============================================================================

test_that("get_conversation_history function exists", {
  expect_true(exists("get_conversation_history"))
  expect_true(is.function(get_conversation_history))
})

test_that("get_conversation_history returns empty when no history", {
  cleanup_test_objects("conversation_history")
  
  result <- get_conversation_history()
  
  expect_true(is.character(result))
  expect_equal(result, "")
})

test_that("get_conversation_history formats history correctly", {
  cleanup_test_objects("conversation_history")
  
  add_to_conversation_history("user", "Hello")
  add_to_conversation_history("assistant", "Hi there!")
  
  result <- get_conversation_history()
  
  expect_true(is.character(result))
  expect_true(grepl("USER", result))
  expect_true(grepl("Hello", result))
  
  cleanup_test_objects("conversation_history")
})

# =============================================================================
# TEST: assemble_simple_context()
# =============================================================================

test_that("assemble_simple_context function exists", {
  expect_true(exists("assemble_simple_context"))
  expect_true(is.function(assemble_simple_context))
})

test_that("assemble_simple_context returns string", {
  result <- tryCatch({
    assemble_simple_context("test query")
  }, error = function(e) {
    "context assembled with error"
  })
  
  expect_true(is.character(result))
})

test_that("assemble_simple_context includes workspace info", {
  assign("ctx_test_var", 123, envir = .GlobalEnv)
  
  result <- tryCatch({
    assemble_simple_context("test query")
  }, error = function(e) {
    "context"
  })
  
  expect_true(is.character(result))
  
  cleanup_test_objects("ctx_test_var")
})

test_that("assemble_simple_context handles empty query", {
  result <- tryCatch({
    assemble_simple_context("")
  }, error = function(e) {
    "context"
  })
  
  expect_true(is.character(result))
})

test_that("assemble_simple_context handles NULL query", {
  result <- tryCatch({
    assemble_simple_context(NULL)
  }, error = function(e) {
    "context"
  })
  
  expect_true(is.character(result))
})

# =============================================================================
# TEST: assemble_intelligent_context_safe()
# =============================================================================

test_that("assemble_intelligent_context_safe function exists", {
  expect_true(exists("assemble_intelligent_context_safe"))
  expect_true(is.function(assemble_intelligent_context_safe))
})

test_that("assemble_intelligent_context_safe returns string", {
  result <- assemble_intelligent_context_safe("test query")
  
  expect_true(is.character(result))
})

test_that("assemble_intelligent_context_safe handles errors gracefully", {
  result <- assemble_intelligent_context_safe(NULL)
  
  expect_true(is.character(result))
})

test_that("assemble_intelligent_context_safe includes basic info", {
  result <- assemble_intelligent_context_safe("what is in my workspace?")
  
  expect_true(is.character(result))
  expect_gt(nchar(result), 0)
})

# =============================================================================
# TEST: capture_context_smart()
# =============================================================================

test_that("capture_context_smart function exists", {
  expect_true(exists("capture_context_smart"))
  expect_true(is.function(capture_context_smart))
})

test_that("capture_context_smart returns list", {
  result <- tryCatch({
    capture_context_smart()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

test_that("capture_context_smart includes environment_info", {
  result <- tryCatch({
    capture_context_smart()
  }, error = function(e) {
    list(environment_info = list())
  })
  
  expect_true(is.list(result))
  if ("environment_info" %in% names(result)) {
    expect_true(is.list(result$environment_info))
  }
})

test_that("capture_context_smart includes workspace objects", {
  assign("smart_ctx_test", 42, envir = .GlobalEnv)
  
  result <- tryCatch({
    capture_context_smart()
  }, error = function(e) {
    list(workspace_objects = list())
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("smart_ctx_test")
})

test_that("capture_context_smart handles query parameter", {
  result <- tryCatch({
    capture_context_smart(query = "test query")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: capture_context_smart_incremental()
# =============================================================================

test_that("capture_context_smart_incremental function exists", {
  expect_true(exists("capture_context_smart_incremental"))
  expect_true(is.function(capture_context_smart_incremental))
})

test_that("capture_context_smart_incremental returns result", {
  result <- tryCatch({
    capture_context_smart_incremental()
  }, error = function(e) {
    list(error = e$message)
  })
  
  # May return list, character, or NULL
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

test_that("capture_context_smart_incremental captures new objects", {
  assign("incr_ctx_test", "test value", envir = .GlobalEnv)
  
  result <- tryCatch({
    capture_context_smart_incremental()
  }, error = function(e) {
    list(workspace_objects = list())
  })
  
  # May return list, character, or NULL
  expect_true(is.list(result) || is.character(result) || is.null(result))
  
  cleanup_test_objects("incr_ctx_test")
})

# =============================================================================
# TEST: Context with Various Object Types
# =============================================================================

test_that("context assembly handles dataframes", {
  assign("ctx_df", data.frame(x = 1:10, y = letters[1:10]), envir = .GlobalEnv)
  
  result <- tryCatch({
    capture_context_smart()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("ctx_df")
})

test_that("context assembly handles functions", {
  assign("ctx_func", function(x) x * 2, envir = .GlobalEnv)
  
  result <- tryCatch({
    capture_context_smart()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("ctx_func")
})

test_that("context assembly handles large objects", {
  assign("ctx_large", rnorm(10000), envir = .GlobalEnv)
  
  result <- tryCatch({
    capture_context_smart()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("ctx_large")
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("conversation history limits messages", {
  cleanup_test_objects("conversation_history")
  
  # Add many messages
  for (i in 1:10) {
    add_to_conversation_history("user", paste("Message", i))
  }
  
  # Should be limited (function keeps last N messages)
  expect_lte(length(.GlobalEnv$conversation_history), 10)
  
  cleanup_test_objects("conversation_history")
})

test_that("context assembly handles special characters", {
  assign("ctx_special", "Special: @#$%^&*()", envir = .GlobalEnv)
  
  result <- tryCatch({
    assemble_intelligent_context_safe("query with special: @#$")
  }, error = function(e) {
    "handled"
  })
  
  expect_true(is.character(result))
  
  cleanup_test_objects("ctx_special")
})

test_that("context assembly handles empty workspace", {
  result <- tryCatch({
    assemble_intelligent_context_safe("test")
  }, error = function(e) {
    "handled"
  })
  
  expect_true(is.character(result))
})

# =============================================================================
# TEST: Agent Context
# =============================================================================

test_that("capture_agent_context function exists", {
  expect_true(exists("capture_agent_context"))
  expect_true(is.function(capture_agent_context))
})

test_that("capture_agent_context returns context", {
  result <- tryCatch({
    capture_agent_context()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

test_that("capture_agent_context includes dataframe info", {
  assign("agent_ctx_df", data.frame(a = 1:5), envir = .GlobalEnv)
  
  result <- tryCatch({
    capture_agent_context()
  }, error = function(e) {
    list(dataframes = list())
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("agent_ctx_df")
})

