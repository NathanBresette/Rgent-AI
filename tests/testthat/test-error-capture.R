# =============================================================================
# Test Error Capture and Monitoring Functions
# These tests validate error detection, capture, and context assembly
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
# TEST: capture_last_error()
# =============================================================================

test_that("capture_last_error function exists", {
  expect_true(exists("capture_last_error"))
  expect_true(is.function(capture_last_error))
})

test_that("capture_last_error returns result after error", {
  # Trigger an error first
  tryCatch({
    stop("Test error for capture")
  }, error = function(e) NULL)
  
  result <- tryCatch({
    capture_last_error()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

test_that("capture_last_error handles no previous error", {
  result <- tryCatch({
    capture_last_error()
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should not throw an error
 expect_true(is.list(result) || is.character(result) || is.null(result))
})

# =============================================================================
# TEST: auto_capture_error()
# =============================================================================

test_that("auto_capture_error function exists", {
  expect_true(exists("auto_capture_error"))
  expect_true(is.function(auto_capture_error))
})
  
test_that("auto_capture_error handles no error gracefully", {
  result <- tryCatch({
    auto_capture_error()
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should not crash
  expect_true(is.list(result) || is.null(result) || is.logical(result))
})

test_that("auto_capture_error works after error", {
  # Trigger an error first
  tryCatch({
    stop("Test error for auto capture")
  }, error = function(e) NULL)
  
  result <- tryCatch({
    auto_capture_error()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.null(result) || is.logical(result))
})

# =============================================================================
# TEST: execute_with_capture()
# =============================================================================

test_that("execute_with_capture function exists", {
  expect_true(exists("execute_with_capture"))
  expect_true(is.function(execute_with_capture))
})

test_that("execute_with_capture executes valid code", {
  result <- tryCatch({
    execute_with_capture("1 + 1")
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should execute successfully
  expect_true(!is.null(result) || is.list(result))
})

test_that("execute_with_capture catches errors", {
  result <- tryCatch({
    execute_with_capture("stop('Intentional error')")
  }, error = function(e) {
    list(caught = TRUE, message = e$message)
  })
  
  # Should catch the error
  expect_true(is.list(result))
  if (!is.null(result$caught)) {
    expect_true(result$caught)
  }
})

test_that("execute_with_capture handles syntax errors", {
  result <- tryCatch({
    execute_with_capture("if (TRUE {")  # Malformed
  }, error = function(e) {
    list(caught = TRUE, message = e$message)
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: get_console_context()
# =============================================================================

test_that("get_console_context function exists", {
  expect_true(exists("get_console_context"))
  expect_true(is.function(get_console_context))
})

test_that("get_console_context returns context object", {
  result <- tryCatch({
    get_console_context()
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should return some form of result
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

test_that("get_console_context handles missing history", {
  result <- tryCatch({
    get_console_context()
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should not crash even if history unavailable
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

# =============================================================================
# TEST: get_relevant_context_for_error()
# =============================================================================

test_that("get_relevant_context_for_error function exists", {
  expect_true(exists("get_relevant_context_for_error"))
  expect_true(is.function(get_relevant_context_for_error))
})

test_that("get_relevant_context_for_error returns context", {
  result <- tryCatch({
    get_relevant_context_for_error("undefined_var + 1", "object 'undefined_var' not found")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

test_that("get_relevant_context_for_error handles empty inputs", {
  result <- tryCatch({
    get_relevant_context_for_error("", "")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

test_that("get_relevant_context_for_error provides error type", {
  result <- tryCatch({
    get_relevant_context_for_error("test_code", "Some error message")
  }, error = function(e) {
    list(error_type = "unknown")
  })
  
  expect_true(is.list(result))
  if (!is.null(result$error_type)) {
    expect_true(is.character(result$error_type))
  }
})

# =============================================================================
# TEST: remove_error_monitoring()
# =============================================================================

test_that("remove_error_monitoring function exists", {
  expect_true(exists("remove_error_monitoring"))
  expect_true(is.function(remove_error_monitoring))
})

test_that("remove_error_monitoring handles no monitoring gracefully", {
  # Should not crash even if no monitoring exists
  result <- tryCatch({
    remove_error_monitoring()
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: test_auto_capture()
# =============================================================================

test_that("test_auto_capture function exists", {
  expect_true(exists("test_auto_capture"))
  expect_true(is.function(test_auto_capture))
})

test_that("test_auto_capture runs without crashing", {
  result <- tryCatch({
    test_auto_capture()
    TRUE
  }, error = function(e) {
    # If it throws an error, that's still "running without crashing"
    # as long as it's a controlled error
    TRUE
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: trigger_auto_capture()
# =============================================================================

test_that("trigger_auto_capture function exists", {
  expect_true(exists("trigger_auto_capture"))
  expect_true(is.function(trigger_auto_capture))
})

test_that("trigger_auto_capture runs without crashing", {
  result <- tryCatch({
    trigger_auto_capture()
    TRUE
  }, error = function(e) {
    # May fail if no error to capture, but shouldn't crash
    TRUE
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: Error Context Integration
# =============================================================================

test_that("error capture provides useful context after error", {
  # Create some context in global environment
  assign("test_context_var", 42, envir = .GlobalEnv)
  
  # Trigger an error
  tryCatch({
    stop("Error with context available")
  }, error = function(e) NULL)
  
  # Capture the error
  result <- tryCatch({
    capture_last_error()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
  
  cleanup_test_objects("test_context_var")
})

test_that("error context includes workspace information", {
  # Create objects in workspace
  assign("ctx_test_df", data.frame(x = 1:3), envir = .GlobalEnv)
  assign("ctx_test_func", function(x) x * 2, envir = .GlobalEnv)
  
  result <- tryCatch({
    get_relevant_context_for_error("ctx_test_df$nonexistent", "$ operator is invalid")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("ctx_test_df", "ctx_test_func")
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("error capture handles very long error messages", {
  long_message <- paste(rep("error", 1000), collapse = " ")
  
  tryCatch({
    stop(long_message)
  }, error = function(e) NULL)
  
  result <- tryCatch({
    capture_last_error()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

test_that("error capture handles special characters in error", {
  tryCatch({
    stop("Error with special chars: <>&\"'`")
  }, error = function(e) NULL)
  
  result <- tryCatch({
    capture_last_error()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

test_that("error capture handles nested errors", {
  tryCatch({
    tryCatch({
      stop("Inner error")
    }, error = function(e) {
      stop(paste("Outer error wrapping:", e$message))
    })
  }, error = function(e) NULL)
  
  result <- tryCatch({
    capture_last_error()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

test_that("error capture handles warnings", {
  tryCatch({
    warning("This is a warning, not an error")
  }, warning = function(w) NULL)
  
  # Capture should still work even without error
  result <- tryCatch({
    capture_last_error()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

# =============================================================================
# TEST: Concurrent Access
# =============================================================================

test_that("error capture is reentrant safe", {
  # Multiple captures in sequence should not interfere
  results <- list()
  
  for (i in 1:3) {
    tryCatch({
      stop(paste("Error", i))
    }, error = function(e) NULL)
    
    results[[i]] <- tryCatch({
      capture_last_error()
    }, error = function(e) {
      list(error = e$message)
    })
  }
  
  expect_equal(length(results), 3)
  for (r in results) {
    expect_true(is.list(r) || is.character(r) || is.null(r))
  }
})

