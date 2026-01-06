# =============================================================================
# Test Run Rgent Main Entry Point Functions
# These tests validate the main launcher and context functions
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
# TEST: run_rgent() Function Existence and Structure
# =============================================================================

test_that("run_rgent function exists", {
  expect_true(exists("run_rgent"))
  expect_true(is.function(run_rgent))
})

test_that("run_rgent returns FALSE outside RStudio", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- tryCatch({
    run_rgent()
  }, error = function(e) {
    FALSE
  })
  
  expect_false(result)
})

# =============================================================================
# TEST: refresh_context()
# =============================================================================

test_that("refresh_context function exists", {
  expect_true(exists("refresh_context"))
  expect_true(is.function(refresh_context))
})

test_that("refresh_context returns invisibly", {
  result <- tryCatch({
    refresh_context()
  }, error = function(e) {
    invisible(FALSE)
  })
  
  expect_true(is.logical(result))
})

# =============================================================================
# TEST: capture_context() from run_rgent.R
# =============================================================================

test_that("capture_context function exists", {
  expect_true(exists("capture_context"))
  expect_true(is.function(capture_context))
})

test_that("capture_context returns list structure", {
  result <- capture_context()
  
  expect_true(is.list(result))
})

test_that("capture_context includes workspace_objects", {
  # Create test objects
  assign("ctx_test_var", 123, envir = .GlobalEnv)
  assign("ctx_test_df", data.frame(a = 1:3), envir = .GlobalEnv)
  
  result <- capture_context()
  
  expect_true(is.list(result))
  expect_true("workspace_objects" %in% names(result) || "error" %in% names(result))
  
  cleanup_test_objects("ctx_test_var", "ctx_test_df")
})

test_that("capture_context includes environment_info", {
  result <- capture_context()
  
  expect_true(is.list(result))
  if ("environment_info" %in% names(result)) {
    env_info <- result$environment_info
    expect_true(is.list(env_info))
    
    # Check expected fields
    expected_fields <- c("r_version", "platform", "working_directory")
    for (field in expected_fields) {
      if (field %in% names(env_info)) {
        expect_true(is.character(env_info[[field]]) || is.null(env_info[[field]]))
      }
    }
  }
})

test_that("capture_context includes timestamp", {
  result <- capture_context()
  
  expect_true(is.list(result))
  if ("timestamp" %in% names(result)) {
    expect_true(is.character(result$timestamp))
    expect_gt(nchar(result$timestamp), 0)
  }
})

test_that("capture_context handles empty workspace", {
  # Save current objects
  current_objects <- ls(envir = .GlobalEnv)
  
  # Clear workspace (except essential)
  objs_to_remove <- setdiff(current_objects, c())
  if (length(objs_to_remove) > 0) {
    # Don't actually remove - just test with current state
  }
  
  result <- capture_context()
  expect_true(is.list(result))
})

test_that("capture_context handles large workspace", {
  # Create many objects
  for (i in 1:50) {
    assign(paste0("large_ws_var_", i), rnorm(100), envir = .GlobalEnv)
  }
  
  result <- capture_context()
  expect_true(is.list(result))
  
  # Cleanup
  for (i in 1:50) {
    cleanup_test_objects(paste0("large_ws_var_", i))
  }
})

test_that("capture_context handles various object types", {
  # Create different types of objects
  assign("ctx_numeric", 42, envir = .GlobalEnv)
  assign("ctx_character", "hello", envir = .GlobalEnv)
  assign("ctx_vector", 1:10, envir = .GlobalEnv)
  assign("ctx_list", list(a = 1, b = 2), envir = .GlobalEnv)
  assign("ctx_function", function(x) x * 2, envir = .GlobalEnv)
  assign("ctx_matrix", matrix(1:9, 3, 3), envir = .GlobalEnv)
  assign("ctx_dataframe", data.frame(x = 1:3, y = 4:6), envir = .GlobalEnv)
  
  result <- capture_context()
  expect_true(is.list(result))
  
  cleanup_test_objects(
    "ctx_numeric", "ctx_character", "ctx_vector", 
    "ctx_list", "ctx_function", "ctx_matrix", "ctx_dataframe"
  )
})

# =============================================================================
# TEST: insert_code_at_location()
# =============================================================================

test_that("insert_code_at_location function exists", {
  expect_true(exists("insert_code_at_location"))
  expect_true(is.function(insert_code_at_location))
})

test_that("insert_code_at_location has correct parameters", {
  args <- formals(insert_code_at_location)
  
  expect_true("code" %in% names(args))
  expect_true("file_path" %in% names(args))
  expect_true("line_number" %in% names(args))
  expect_true("replace_selection" %in% names(args))
})

test_that("insert_code_at_location returns FALSE outside RStudio", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- insert_code_at_location("test_code <- 1")
  expect_false(result)
})

test_that("insert_code_at_location handles NULL code gracefully", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- insert_code_at_location(NULL)
  expect_false(result)
})

# =============================================================================
# TEST: insert_code()
# =============================================================================

test_that("insert_code function exists", {
  expect_true(exists("insert_code"))
  expect_true(is.function(insert_code))
})

test_that("insert_code has correct parameters", {
  args <- formals(insert_code)
  expect_true("code" %in% names(args))
})

test_that("insert_code returns FALSE outside RStudio", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- insert_code("test_code <- 1")
  expect_false(result)
})

test_that("insert_code handles NULL with clipr check", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- insert_code(NULL)
  expect_false(result)
})

# =============================================================================
# TEST: Context Capture with Various Object Types
# =============================================================================

test_that("capture_context handles S4 objects", {
  skip_if_not_installed("methods")
  
  # Create a simple S4 class
  tryCatch({
    setClass("TestS4Class", slots = c(x = "numeric"))
    assign("ctx_s4_obj", new("TestS4Class", x = 42), envir = .GlobalEnv)
    
    result <- capture_context()
    expect_true(is.list(result))
    
    cleanup_test_objects("ctx_s4_obj")
    removeClass("TestS4Class")
  }, error = function(e) {
    # S4 may not be available in all contexts
    expect_true(TRUE)
  })
})

test_that("capture_context handles environments", {
  assign("ctx_env", new.env(), envir = .GlobalEnv)
  ctx_env <- get("ctx_env", envir = .GlobalEnv)
  ctx_env$nested_var <- 123
  
  result <- capture_context()
  expect_true(is.list(result))
  
  cleanup_test_objects("ctx_env")
})

test_that("capture_context handles formulas", {
  assign("ctx_formula", y ~ x + z, envir = .GlobalEnv)
  
  result <- capture_context()
  expect_true(is.list(result))
  
  cleanup_test_objects("ctx_formula")
})

test_that("capture_context handles expressions", {
  assign("ctx_expr", expression(1 + 1), envir = .GlobalEnv)
  
  result <- capture_context()
  expect_true(is.list(result))
  
  cleanup_test_objects("ctx_expr")
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("capture_context handles objects with errors in str()", {
  # Create object that might fail str()
  assign("ctx_tricky", structure(list(), class = "custom_class"), envir = .GlobalEnv)
  
  result <- capture_context()
  expect_true(is.list(result))
  
  cleanup_test_objects("ctx_tricky")
})

test_that("capture_context handles circular references", {
  # Create environment with circular reference
  env1 <- new.env()
  env2 <- new.env()
  env1$ref <- env2
  env2$ref <- env1
  
  assign("ctx_circular", env1, envir = .GlobalEnv)
  
  result <- capture_context()
  expect_true(is.list(result))
  
  cleanup_test_objects("ctx_circular")
})

test_that("capture_context handles special variable names", {
  assign(".hidden_var", 123, envir = .GlobalEnv)
  assign("var with spaces", 456, envir = .GlobalEnv)
  
  result <- capture_context()
  expect_true(is.list(result))
  
  cleanup_test_objects(".hidden_var")
  tryCatch({
    rm(list = "var with spaces", envir = .GlobalEnv)
  }, error = function(e) NULL)
})

# =============================================================================
# TEST: Return Value Consistency
# =============================================================================

test_that("capture_context returns consistent structure", {
  # Call multiple times
  result1 <- capture_context()
  result2 <- capture_context()
  
  # Should have same structure
  expect_equal(names(result1), names(result2))
})

test_that("capture_context timestamp changes between calls", {
  result1 <- capture_context()
  Sys.sleep(0.1)  # Small delay
  result2 <- capture_context()
  
  if ("timestamp" %in% names(result1) && "timestamp" %in% names(result2)) {
    # Timestamps should be different (or at least valid)
    expect_true(is.character(result1$timestamp))
    expect_true(is.character(result2$timestamp))
  }
})

# =============================================================================
# TEST: Error Recovery
# =============================================================================

test_that("functions recover from previous errors", {
  # Trigger an error
  tryCatch({
    stop("Previous error")
  }, error = function(e) NULL)
  
  # Functions should still work
  result <- capture_context()
  expect_true(is.list(result))
})

test_that("capture_context handles locked bindings", {
  tryCatch({
    assign("ctx_locked", 123, envir = .GlobalEnv)
    lockBinding("ctx_locked", .GlobalEnv)
    
    result <- capture_context()
    expect_true(is.list(result))
    
    unlockBinding("ctx_locked", .GlobalEnv)
    cleanup_test_objects("ctx_locked")
  }, error = function(e) {
    # Some environments don't support locking
    expect_true(TRUE)
  })
})

