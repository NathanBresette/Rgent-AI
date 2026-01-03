# =============================================================================
# Test AI Code Functions
# These tests validate the AI-driven code insertion and manipulation functions
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
# TEST: execute_code()
# =============================================================================

test_that("execute_code executes simple arithmetic", {
  result <- execute_code("1 + 1")
  expect_true(result)
})

test_that("execute_code creates variables in global environment", {
  result <- execute_code("test_exec_var <- 42")
  expect_true(result)
  expect_true(exists("test_exec_var", envir = .GlobalEnv))
  expect_equal(get("test_exec_var", envir = .GlobalEnv), 42)
  cleanup_test_objects("test_exec_var")
})

test_that("execute_code handles multi-line code", {
  code <- "
    test_x <- 10
    test_y <- 20
    test_z <- test_x + test_y
  "
  result <- execute_code(code)
  expect_true(result)
  expect_true(exists("test_z", envir = .GlobalEnv))
  expect_equal(get("test_z", envir = .GlobalEnv), 30)
  cleanup_test_objects("test_x", "test_y", "test_z")
})

test_that("execute_code handles errors gracefully", {
  result <- execute_code("stop('Test error')")
  expect_false(result)
})

test_that("execute_code handles syntax errors gracefully", {
  result <- execute_code("if (TRUE {")  # Malformed
  expect_false(result)
})

test_that("execute_code handles undefined variables", {
  result <- execute_code("undefined_var_xyz + 1")
  expect_false(result)
})

test_that("execute_code handles NULL result", {
  result <- execute_code("invisible(NULL)")
  expect_true(result)
})

test_that("execute_code handles function creation", {
  code <- "test_func <- function(x) x * 2"
  result <- execute_code(code)
  expect_true(result)
  expect_true(exists("test_func", envir = .GlobalEnv))
  test_func <- get("test_func", envir = .GlobalEnv)
  expect_equal(test_func(5), 10)
  cleanup_test_objects("test_func")
})

test_that("execute_code handles data frame creation", {
  code <- "test_df <- data.frame(a = 1:3, b = c('x', 'y', 'z'))"
  result <- execute_code(code)
  expect_true(result)
  expect_true(exists("test_df", envir = .GlobalEnv))
  test_df <- get("test_df", envir = .GlobalEnv)
  expect_equal(nrow(test_df), 3)
  expect_equal(ncol(test_df), 2)
  cleanup_test_objects("test_df")
})

test_that("execute_code handles library loading", {
  result <- execute_code("library(stats)")
  expect_true(result)
})

test_that("execute_code handles print output", {
  result <- execute_code('print("Hello World")')
  expect_true(result)
})

test_that("execute_code handles empty string", {
  result <- execute_code("")
  # Empty code should execute without error
  expect_true(is.logical(result))
})

test_that("execute_code handles whitespace only", {
  result <- execute_code("   \n\t  ")
  expect_true(is.logical(result))
})

# =============================================================================
# TEST: execute_r_function()
# =============================================================================

test_that("execute_r_function validates function whitelist", {
  # Invalid function should return FALSE
  result <- execute_r_function("invalid_function", "code")
  expect_false(result)
})

test_that("execute_r_function rejects non-whitelisted functions", {
  result <- execute_r_function("rm", "test_var")
  expect_false(result)
})

test_that("execute_r_function rejects system functions", {
  result <- execute_r_function("system", "ls")
  expect_false(result)
})

test_that("execute_r_function accepts valid function names", {
  # Should accept but return FALSE (no RStudio)
  result <- execute_r_function("ai_smart_insert", "code", "top")
  expect_false(result)  # Will fail without RStudio
})

test_that("execute_r_function handles errors gracefully", {
  result <- tryCatch({
    execute_r_function(NULL, "param1")
  }, error = function(e) {
    FALSE
  })
  expect_false(result)
})

test_that("execute_r_function handles missing params gracefully", {
  result <- tryCatch({
    execute_r_function("ai_insert_code")
  }, error = function(e) {
    FALSE
  })
  expect_false(result)
})

# =============================================================================
# TEST: ai_insert_code() - RStudio API Functions
# =============================================================================

test_that("ai_insert_code returns FALSE when RStudio not available", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- ai_insert_code("test_code <- 1")
  expect_false(result)
})

test_that("ai_insert_code handles NULL code gracefully", {
  result <- tryCatch({
    ai_insert_code(NULL)
  }, error = function(e) {
    FALSE
  })
  expect_false(result)
})

test_that("ai_insert_code is a function", {
  expect_true(is.function(ai_insert_code))
})

test_that("ai_insert_code has correct parameters", {
  args <- formals(ai_insert_code)
  expect_true("code" %in% names(args))
  expect_true("context_context" %in% names(args))
})

# =============================================================================
# TEST: ai_replace_code() - RStudio API Functions
# =============================================================================

test_that("ai_replace_code returns FALSE when RStudio not available", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- ai_replace_code("old_pattern", "new_code")
  expect_false(result)
})

test_that("ai_replace_code is a function", {
  expect_true(is.function(ai_replace_code))
})

test_that("ai_replace_code has correct parameters", {
  args <- formals(ai_replace_code)
  expect_true("old_code_pattern" %in% names(args))
  expect_true("new_code" %in% names(args))
})

# =============================================================================
# TEST: ai_smart_insert() - RStudio API Functions
# =============================================================================

test_that("ai_smart_insert returns FALSE when RStudio not available", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- ai_smart_insert("test_code <- 1", "top")
  expect_false(result)
})

test_that("ai_smart_insert is a function", {
  expect_true(is.function(ai_smart_insert))
})

test_that("ai_smart_insert has correct parameters", {
  args <- formals(ai_smart_insert)
  expect_true("code" %in% names(args))
  expect_true("location_hint" %in% names(args))
})

test_that("ai_smart_insert accepts location hints", {
  # These should fail gracefully without RStudio
  hints <- c("top", "bottom", "end", "start", "cursor", "after function")
  for (hint in hints) {
    result <- ai_smart_insert("code", hint)
    expect_false(result)  # False because no RStudio
  }
})

# =============================================================================
# TEST: Function Integration
# =============================================================================

test_that("execute_r_function routes to ai_insert_code", {
  # Should attempt to call ai_insert_code
  result <- execute_r_function("ai_insert_code", "test_code", NULL)
  expect_false(result)  # Will fail without RStudio
})

test_that("execute_r_function routes to ai_replace_code", {
  result <- execute_r_function("ai_replace_code", "old_pattern", "new_code")
  expect_false(result)  # Will fail without RStudio
})

test_that("execute_r_function routes to ai_smart_insert", {
  result <- execute_r_function("ai_smart_insert", "code", "top")
  expect_false(result)  # Will fail without RStudio
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("execute_code handles very long code", {
  # Generate long code
  long_code <- paste0("long_var <- c(", paste(1:1000, collapse = ", "), ")")
  result <- execute_code(long_code)
  expect_true(result)
  cleanup_test_objects("long_var")
})

test_that("execute_code handles special characters", {
  result <- execute_code('special_var <- "Special chars: @#$%^&*()"')
  expect_true(result)
  cleanup_test_objects("special_var")
})

test_that("execute_code handles unicode characters", {
  result <- execute_code('unicode_var <- "Unicode: cafe"')
  expect_true(result)
  cleanup_test_objects("unicode_var")
})

test_that("execute_code handles comments", {
  code <- "
    # This is a comment
    comment_var <- 123  # Inline comment
    # Another comment
  "
  result <- execute_code(code)
  expect_true(result)
  expect_equal(get("comment_var", envir = .GlobalEnv), 123)
  cleanup_test_objects("comment_var")
})

test_that("execute_code handles pipe operators", {
  skip_if_not_installed("magrittr")
  code <- "
    library(magrittr)
    pipe_result <- c(1, 2, 3) %>% sum()
  "
  result <- execute_code(code)
  expect_true(result)
  cleanup_test_objects("pipe_result")
})

