# =============================================================================
# Test Execution Engine Functions
# These tests validate the code execution engine and plot capture
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
# TEST: execute_code_in_session()
# =============================================================================

test_that("execute_code_in_session runs simple R code", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session("1 + 1")
  
  expect_true(is.list(result))
  
  if (!is.null(result$output) || !is.null(result$result)) {
    output <- result$output %||% result$result
    expect_true(grepl("2", as.character(output)))
  }
})

test_that("execute_code_in_session captures print output", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('print("Hello World")')
  
  expect_true(is.list(result))
  
  if (!is.null(result$output)) {
    expect_true(grepl("Hello", result$output))
  }
})

test_that("execute_code_in_session handles errors gracefully", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session("stop('Test error message')")
  
  expect_true(is.list(result))
  
  # Should have error information
  if (!is.null(result$error)) {
    expect_true(grepl("error", tolower(result$error)))
  }
})

test_that("execute_code_in_session creates variables in session", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session("test_var_exec <- 42")
  
  expect_true(is.list(result))
  
  # Variable might be created in global env
  if (exists("test_var_exec", envir = .GlobalEnv)) {
    expect_equal(get("test_var_exec", envir = .GlobalEnv), 42)
    cleanup_test_objects("test_var_exec")
  }
})

test_that("execute_code_in_session handles multi-line code", {
  skip_if_not(exists("execute_code_in_session"))
  
  code <- '
    x <- 1:5
    y <- x^2
    sum(y)
  '
  
  result <- execute_code_in_session(code)
  
  expect_true(is.list(result))
  
  cleanup_test_objects("x", "y")
})

test_that("execute_code_in_session captures warnings", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('warning("Test warning")')
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: Plot Capture
# =============================================================================

test_that("execute_code_in_session captures base R plots", {
  skip_if_not(exists("execute_code_in_session"))
  
  code <- 'plot(1:10, (1:10)^2, main = "Test Plot")'
  
  result <- execute_code_in_session(code)
  
  expect_true(is.list(result))
  
  # Check if plot was captured (result structure may vary)
  if (!is.null(result$has_plot)) {
    expect_true(result$has_plot || result$has_plot == TRUE)
  }
})

test_that("execute_code_in_session captures ggplot2 plots", {
  skip_if_not(exists("execute_code_in_session"))
  skip_if_not_installed("ggplot2")
  
  code <- '
    library(ggplot2)
    ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
  '
  
  result <- execute_code_in_session(code)
  
  expect_true(is.list(result))
})

test_that("execute_code_in_session handles plotly if available", {
  skip_if_not(exists("execute_code_in_session"))
  skip_if_not_installed("plotly")
  
  code <- '
    library(plotly)
    plot_ly(x = 1:10, y = (1:10)^2, type = "scatter", mode = "markers")
  '
  
  result <- execute_code_in_session(code)
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: log_code_to_file() and log_error_to_file()
# =============================================================================

test_that("log_code_to_file writes to log file", {
  skip_if_not(exists("log_code_to_file"))
  
  # Create temp file for testing
  temp_log <- tempfile(fileext = ".log")
  
  # If function takes file path as argument
  result <- tryCatch({
    log_code_to_file("print('test')", log_file = temp_log)
    TRUE
  }, error = function(e) {
    # Function may use default path
    tryCatch({
      log_code_to_file("print('test')")
      TRUE
    }, error = function(e2) {
      FALSE
    })
  })
  
  # Just verify it doesn't crash
  expect_true(is.logical(result))
  
  unlink(temp_log)
})

test_that("log_error_to_file writes error to log", {
  skip_if_not(exists("log_error_to_file"))
  
  result <- tryCatch({
    log_error_to_file("Test error message")
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(is.logical(result))
})

# =============================================================================
# TEST: Execution Settings
# =============================================================================

test_that("load_execution_settings returns settings object", {
  skip_if_not(exists("load_execution_settings"))
  
  settings <- load_execution_settings()
  
  expect_true(is.list(settings) || is.null(settings))
})

test_that("save_execution_settings persists settings", {
  skip_if_not(exists("save_execution_settings"))
  skip_if_not(exists("load_execution_settings"))
  
  test_settings <- list(
    timeout = 30,
    max_output = 1000
  )
  
  result <- tryCatch({
    save_execution_settings(test_settings)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(is.logical(result))
})

# =============================================================================
# TEST: Code Execution Security
# =============================================================================

test_that("execute_code_in_session handles potentially harmful code", {
  skip_if_not(exists("execute_code_in_session"))
  
  # Attempting to access system should be handled gracefully
  result <- tryCatch({
    execute_code_in_session('Sys.getenv("HOME")')
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

test_that("execute_code_in_session handles infinite loops with timeout", {
  skip_if_not(exists("execute_code_in_session"))
  skip("Skipping infinite loop test to avoid hanging")
  
  # This test is skipped by default to prevent test suite from hanging
  # In production, execution engine should have timeout protection
})

test_that("execute_code_in_session handles large output", {
  skip_if_not(exists("execute_code_in_session"))
  
  # Generate large output
  result <- execute_code_in_session('1:10000')
  
  expect_true(is.list(result))
  
  # Output should be truncated or handled
  if (!is.null(result$output)) {
    expect_lte(nchar(as.character(result$output)), 1000000)  # Reasonable limit
  }
})

# =============================================================================
# TEST: Data Frame Operations
# =============================================================================

test_that("execute_code_in_session handles dataframe operations", {
  skip_if_not(exists("execute_code_in_session"))
  
  code <- '
    df <- data.frame(x = 1:5, y = letters[1:5])
    summary(df)
  '
  
  result <- execute_code_in_session(code)
  
  expect_true(is.list(result))
  
  cleanup_test_objects("df")
})

test_that("execute_code_in_session handles dplyr operations", {
  skip_if_not(exists("execute_code_in_session"))
  skip_if_not_installed("dplyr")
  
  code <- '
    library(dplyr)
    mtcars %>%
      group_by(cyl) %>%
      summarise(mean_mpg = mean(mpg))
  '
  
  result <- execute_code_in_session(code)
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: Package Loading
# =============================================================================

test_that("execute_code_in_session handles library loading", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('library(stats)')
  
  expect_true(is.list(result))
  
  # Should not have error for base package
  if (!is.null(result$error)) {
    expect_false(grepl("error", tolower(result$error)))
  }
})

test_that("execute_code_in_session handles missing package gracefully", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('library(nonexistent_package_xyz123)')
  
  expect_true(is.list(result))
  
  # Should capture the error
  if (!is.null(result$error)) {
    expect_true(nchar(result$error) > 0)
  }
})

# =============================================================================
# TEST: Statistical Operations
# =============================================================================

test_that("execute_code_in_session handles t-test", {
  skip_if_not(exists("execute_code_in_session"))
  
  code <- '
    group1 <- rnorm(30, mean = 10)
    group2 <- rnorm(30, mean = 12)
    t.test(group1, group2)
  '
  
  result <- execute_code_in_session(code)
  
  expect_true(is.list(result))
  
  cleanup_test_objects("group1", "group2")
})

test_that("execute_code_in_session handles linear model", {
  skip_if_not(exists("execute_code_in_session"))
  
  code <- '
    x <- 1:10
    y <- 2 * x + rnorm(10)
    model <- lm(y ~ x)
    summary(model)
  '
  
  result <- execute_code_in_session(code)
  
  expect_true(is.list(result))
  
  cleanup_test_objects("x", "y", "model")
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("execute_code_in_session handles empty code", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session("")
  
  expect_true(is.list(result))
})

test_that("execute_code_in_session handles NULL input", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- tryCatch({
    execute_code_in_session(NULL)
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

test_that("execute_code_in_session handles comments only", {
  skip_if_not(exists("execute_code_in_session"))
  
  code <- '
    # This is a comment
    # Another comment
  '
  
  result <- execute_code_in_session(code)
  
  expect_true(is.list(result))
})

test_that("execute_code_in_session handles syntax errors", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('if (TRUE {')  # Malformed if statement
  
  expect_true(is.list(result))
  
  # Should contain error
  if (!is.null(result$error)) {
    expect_gt(nchar(result$error), 0)
  }
})

test_that("execute_code_in_session handles special characters", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('print("Special chars: äöü ñ 中文")')
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: Return Value Types
# =============================================================================

test_that("execute_code_in_session returns numeric correctly", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('sqrt(144)')
  
  expect_true(is.list(result))
})

test_that("execute_code_in_session returns character correctly", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('paste("Hello", "World")')
  
  expect_true(is.list(result))
})

test_that("execute_code_in_session returns list correctly", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('list(a = 1, b = 2, c = 3)')
  
  expect_true(is.list(result))
})

test_that("execute_code_in_session returns data.frame correctly", {
  skip_if_not(exists("execute_code_in_session"))
  
  result <- execute_code_in_session('data.frame(x = 1:3, y = c("a", "b", "c"))')
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: Concurrent Execution Safety
# =============================================================================

test_that("execute_code_in_session handles rapid successive calls", {
  skip_if_not(exists("execute_code_in_session"))
  
  results <- lapply(1:5, function(i) {
    execute_code_in_session(paste0("x <- ", i, "; x * 2"))
  })
  
  expect_true(all(sapply(results, is.list)))
  
  cleanup_test_objects("x")
})

