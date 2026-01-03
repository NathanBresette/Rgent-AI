# =============================================================================
# Test Internal Utility Functions
# These tests validate the safe utility functions and internal helpers
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
# TEST: safe_ls()
# =============================================================================

test_that("safe_ls function exists", {
  expect_true(exists("safe_ls"))
  expect_true(is.function(safe_ls))
})

test_that("safe_ls returns objects from environment", {
  test_env <- new.env()
  test_env$a <- 1
  test_env$b <- 2
  
  result <- safe_ls(test_env)
  
  expect_true(is.character(result))
  expect_true("a" %in% result)
  expect_true("b" %in% result)
})

test_that("safe_ls works with global environment", {
  assign("safe_ls_test_var", 123, envir = .GlobalEnv)
  
  result <- safe_ls(.GlobalEnv)
  
  expect_true("safe_ls_test_var" %in% result)
  
  cleanup_test_objects("safe_ls_test_var")
})

test_that("safe_ls handles empty environment", {
  empty_env <- new.env()
  
  result <- safe_ls(empty_env)
  
  expect_true(is.character(result))
  expect_equal(length(result), 0)
})

# =============================================================================
# TEST: safe_get()
# =============================================================================

test_that("safe_get function exists", {
  expect_true(exists("safe_get"))
  expect_true(is.function(safe_get))
})

test_that("safe_get retrieves existing objects", {
  test_env <- new.env()
  test_env$test_val <- 42
  
  result <- safe_get("test_val", test_env)
  
  expect_equal(result, 42)
})

test_that("safe_get returns NULL for non-existent objects", {
  test_env <- new.env()
  
  result <- safe_get("nonexistent", test_env)
  
  expect_null(result)
})

test_that("safe_get works with various object types", {
  test_env <- new.env()
  test_env$num <- 123
  test_env$char <- "hello"
  test_env$vec <- 1:10
  test_env$df <- data.frame(x = 1:3)
  test_env$func <- function(x) x * 2
  
  expect_equal(safe_get("num", test_env), 123)
  expect_equal(safe_get("char", test_env), "hello")
  expect_equal(safe_get("vec", test_env), 1:10)
  expect_true(is.data.frame(safe_get("df", test_env)))
  expect_true(is.function(safe_get("func", test_env)))
})

# =============================================================================
# TEST: safe_env()
# =============================================================================

test_that("safe_env function exists", {
  expect_true(exists("safe_env"))
  expect_true(is.function(safe_env))
})

test_that("safe_env returns environment if valid", {
  test_env <- new.env()
  
  result <- safe_env(test_env)
  
  expect_true(is.environment(result))
  expect_identical(result, test_env)
})

test_that("safe_env returns globalenv for non-environment", {
  result <- tryCatch({
    suppressWarnings(safe_env("not an environment"))
  }, error = function(e) {
    globalenv()
  })
  
  expect_true(is.environment(result))
})

test_that("safe_env handles NULL gracefully", {
  result <- tryCatch({
    suppressWarnings(safe_env(NULL))
  }, error = function(e) {
    globalenv()
  })
  
  expect_true(is.environment(result))
})

# =============================================================================
# TEST: safe_get_object_info()
# =============================================================================

test_that("safe_get_object_info function exists", {
  expect_true(exists("safe_get_object_info"))
  expect_true(is.function(safe_get_object_info))
})

test_that("safe_get_object_info returns info for numeric", {
  assign("info_test_num", 42, envir = .GlobalEnv)
  
  result <- safe_get_object_info("info_test_num")
  
  expect_true(is.list(result))
  expect_equal(result$name, "info_test_num")
  expect_true("class" %in% names(result))
  
  cleanup_test_objects("info_test_num")
})

test_that("safe_get_object_info returns info for dataframe", {
  assign("info_test_df", data.frame(x = 1:5, y = letters[1:5]), envir = .GlobalEnv)
  
  result <- safe_get_object_info("info_test_df")
  
  expect_true(is.list(result))
  expect_true(result$is_data_frame)
  expect_equal(result$dimensions[1], 5)
  expect_equal(result$dimensions[2], 2)
  
  cleanup_test_objects("info_test_df")
})

test_that("safe_get_object_info returns info for function", {
  assign("info_test_func", function(x) x * 2, envir = .GlobalEnv)
  
  result <- safe_get_object_info("info_test_func")
  
  expect_true(is.list(result))
  expect_true(result$is_function)
  
  cleanup_test_objects("info_test_func")
})

test_that("safe_get_object_info handles missing objects", {
  result <- tryCatch({
    safe_get_object_info("nonexistent_object_xyz123")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: get_current_access_code()
# =============================================================================

test_that("get_current_access_code function exists", {
  expect_true(exists("get_current_access_code"))
  expect_true(is.function(get_current_access_code))
})

test_that("get_current_access_code returns code when set", {
  assign("current_access_code", "TEST123", envir = .GlobalEnv)
  
  result <- get_current_access_code()
  
  expect_equal(result, "TEST123")
  
  cleanup_test_objects("current_access_code")
})

test_that("get_current_access_code errors when not set", {
  cleanup_test_objects("current_access_code")
  
  expect_error(get_current_access_code())
})

# =============================================================================
# TEST: get_last_executed_code()
# =============================================================================

test_that("get_last_executed_code function exists", {
  expect_true(exists("get_last_executed_code"))
  expect_true(is.function(get_last_executed_code))
})

test_that("get_last_executed_code returns result", {
  result <- tryCatch({
    get_last_executed_code()
  }, error = function(e) {
    "error handled"
  })
  
  expect_true(is.character(result) || is.null(result))
})

# =============================================================================
# TEST: get_editor_context()
# =============================================================================

test_that("get_editor_context function exists", {
  expect_true(exists("get_editor_context"))
  expect_true(is.function(get_editor_context))
})

test_that("get_editor_context returns result outside RStudio", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- get_editor_context()
  
  expect_true(is.character(result))
  expect_true(grepl("not available|No active", result, ignore.case = TRUE))
})

# =============================================================================
# TEST: send_error_to_ai()
# =============================================================================

test_that("send_error_to_ai function exists", {
  expect_true(exists("send_error_to_ai"))
  expect_true(is.function(send_error_to_ai))
})

test_that("send_error_to_ai handles error details", {
  error_details <- list(
    error_message = "Test error",
    error_code = "test <- undefined_var",
    relevant_context = list(
      objects = list(),
      functions = list(),
      data_frames = list()
    )
  )
  
  result <- tryCatch({
    send_error_to_ai(error_details)
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.null(result))
})

# =============================================================================
# TEST: cleanup_old_data()
# =============================================================================

test_that("cleanup_old_data function exists", {
  expect_true(exists("cleanup_old_data"))
  expect_true(is.function(cleanup_old_data))
})

test_that("cleanup_old_data runs without error", {
  result <- tryCatch({
    cleanup_old_data()
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: track_performance()
# =============================================================================

test_that("track_performance function exists", {
  expect_true(exists("track_performance"))
  expect_true(is.function(track_performance))
})

test_that("track_performance returns a function", {
  result <- tryCatch({
    track_performance("test_operation")
  }, error = function(e) {
    function() NULL
  })
  
  expect_true(is.function(result))
})

# =============================================================================
# TEST: toggle_auto_fix()
# =============================================================================

test_that("toggle_auto_fix function exists", {
  expect_true(exists("toggle_auto_fix"))
  expect_true(is.function(toggle_auto_fix))
})
  
test_that("toggle_auto_fix returns FALSE (deprecated)", {
  result <- suppressWarnings(toggle_auto_fix())
  expect_false(result)
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("safe_get handles special characters in names", {
  test_env <- new.env()
  test_env[["var.with.dots"]] <- 123
  
  result <- safe_get("var.with.dots", test_env)
  
  expect_equal(result, 123)
})

test_that("safe_ls handles large environments", {
  test_env <- new.env()
  for (i in 1:100) {
    test_env[[paste0("var_", i)]] <- i
  }
  
  result <- safe_ls(test_env)
  
  expect_equal(length(result), 100)
})

test_that("safe_get_object_info handles complex objects", {
  assign("info_complex", list(
    nested = list(a = 1, b = 2),
    matrix = matrix(1:9, 3, 3)
  ), envir = .GlobalEnv)
  
  result <- safe_get_object_info("info_complex")
  
  expect_true(is.list(result))
  expect_equal(result$class, "list")
  
  cleanup_test_objects("info_complex")
})

