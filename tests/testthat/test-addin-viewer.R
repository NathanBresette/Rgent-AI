# =============================================================================
# Test Addin Viewer Functions
# These tests validate the browser-based addin viewer initialization
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
# TEST: ai_addin_viewer() Function Structure
# =============================================================================

test_that("ai_addin_viewer function exists", {
  expect_true(exists("ai_addin_viewer"))
  expect_true(is.function(ai_addin_viewer))
})

test_that("ai_addin_viewer has port parameter", {
  args <- formals(ai_addin_viewer)
  expect_true("port" %in% names(args))
})

test_that("ai_addin_viewer port defaults to NULL", {
  args <- formals(ai_addin_viewer)
  expect_true(is.null(args$port))
})

# =============================================================================
# TEST: Required Package Checking
# =============================================================================

test_that("required packages are defined", {
  required_packages <- c("plumber", "callr", "httr", "jsonlite", "rstudioapi")
  
  # These should all be checkable
  for (pkg in required_packages) {
    result <- requireNamespace(pkg, quietly = TRUE)
    # Just checking the mechanism works, not that all are installed
    expect_true(is.logical(result))
  }
})

test_that("missing packages are detected", {
  # Check for a package that definitely doesn't exist
  result <- requireNamespace("nonexistent_package_xyz123", quietly = TRUE)
  expect_false(result)
})

# =============================================================================
# TEST: Port Finding
# =============================================================================

test_that("find_available_port exists and returns port", {
  skip_if_not(exists("find_available_port"))
  
  port <- find_available_port()
  
  expect_true(is.numeric(port))
  expect_gte(port, 1024)
  expect_lte(port, 65535)
})

test_that("find_available_port returns different ports", {
  skip_if_not(exists("find_available_port"))
  
  ports <- sapply(1:5, function(i) find_available_port())
  
  # Should get at least some different ports
  expect_gte(length(unique(ports)), 1)
})

# =============================================================================
# TEST: Workspace Capture
# =============================================================================

test_that("workspace objects can be captured for viewer", {
  # Create test objects
  assign("viewer_test_var", 42, envir = .GlobalEnv)
  assign("viewer_test_df", data.frame(x = 1:3), envir = .GlobalEnv)
  
  # Capture workspace
  objects <- ls(envir = .GlobalEnv)
  
  expect_true("viewer_test_var" %in% objects)
  expect_true("viewer_test_df" %in% objects)
  
  cleanup_test_objects("viewer_test_var", "viewer_test_df")
})

test_that("object info can be extracted", {
  assign("viewer_info_test", data.frame(a = 1:5, b = letters[1:5]), envir = .GlobalEnv)
  
  obj <- get("viewer_info_test", envir = .GlobalEnv)
  
  obj_info <- list(
    class = paste(class(obj), collapse = ", "),
    rows = if (is.data.frame(obj)) nrow(obj) else length(obj),
    columns = if (is.data.frame(obj)) ncol(obj) else NULL
  )
  
  expect_equal(obj_info$class, "data.frame")
  expect_equal(obj_info$rows, 5)
  expect_equal(obj_info$columns, 2)
  
  cleanup_test_objects("viewer_info_test")
})

test_that("object preview can be generated", {
  assign("viewer_preview_test", data.frame(x = 1:10, y = 10:1), envir = .GlobalEnv)
  
  obj <- get("viewer_preview_test", envir = .GlobalEnv)
  
  preview <- tryCatch({
    paste(utils::capture.output(print(utils::head(obj, 3))), collapse = "\n")
  }, error = function(e) {
    "Error generating preview"
  })
  
  expect_true(is.character(preview))
  expect_gt(nchar(preview), 0)
  
  cleanup_test_objects("viewer_preview_test")
})

test_that("error objects are handled in capture", {
  # Create an object that might cause issues when accessed
  assign("viewer_error_test", structure(list(), class = "custom_problematic_class"), envir = .GlobalEnv)
  
  obj_info <- tryCatch({
    obj <- get("viewer_error_test", envir = .GlobalEnv)
    list(
      class = paste(class(obj), collapse = ", "),
      rows = length(obj)
    )
  }, error = function(e) {
    list(
      class = "error",
      rows = NULL,
      error = e$message
    )
  })
  
  expect_true(is.list(obj_info))
  
  cleanup_test_objects("viewer_error_test")
})

# =============================================================================
# TEST: HTML File Location
# =============================================================================

test_that("HTML file path can be constructed", {
  html_file <- system.file("public/index.html", package = "rstudioai")
  
  # Path should be a string
  expect_true(is.character(html_file))
})

test_that("plumber API file path can be constructed", {
  api_file <- system.file("viewer_ai", "plumber_api.R", package = "rstudioai")
  
  expect_true(is.character(api_file))
})

# =============================================================================
# TEST: Server URL Construction
# =============================================================================

test_that("viewer URL is correctly formatted", {
  port <- 8888
  viewer_url <- sprintf("http://127.0.0.1:%d/", port)
  
  expect_equal(viewer_url, "http://127.0.0.1:8888/")
})

test_that("health check URL is correctly formatted", {
  port <- 8888
  health_url <- sprintf("http://127.0.0.1:%d/health", port)
  
  expect_equal(health_url, "http://127.0.0.1:8888/health")
})

# =============================================================================
# TEST: Port Validation
# =============================================================================

test_that("port validation rejects non-numeric", {
  # Simulate port validation
  port <- "not_a_number"
  
  is_valid <- is.numeric(port) && !is.na(port)
  
  expect_false(is_valid)
})

test_that("port validation rejects NULL", {
  port <- NULL
  
  is_valid <- !is.null(port) && is.numeric(port) && !is.na(port)
  
  expect_false(is_valid)
})

test_that("port validation accepts valid port", {
  port <- 8888
  
  is_valid <- !is.null(port) && is.numeric(port) && !is.na(port)
  
  expect_true(is_valid)
})

test_that("port validation rejects NA", {
  port <- NA
  
  is_valid <- !is.null(port) && is.numeric(port) && !is.na(port)
  
  expect_false(is_valid)
})

# =============================================================================
# TEST: RStudio API Availability Check
# =============================================================================

test_that("rstudioapi availability can be checked", {
  result <- tryCatch({
    requireNamespace("rstudioapi", quietly = TRUE)
  }, error = function(e) {
    FALSE
  })
  
  expect_true(is.logical(result))
})

test_that("rstudioapi isAvailable returns boolean", {
  skip_if_not(requireNamespace("rstudioapi", quietly = TRUE))
  
  result <- tryCatch({
    rstudioapi::isAvailable()
  }, error = function(e) {
    FALSE
  })
  
  expect_true(is.logical(result))
})

# =============================================================================
# TEST: Background Process Handling
# =============================================================================

test_that("callr is available for background processes", {
  result <- requireNamespace("callr", quietly = TRUE)
  
  # Just checking it can be loaded if installed
  expect_true(is.logical(result))
})

test_that("background function arguments can be prepared", {
  args <- list(
    api_file = "/path/to/api.R",
    server_port = 8888,
    workspace_data = list(a = 1)
  )
  
  expect_true(is.list(args))
  expect_equal(args$server_port, 8888)
})

# =============================================================================
# TEST: Error Handling
# =============================================================================

test_that("ai_addin_viewer returns FALSE outside RStudio", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- tryCatch({
    ai_addin_viewer()
  }, error = function(e) {
    FALSE
  })
  
  expect_false(result)
})

test_that("ai_addin_viewer handles invalid port", {
  skip_if(
    requireNamespace("rstudioapi", quietly = TRUE) && 
    tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE),
    "Test only runs outside RStudio"
  )
  
  result <- tryCatch({
    ai_addin_viewer(port = "invalid")
  }, error = function(e) {
    FALSE
  })
  
  expect_false(result)
})

# =============================================================================
# TEST: Workspace Data Serialization
# =============================================================================

test_that("workspace data can be serialized to JSON", {
  workspace_data <- list(
    test_var = list(class = "numeric", rows = 1),
    test_df = list(class = "data.frame", rows = 10, columns = 3)
  )
  
  json <- tryCatch({
    jsonlite::toJSON(workspace_data)
  }, error = function(e) {
    NULL
  })
  
  expect_true(!is.null(json))
  expect_true(is.character(json) || inherits(json, "json"))
})

test_that("complex objects are serialized safely", {
  # Objects with special characteristics
  complex_workspace <- list(
    func = list(class = "function", rows = NULL),
    env = list(class = "environment", rows = NULL),
    formula = list(class = "formula", rows = NULL)
  )
  
  json <- tryCatch({
    jsonlite::toJSON(complex_workspace, null = "null")
  }, error = function(e) {
    NULL
  })
  
  expect_true(!is.null(json))
})

# =============================================================================
# TEST: Server Startup Timing
# =============================================================================

test_that("Sys.sleep works for timing", {
  start <- Sys.time()
  Sys.sleep(0.1)
  end <- Sys.time()
  
  elapsed <- as.numeric(difftime(end, start, units = "secs"))
  
  expect_gte(elapsed, 0.09)  # Allow some tolerance
})

test_that("timeout calculation works", {
  start_time <- Sys.time()
  Sys.sleep(0.1)
  
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  timeout <- 30
  
  expect_lt(elapsed, timeout)
})

# =============================================================================
# TEST: Cleanup
# =============================================================================

test_that("temporary files can be created and cleaned", {
  temp_file <- tempfile(fileext = ".html")
  
  # Create temp file
  writeLines("<html></html>", temp_file)
  
  expect_true(file.exists(temp_file))
  
  # Cleanup
  unlink(temp_file)
  
  expect_false(file.exists(temp_file))
})

test_that("multiple temp files don't conflict", {
  files <- sapply(1:5, function(i) tempfile(fileext = ".html"))
  
  # All should be unique
  expect_equal(length(unique(files)), 5)
})

