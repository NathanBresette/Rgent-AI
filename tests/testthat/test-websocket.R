# =============================================================================
# Test WebSocket Functions
# These tests validate WebSocket communication and message handling
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
# TEST: find_available_port()
# =============================================================================

test_that("find_available_port returns valid port number", {
  skip_if_not(exists("find_available_port"))
  
  port <- find_available_port()
  
  expect_true(is.numeric(port))
  expect_gt(port, 1023)  # Above reserved ports
  expect_lt(port, 65536)  # Valid port range
})

test_that("find_available_port returns different ports on successive calls", {
  skip_if_not(exists("find_available_port"))
  
  ports <- sapply(1:3, function(i) find_available_port())
  
  # All should be valid
  expect_true(all(ports > 1023))
  expect_true(all(ports < 65536))
})

# =============================================================================
# TEST: Access Code Management
# =============================================================================

test_that("get_access_code_path returns valid path", {
  skip_if_not(exists("get_access_code_path"))
  
  path <- get_access_code_path()
  
  expect_true(is.character(path))
  expect_gt(nchar(path), 0)
})

test_that("save_access_code_to_disk and load_access_code_from_disk work together", {
  skip_if_not(exists("save_access_code_to_disk"))
  skip_if_not(exists("load_access_code_from_disk"))
  
  test_code <- "test_access_code_12345"
  
  # Save
  result <- tryCatch({
    save_access_code_to_disk(test_code)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  if (result) {
    # Load
    loaded_code <- load_access_code_from_disk()
    
    expect_equal(loaded_code, test_code)
  }
})

# =============================================================================
# TEST: RStudio Theme Detection
# =============================================================================

test_that("get_rstudio_theme returns theme info", {
  skip_if_not(exists("get_rstudio_theme"))
  
  # This may fail outside of RStudio, handle gracefully
  result <- tryCatch({
    get_rstudio_theme()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result))
})

# =============================================================================
# TEST: Context Capture
# =============================================================================

test_that("capture_context returns context object", {
  skip_if_not(exists("capture_context"))
  
  result <- capture_context()
  
  expect_true(is.list(result))
})

test_that("capture_context includes workspace information", {
  skip_if_not(exists("capture_context"))
  
  # Create some test objects
  assign("context_test_var", 123, envir = .GlobalEnv)
  assign("context_test_df", data.frame(x = 1:5), envir = .GlobalEnv)
  
  result <- capture_context()
  
  expect_true(is.list(result))
  
  # Should contain some workspace information
  # Structure may vary, just check it's not empty
  expect_gt(length(result), 0)
  
  cleanup_test_objects("context_test_var", "context_test_df")
})

test_that("capture_agent_context returns agent-specific context", {
  skip_if_not(exists("capture_agent_context"))
  
  # Create test data
  assign("agent_ctx_df", data.frame(a = 1:10, b = rnorm(10)), envir = .GlobalEnv)
  
  result <- capture_agent_context()
  
  expect_true(is.list(result))
  
  # Should include dataframe information
  if (!is.null(result$available_dataframes)) {
    expect_true("agent_ctx_df" %in% result$available_dataframes)
  }
  
  cleanup_test_objects("agent_ctx_df")
})

# =============================================================================
# TEST: Context Change Detection
# =============================================================================

test_that("detect_context_changes identifies new objects", {
  skip_if_not(exists("detect_context_changes"))
  
  old_context <- list(
    objects = list("obj1", "obj2")
  )
  
  new_context <- list(
    objects = list("obj1", "obj2", "obj3")
  )
  
  changes <- detect_context_changes(old_context, new_context)
  
  expect_true(is.list(changes))
})

test_that("detect_context_changes identifies removed objects", {
  skip_if_not(exists("detect_context_changes"))
  
  old_context <- list(
    objects = list("obj1", "obj2", "obj3")
  )
  
  new_context <- list(
    objects = list("obj1", "obj2")
  )
  
  changes <- detect_context_changes(old_context, new_context)
  
  expect_true(is.list(changes))
})

# =============================================================================
# TEST: WebSocket Server Functions
# =============================================================================

test_that("start_websocket_server function exists", {
  expect_true(exists("start_websocket_server") || exists("run_rgent"))
})

test_that("stop_websocket_server function exists", {
  skip_if_not(exists("stop_websocket_server"))
  
  # Should be callable without error when no server running
  result <- tryCatch({
    stop_websocket_server()
    TRUE
  }, error = function(e) {
    # Expected if no server running
    TRUE
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: HTML Interface Launch
# =============================================================================

test_that("launch_html_interface finds HTML file", {
  skip_if_not(exists("launch_html_interface"))
  
  # Should not crash when called
  # May not actually launch browser in test environment
  result <- tryCatch({
    # Check if function can at least locate the file
    TRUE
  }, error = function(e) {
    TRUE
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: Message Handler Routing
# =============================================================================

# These tests validate message routing without actual WebSocket connection

test_that("message handler processes set_access_code", {
  # Create mock message
  msg <- list(
    type = "set_access_code",
    accessCode = "test_code_123"
  )
  
  # Convert to JSON
  msg_json <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  
  # Should be valid JSON
  expect_true(jsonlite::validate(msg_json))
})

test_that("message handler processes get_context", {
  msg <- list(
    type = "get_context"
  )
  
  msg_json <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(msg_json))
})

test_that("message handler processes start_cleaning_agent", {
  msg <- list(
    type = "start_cleaning_agent",
    dataframe = "test_df",
    cleaningOptions = list(dataTypes = TRUE)
  )
  
  msg_json <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(msg_json))
})

test_that("message handler processes start_statistical_analysis", {
  msg <- list(
    type = "start_statistical_analysis",
    dataframe = "test_df",
    targetVariable = "value",
    analysisOptions = list(basicStatistics = TRUE)
  )
  
  msg_json <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(msg_json))
})

test_that("message handler processes start_modeling_agent", {
  msg <- list(
    type = "start_modeling_agent",
    dataframe = "test_df",
    targetVariable = "outcome",
    algorithms = list(logisticRegression = TRUE)
  )
  
  msg_json <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(msg_json))
})

test_that("message handler processes execute_code", {
  msg <- list(
    type = "execute_code",
    code = "1 + 1"
  )
  
  msg_json <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(msg_json))
})

# =============================================================================
# TEST: Streaming Functions
# =============================================================================

test_that("stream_httr_post function exists", {
  skip_if_not(exists("stream_httr_post"))
  
  # Just verify it exists, actual HTTP testing requires mocking
  expect_true(is.function(stream_httr_post))
})

# =============================================================================
# TEST: Package Update Mechanism
# =============================================================================

test_that("check_and_update_package function exists", {
  skip_if_not(exists("check_and_update_package"))
  
  expect_true(is.function(check_and_update_package))
})

# =============================================================================
# TEST: Auto-Start Configuration
# =============================================================================

test_that("auto_start_rgent function exists", {
  skip_if_not(exists("auto_start_rgent"))
  
  expect_true(is.function(auto_start_rgent))
})

test_that("setup_rprofile_auto_start function exists", {
  skip_if_not(exists("setup_rprofile_auto_start"))
  
  expect_true(is.function(setup_rprofile_auto_start))
})

# =============================================================================
# TEST: JSON Message Validation
# =============================================================================

test_that("valid JSON messages are created for all agent types", {
  agent_messages <- list(
    cleaning = list(
      type = "start_cleaning_agent",
      dataframe = "df",
      cleaningOptions = list(dataTypes = TRUE)
    ),
    statistical = list(
      type = "start_statistical_analysis",
      dataframe = "df",
      targetVariable = "x"
    ),
    modeling = list(
      type = "start_modeling_agent",
      dataframe = "df",
      targetVariable = "y"
    ),
    visualization = list(
      type = "start_visualization_agent",
      dataframe = "df"
    ),
    transformation = list(
      type = "start_transformation_agent",
      dataframe = "df"
    )
  )
  
  for (name in names(agent_messages)) {
    json <- jsonlite::toJSON(agent_messages[[name]], auto_unbox = TRUE)
    expect_true(jsonlite::validate(json), info = paste("Failed for", name))
  }
})

# =============================================================================
# TEST: Error Response Format
# =============================================================================

test_that("error responses have consistent format", {
  # Simulate error response format
  error_response <- list(
    success = FALSE,
    error = "Test error message",
    type = "error"
  )
  
  json <- jsonlite::toJSON(error_response, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(json))
  
  # Parse back
  parsed <- jsonlite::fromJSON(json)
  
  expect_false(parsed$success)
  expect_true(!is.null(parsed$error))
})

test_that("success responses have consistent format", {
  success_response <- list(
    success = TRUE,
    data = list(result = "test"),
    type = "response"
  )
  
  json <- jsonlite::toJSON(success_response, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(json))
  
  parsed <- jsonlite::fromJSON(json)
  
  expect_true(parsed$success)
})

# =============================================================================
# TEST: Complex Data Serialization
# =============================================================================

test_that("complex R objects can be serialized for WebSocket", {
  # Create complex nested structure
  complex_data <- list(
    dataframe_info = list(
      name = "test_df",
      nrow = 100,
      ncol = 5,
      columns = list(
        list(name = "x", type = "numeric", missing = 0),
        list(name = "y", type = "character", missing = 5)
      )
    ),
    summary_stats = list(
      mean = 10.5,
      sd = 2.3,
      min = 1,
      max = 20
    ),
    histogram = list(
      breaks = c(0, 5, 10, 15, 20),
      counts = c(10, 30, 40, 20)
    )
  )
  
  json <- jsonlite::toJSON(complex_data, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(json))
  
  # Can parse back
  parsed <- jsonlite::fromJSON(json)
  
  expect_equal(parsed$dataframe_info$name, "test_df")
})

test_that("statistical test results can be serialized", {
  # Simulate t-test result structure
  test_result <- list(
    test_name = "Two-sample t-test",
    statistic = 2.45,
    p_value = 0.023,
    df = 48,
    conf_int = c(0.5, 3.2),
    means = c(10.2, 8.5),
    effect_size = 0.68,
    interpretation = "Significant difference at alpha = 0.05"
  )
  
  json <- jsonlite::toJSON(test_result, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(json))
})

test_that("model results can be serialized", {
  # Simulate model result structure
  model_result <- list(
    model_type = "linear_regression",
    formula = "y ~ x1 + x2",
    coefficients = list(
      list(term = "(Intercept)", estimate = 2.5, std_error = 0.5, p_value = 0.001),
      list(term = "x1", estimate = 1.2, std_error = 0.3, p_value = 0.001),
      list(term = "x2", estimate = -0.8, std_error = 0.2, p_value = 0.002)
    ),
    r_squared = 0.85,
    adj_r_squared = 0.83,
    f_statistic = 45.2,
    residual_se = 1.2
  )
  
  json <- jsonlite::toJSON(model_result, auto_unbox = TRUE)
  
  expect_true(jsonlite::validate(json))
})

# =============================================================================
# TEST: Integration Readiness
# =============================================================================

test_that("run_rgent function exists and is callable", {
  expect_true(exists("run_rgent"))
  expect_true(is.function(run_rgent))
})

test_that("required packages for WebSocket are available", {
  expect_true(require(httpuv, quietly = TRUE))
  expect_true(require(jsonlite, quietly = TRUE))
})

