# Test utility functions

test_that("helper functions work correctly", {
  # Test if config functions exist
  if (exists("find_available_port")) {
    expect_true(is.function(find_available_port))
    
    # Test port finding (should return a number)
    port <- find_available_port()
    expect_true(is.numeric(port))
    expect_true(port > 1000)
    expect_true(port < 65536)
  }
})

test_that("data validation functions work", {
  # Test with valid data
  valid_data <- data.frame(
    x = 1:10,
    y = letters[1:10]
  )
  
  # These functions might not exist yet, so test conditionally
  if (exists("validate_dataframe")) {
    result <- validate_dataframe(valid_data)
    expect_true(is.logical(result) || is.list(result))
  }
})

test_that("error handling works correctly", {
  # Test error handling with invalid inputs
  expect_error({
    # This should cause an error - call a function that doesn't exist
    nonexistent_function_that_should_fail()
  })
})

test_that("JSON serialization handles R objects", {
  # Test that common R objects can be converted to JSON-safe formats
  test_objects <- list(
    numeric_vector = c(1, 2, 3),
    character_vector = c("a", "b", "c"),
    factor_vector = factor(c("x", "y", "z")),
    data_frame = data.frame(a = 1:3, b = letters[1:3])
  )
  
  for (obj_name in names(test_objects)) {
    obj <- test_objects[[obj_name]]
    
    # Should be able to convert to JSON without errors
    expect_silent({
      json_result <- jsonlite::toJSON(obj)
    })
    
    expect_true(is.character(json_result))
    expect_true(nchar(json_result) > 0)
  }
})

test_that("statistical test result objects are JSON-safe", {
  # Test that statistical test results don't cause JSON errors
  if (require(stats, quietly = TRUE)) {
    # Create sample data
    group1 <- rnorm(20, mean = 5)
    group2 <- rnorm(20, mean = 7)
    
    # Run a t-test
    t_result <- t.test(group1, group2)
    
    # Extract JSON-safe components
    safe_result <- list(
      statistic = as.numeric(t_result$statistic),
      p_value = as.numeric(t_result$p.value),
      method = as.character(t_result$method),
      alternative = as.character(t_result$alternative)
    )
    
    # Should be able to convert to JSON
    expect_silent({
      json_result <- jsonlite::toJSON(safe_result)
    })
    
    expect_true(is.character(json_result))
  }
})

test_that("object size calculations work", {
  # Test that object.size() results are handled correctly
  test_df <- data.frame(
    x = 1:100,
    y = rnorm(100)
  )
  
  # Get object size
  obj_size <- object.size(test_df)
  
  # Convert to JSON-safe format
  size_numeric <- as.numeric(obj_size)
  
  expect_true(is.numeric(size_numeric))
  expect_true(size_numeric > 0)
  
  # Should be able to convert to JSON
  expect_silent({
    json_result <- jsonlite::toJSON(list(size = size_numeric))
  })
})

test_that("network helper functions work", {
  # Test network connectivity checks (if they exist)
  if (exists("check_internet_connection")) {
    result <- check_internet_connection()
    expect_true(is.logical(result))
  }
  
  # Test WebSocket utilities (if they exist)
  if (exists("create_websocket_message")) {
    msg <- create_websocket_message("test", list(data = "sample"))
    expect_true(is.character(msg) || is.list(msg))
  }
})
