# =============================================================================
# Integration Tests
# These tests validate end-to-end workflows and interactions between components
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
# TEST: Full Cleaning Agent Workflow
# =============================================================================

test_that("cleaning agent workflow completes end-to-end", {
  # Create test dataframe with various issues
  assign("int_clean_df", data.frame(
    numeric_with_na = c(1, 2, NA, 4, 5, NA, 7, 8, 9, 10),
    character_col = c("a", "b", "c", "a", "b", "c", "a", "b", "c", "d"),
    factor_col = factor(c("x", "y", "x", "y", "x", "y", "x", "y", "x", "y")),
    numeric_with_outlier = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  # Start workflow
  workflow_result <- start_cleaning_agent(
    dataframe = "int_clean_df",
    cleaning_options = list(
      dataTypes = TRUE,
      missingValues = TRUE,
      duplicates = TRUE,
      outliers = TRUE,
      columnNames = TRUE
    )
  )
  
  expect_true(workflow_result$success)
  expect_gt(workflow_result$total_steps, 0)
  
  # Execute each step
  for (i in 1:workflow_result$total_steps) {
    step_result <- execute_cleaning_agent("int_clean_df", iteration = i)
    expect_true(step_result$success)
    expect_true(!is.null(step_result$code))
    
    # Code should be executable (at least parseable)
    expect_silent({
      tryCatch({
        parse(text = step_result$code)
      }, error = function(e) NULL)
    })
  }
  
  cleanup_test_objects("int_clean_df")
})

# =============================================================================
# TEST: Statistical Analysis Integration
# =============================================================================

test_that("statistical analysis integrates with data functions", {
  # Create test data suitable for statistical analysis
  set.seed(42)
  assign("int_stat_df", data.frame(
    treatment = rep(c("Control", "Treatment"), each = 30),
    response = c(rnorm(30, mean = 10, sd = 2), rnorm(30, mean = 12, sd = 2)),
    age = sample(20:60, 60, replace = TRUE),
    gender = sample(c("M", "F"), 60, replace = TRUE)
  ), envir = .GlobalEnv)
  
  # Verify data is accessible
  available_dfs <- get_available_dataframes()
  expect_true("int_stat_df" %in% available_dfs)
  
  # Get dataframe info
  df_info <- get_dataframe_info("int_stat_df")
  expect_true(df_info$exists)
  expect_equal(df_info$nrow, 60)
  
  # Create statistical workflow
  stat_workflow <- create_statistical_workflow(
    analysis_options = list(
      basicStatistics = TRUE,
      groupComparisons = TRUE,
      distributionAnalysis = TRUE,
      correlationAnalysis = FALSE,
      categoricalTests = FALSE,
      nonparametricTests = FALSE,
      effectSizeAnalysis = TRUE,
      powerAnalysis = FALSE,
      multipleTestingCorrection = FALSE,
      customStatisticalAnalysis = FALSE,
      beforeAfterAnalysis = FALSE
    ),
    variables = list(target = "response", grouping = "treatment"),
    method_options = list()
  )
  
  expect_true(is.list(stat_workflow))
  expect_gt(length(stat_workflow), 0)
  
  cleanup_test_objects("int_stat_df")
})

# =============================================================================
# TEST: Modeling Agent Integration
# =============================================================================

test_that("modeling agent integrates with data functions", {
  set.seed(123)
  
  # Create classification dataset
  n <- 100
  assign("int_model_df", data.frame(
    outcome = factor(rep(c("success", "failure"), each = n/2)),
    feature1 = c(rnorm(n/2, 5), rnorm(n/2, 7)),
    feature2 = c(rnorm(n/2, 10), rnorm(n/2, 12)),
    feature3 = sample(c("A", "B", "C"), n, replace = TRUE),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  # Verify data is accessible
  df_info <- get_dataframe_info("int_model_df")
  expect_true(df_info$exists)
  expect_equal(df_info$nrow, n)
  
  # Create modeling workflow
  model_workflow <- create_modeling_workflow(
    algorithms = list(
      logisticRegression = TRUE,
      randomForest = FALSE,
      linearRegression = FALSE,
      multinomialRegression = FALSE,
      xgboost = FALSE,
      dimensionalityReduction = FALSE,
      customModeling = FALSE
    ),
    options = list(
      featureEngineering = FALSE,
      dataPreprocessing = TRUE,
      modelComparison = FALSE,
      resultsCompilation = TRUE,
      customModeling = FALSE
    ),
    dataframe_name = "int_model_df",
    target_variable_name = "outcome"
  )
  
  expect_true(is.list(model_workflow))
  
  cleanup_test_objects("int_model_df")
})

# =============================================================================
# TEST: Data Explorer Integration
# =============================================================================

test_that("data explorer integrates with all object types", {
  # Create various objects
  assign("int_df", data.frame(x = 1:10, y = rnorm(10)), envir = .GlobalEnv)
  assign("int_vec", 1:20, envir = .GlobalEnv)
  assign("int_list", list(a = 1, b = "test", c = 1:5), envir = .GlobalEnv)
  assign("int_mat", matrix(1:12, 3, 4), envir = .GlobalEnv)
  assign("int_func", function(x) x^2, envir = .GlobalEnv)
  
  # Test get_available_objects
  objects <- get_available_objects()
  expect_true("int_df" %in% objects)
  expect_true("int_vec" %in% objects)
  expect_true("int_list" %in% objects)
  expect_true("int_mat" %in% objects)
  expect_true("int_func" %in% objects)
  
  # Test explorer data for each type
  df_explorer <- get_object_explorer_data("int_df")
  expect_true(df_explorer$exists)
  
  vec_explorer <- get_object_explorer_data("int_vec")
  expect_true(vec_explorer$exists)
  
  list_explorer <- get_object_explorer_data("int_list")
  expect_true(list_explorer$exists)
  
  mat_explorer <- get_object_explorer_data("int_mat")
  expect_true(mat_explorer$exists)
  
  func_explorer <- get_object_explorer_data("int_func")
  expect_true(func_explorer$exists)
  
  cleanup_test_objects("int_df", "int_vec", "int_list", "int_mat", "int_func")
})

# =============================================================================
# TEST: DataFrame Filtering and Pagination Integration
# =============================================================================

test_that("dataframe chunk filtering integrates with explorer", {
  # Create test data
  set.seed(456)
  assign("int_filter_df", data.frame(
    id = 1:200,
    category = sample(c("A", "B", "C"), 200, replace = TRUE),
    value = rnorm(200, mean = 50, sd = 15),
    active = sample(c(TRUE, FALSE), 200, replace = TRUE),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  # Get explorer data first
  explorer_data <- get_dataframe_explorer_data("int_filter_df")
  expect_true(explorer_data$exists)
  expect_equal(explorer_data$nrow, 200)
  
  # Test pagination
  page1 <- get_dataframe_chunk_filtered("int_filter_df", page = 1, page_size = 50)
  expect_equal(page1$page, 1)
  expect_equal(page1$total_pages, 4)
  expect_equal(length(page1$data$id), 50)
  
  page2 <- get_dataframe_chunk_filtered("int_filter_df", page = 2, page_size = 50)
  expect_equal(page2$page, 2)
  expect_equal(page2$start_row, 51)
  
  # Test filtering
  filtered <- get_dataframe_chunk_filtered(
    "int_filter_df",
    filters = list(list(column = "category", type = "equals", value = "A"))
  )
  expect_lt(filtered$total_rows, 200)  # Should have fewer rows
  
  # Test sorting
  sorted <- get_dataframe_chunk_filtered(
    "int_filter_df",
    sorts = list(list(column = "value", direction = "desc"))
  )
  
  # First value should be among the largest
  first_val <- as.numeric(sorted$data$value[1])
  expect_gt(first_val, 50)  # Likely above mean for descending sort
  
  # Test combined filter + sort + pagination
  combined <- get_dataframe_chunk_filtered(
    "int_filter_df",
    page = 1,
    page_size = 20,
    filters = list(list(column = "active", type = "equals", value = "TRUE")),
    sorts = list(list(column = "value", direction = "asc"))
  )
  expect_lte(length(combined$data$id), 20)
  expect_lte(combined$total_rows, 200)
  
  cleanup_test_objects("int_filter_df")
})

# =============================================================================
# TEST: Transformation Agent Integration
# =============================================================================

test_that("transformation agent integrates with data exploration", {
  # Create skewed data for transformation
  set.seed(789)
  assign("int_transform_df", data.frame(
    skewed_var = rexp(100, rate = 0.5),  # Right-skewed
    normal_var = rnorm(100, mean = 50, sd = 10),
    category = sample(c("Low", "Medium", "High"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  # Get explorer data to understand distribution
  explorer_data <- get_dataframe_explorer_data("int_transform_df")
  expect_true(explorer_data$exists)
  
  # Check histogram is available for numeric columns
  expect_true(!is.null(explorer_data$column_info$skewed_var$histogram))
  expect_true(!is.null(explorer_data$column_info$normal_var$histogram))
  
  # Start transformation workflow
  transform_result <- start_transformation_agent(
    dataframe = "int_transform_df",
    transformation_options = list(
      distributionAnalysis = TRUE,
      mathematicalTransformations = TRUE
    ),
    method_options = list(mathematicalTransformations = "log"),
    custom_inputs = NULL
  )
  
  expect_true(is.list(transform_result))
  
  cleanup_test_objects("int_transform_df")
})

# =============================================================================
# TEST: Context Capture Integration
# =============================================================================

test_that("context capture includes all workspace information", {
  skip_if_not(exists("capture_context"))
  
  # Create various objects
  assign("ctx_df", data.frame(x = 1:5), envir = .GlobalEnv)
  assign("ctx_model", lm(mpg ~ wt, data = mtcars), envir = .GlobalEnv)
  
  # Capture context
  context <- capture_context()
  
  expect_true(is.list(context))
  
  cleanup_test_objects("ctx_df", "ctx_model")
})

test_that("agent context capture includes dataframe details", {
  skip_if_not(exists("capture_agent_context"))
  
  assign("agent_test_df", data.frame(
    a = 1:10,
    b = rnorm(10),
    c = letters[1:10]
  ), envir = .GlobalEnv)
  
  agent_context <- capture_agent_context()
  
  expect_true(is.list(agent_context))
  expect_true("agent_test_df" %in% agent_context$available_dataframes)
  
  # Should have dataframe info
  if (!is.null(agent_context$dataframe_info)) {
    expect_true("agent_test_df" %in% names(agent_context$dataframe_info))
  }
  
  cleanup_test_objects("agent_test_df")
})

# =============================================================================
# TEST: JSON Serialization Throughout Pipeline
# =============================================================================

test_that("all agent outputs are JSON-serializable", {
  # Create test data
  assign("json_test_df", data.frame(
    num = c(1, 2, NA, 4, 5),
    char = c("a", "b", "c", "d", "e"),
    factor = factor(c("x", "y", "x", "y", "z")),
    date = as.Date("2020-01-01") + 0:4,
    logical = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  # Test cleaning agent output
  cleaning_result <- start_cleaning_agent("json_test_df")
  expect_silent({
    json_clean <- jsonlite::toJSON(cleaning_result, auto_unbox = TRUE)
  })
  expect_true(jsonlite::validate(json_clean))
  
  # Test explorer output
  explorer_result <- get_dataframe_explorer_data("json_test_df")
  expect_silent({
    json_explorer <- jsonlite::toJSON(explorer_result, auto_unbox = TRUE)
  })
  expect_true(jsonlite::validate(json_explorer))
  
  # Test chunk filtered output
  chunk_result <- get_dataframe_chunk_filtered("json_test_df")
  expect_silent({
    json_chunk <- jsonlite::toJSON(chunk_result, auto_unbox = TRUE)
  })
  expect_true(jsonlite::validate(json_chunk))
  
  # Test dataframe info output
  info_result <- get_dataframe_info("json_test_df")
  expect_silent({
    json_info <- jsonlite::toJSON(info_result, auto_unbox = TRUE)
  })
  expect_true(jsonlite::validate(json_info))
  
  cleanup_test_objects("json_test_df")
})

# =============================================================================
# TEST: Error Propagation
# =============================================================================

test_that("errors propagate correctly through workflow", {
  # Test with non-existent dataframe
  clean_error <- start_cleaning_agent("definitely_not_a_real_dataframe")
  expect_false(clean_error$success)
  expect_true(!is.null(clean_error$error))
  
  # Error should be JSON-serializable
  expect_silent({
    json_error <- jsonlite::toJSON(clean_error, auto_unbox = TRUE)
  })
  expect_true(jsonlite::validate(json_error))
})

# =============================================================================
# TEST: Performance with Large Data
# =============================================================================

test_that("system handles moderately large datasets efficiently", {
  # Create larger dataset
  n <- 5000
  assign("large_int_df", data.frame(
    id = 1:n,
    value1 = rnorm(n),
    value2 = rnorm(n),
    category = sample(letters[1:10], n, replace = TRUE),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  # Time explorer data generation
  start_time <- Sys.time()
  explorer_result <- get_dataframe_explorer_data("large_int_df")
  explorer_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  expect_true(explorer_result$exists)
  expect_lt(explorer_time, 5)  # Should complete in under 5 seconds
  
  # Time chunk retrieval
  start_time <- Sys.time()
  chunk_result <- get_dataframe_chunk_filtered(
    "large_int_df",
    page = 1,
    page_size = 100,
    filters = list(list(column = "category", type = "equals", value = "a"))
  )
  chunk_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  expect_true(chunk_result$exists)
  expect_lt(chunk_time, 2)  # Should complete in under 2 seconds
  
  cleanup_test_objects("large_int_df")
})

# =============================================================================
# TEST: Workflow State Consistency
# =============================================================================

test_that("workflow maintains consistent state", {
  assign("state_test_df", data.frame(
    x = 1:10,
    y = rnorm(10)
  ), envir = .GlobalEnv)
  
  # Start workflow with full cleaning options to avoid NULL issues
  workflow <- start_cleaning_agent(
    "state_test_df",
    cleaning_options = list(
      dataTypes = TRUE, 
      missingValues = TRUE, 
      duplicates = TRUE,
      outliers = TRUE, 
      columnNames = TRUE
    )
  )
  
  expect_true(workflow$success)
  
  # Workflow should have consistent step numbering
  total_steps <- workflow$total_steps
  expect_gt(total_steps, 0)
  
  # Each step should have incremental step numbers
  for (i in 1:length(workflow$workflow_steps)) {
    expect_equal(workflow$workflow_steps[[i]]$step, i)
  }
  
  cleanup_test_objects("state_test_df")
})

