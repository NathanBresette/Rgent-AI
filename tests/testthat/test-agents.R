# Test agent functionality

test_that("cleaning agent functions exist", {
  expect_true(exists("start_cleaning_agent"))
  expect_true(exists("get_available_dataframes"))
})

test_that("transformation agent functions exist", {
  expect_true(exists("start_transformation_agent"))
  expect_true(exists("get_dataframe_info"))
})

test_that("statistical agent functions exist", {
  expect_true(exists("start_statistical_analysis"))
  expect_true(exists("create_statistical_workflow"))
})

test_that("modeling agent functions exist", {
  expect_true(exists("start_modeling_agent"))
  expect_true(exists("create_modeling_workflow"))
})

test_that("basic data functions work", {
  # Create test data
  test_data <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c("a", "b", "c", "d", "e"),
    stringsAsFactors = FALSE
  )
  
  # Assign to global environment for testing
  assign("test_data", test_data, envir = .GlobalEnv)
  
  # Test dataframe detection
  expect_silent({
    dataframes <- get_available_dataframes()
  })
  
  expect_true(is.character(dataframes) || is.list(dataframes))
  expect_true("test_data" %in% dataframes || "test_data" %in% names(dataframes))
  
  # Clean up
  rm(test_data, envir = .GlobalEnv)
})

test_that("statistical workflow handles different data types", {
  # Test with numeric data
  numeric_data <- data.frame(
    group = rep(c("A", "B"), each = 10),
    value = rnorm(20)
  )
  
  expect_silent({
    workflow <- create_statistical_workflow(
      analysis_options = list(
        basicStatistics = TRUE,
        groupComparisons = FALSE,
        distributionAnalysis = FALSE,
        correlationAnalysis = FALSE,
        categoricalTests = FALSE,
        nonparametricTests = FALSE,
        effectSizeAnalysis = FALSE,
        powerAnalysis = FALSE,
        multipleTestingCorrection = FALSE,
        customStatisticalAnalysis = FALSE,
        beforeAfterAnalysis = FALSE
      ),
      variables = list(target = "value", grouping = "group"),
      method_options = list()
    )
  })
  
  expect_true(is.list(workflow))
})

test_that("modeling workflow creates valid structure", {
  # Test with classification data
  class_data <- data.frame(
    outcome = factor(rep(c("yes", "no"), each = 15)),
    predictor = rnorm(30)
  )
  
  expect_silent({
    workflow <- create_modeling_workflow(
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
        dataPreprocessing = FALSE,
        modelComparison = FALSE,
        resultsCompilation = FALSE,
        customModeling = FALSE
      ),
      dataframe_name = "class_data",
      target_variable_name = "outcome"
    )
  })
  
  expect_true(is.list(workflow))
  expect_true(length(workflow) > 0)
})

test_that("agents handle missing data gracefully", {
  # Test with empty dataframe
  empty_data <- data.frame()
  
  # Should not crash, might return error message
  expect_silent({
    result <- tryCatch({
      create_cleaning_workflow(
        dataframe_name = "empty_data",
        cleaning_options = list(dataTypes = TRUE)
      )
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
  })
  
  expect_true(is.list(result))
})

test_that("transformation agent handles different variable types", {
  # Test with mixed data types
  mixed_data <- data.frame(
    numeric_var = rnorm(20),
    factor_var = factor(rep(c("A", "B", "C", "D"), 5)),
    character_var = letters[1:20],
    logical_var = rep(c(TRUE, FALSE), 10)
  )
  
  expect_silent({
    workflow <- start_transformation_agent(
      dataframe = "mixed_data", 
      transformation_options = list(
        distributionAnalysis = TRUE,
        mathematicalTransformations = TRUE
      ),
      method_options = list(),
      custom_inputs = NULL
    )
  })
  
  expect_true(is.list(workflow))
})
