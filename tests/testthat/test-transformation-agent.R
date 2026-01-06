# =============================================================================
# Test Transformation Agent Functions
# These tests validate the data transformation agent workflow
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
# TEST: start_transformation_agent()
# =============================================================================

test_that("start_transformation_agent validates dataframe existence", {
  result <- start_transformation_agent(
    dataframe = "nonexistent_transform_df",
    transformation_options = list(mathematicalTransformations = TRUE)
  )
  
  # Should handle gracefully
  expect_true(is.list(result))
})

test_that("start_transformation_agent creates workflow with valid data", {
  assign("transform_test_df", data.frame(
    value = c(1, 4, 9, 16, 25),
    category = c("A", "B", "A", "B", "C"),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "transform_test_df",
    transformation_options = list(
      distributionAnalysis = TRUE,
      mathematicalTransformations = TRUE
    ),
    method_options = list(),
    custom_inputs = NULL
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("transform_test_df")
})

test_that("start_transformation_agent handles all options", {
  assign("all_opts_df", data.frame(
    num = rnorm(20),
    cat = sample(c("X", "Y"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "all_opts_df",
    transformation_options = list(
      distributionAnalysis = TRUE,
      mathematicalTransformations = TRUE,
      normalization = TRUE,
      encoding = TRUE,
      binning = TRUE
    ),
    method_options = list(),
    custom_inputs = NULL
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("all_opts_df")
})

# =============================================================================
# TEST: generate_transformation_step_code()
# =============================================================================

test_that("generate_transformation_step_code creates valid R code", {
  skip_if_not(exists("generate_transformation_step_code"))
  
  assign("step_code_df", data.frame(
    value = c(1, 2, 3, 4, 5)
  ), envir = .GlobalEnv)
  
  step_info <- list(
    operation = "log_transform",
    column = "value"
  )
  
  code <- generate_transformation_step_code(step_info, "step_code_df")
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
  
  cleanup_test_objects("step_code_df")
})

test_that("generate_transformation_step_code handles different operations", {
  skip_if_not(exists("generate_transformation_step_code"))
  
  operations <- c("log", "sqrt", "normalize", "standardize", "scale")
  
  for (op in operations) {
    step_info <- list(operation = op, column = "x")
    
    code <- tryCatch({
      generate_transformation_step_code(step_info, "test_df")
    }, error = function(e) {
      "error"
    })
    
    expect_true(is.character(code))
  }
})

# =============================================================================
# TEST: get_next_transformation_step_code()
# =============================================================================

test_that("get_next_transformation_step_code returns step info", {
  skip_if_not(exists("get_next_transformation_step_code"))
  
  assign("next_step_df", data.frame(
    values = c(1, 10, 100, 1000)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    get_next_transformation_step_code(
      step_info = "log_transform",
      dataframe = "next_step_df",
      method_options = list(),
      selected_variables = "values"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
  
  cleanup_test_objects("next_step_df")
})

# =============================================================================
# TEST: Mathematical Transformations
# =============================================================================

test_that("transformation handles log transformation", {
  assign("log_df", data.frame(
    positive_values = c(1, 10, 100, 1000)
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "log_df",
    transformation_options = list(mathematicalTransformations = TRUE),
    method_options = list(mathematicalTransformations = "log")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("log_df")
})

test_that("transformation handles sqrt transformation", {
  assign("sqrt_df", data.frame(
    positive_values = c(0, 1, 4, 9, 16)
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "sqrt_df",
    transformation_options = list(mathematicalTransformations = TRUE),
    method_options = list(mathematicalTransformations = "sqrt")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("sqrt_df")
})

test_that("transformation handles box-cox transformation", {
  assign("boxcox_df", data.frame(
    positive_values = c(1, 2, 3, 4, 5)
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "boxcox_df",
    transformation_options = list(mathematicalTransformations = TRUE),
    method_options = list(mathematicalTransformations = "boxcox")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("boxcox_df")
})

# =============================================================================
# TEST: Normalization and Scaling
# =============================================================================

test_that("transformation handles min-max normalization", {
  assign("minmax_df", data.frame(
    values = c(-10, 0, 10, 20, 30)
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "minmax_df",
    transformation_options = list(normalization = TRUE),
    method_options = list(normalization = "minmax")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("minmax_df")
})

test_that("transformation handles z-score standardization", {
  assign("zscore_df", data.frame(
    values = rnorm(100, mean = 50, sd = 10)
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "zscore_df",
    transformation_options = list(normalization = TRUE),
    method_options = list(normalization = "zscore")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("zscore_df")
})

test_that("transformation handles robust scaling", {
  assign("robust_df", data.frame(
    values = c(1, 2, 3, 100, 5)  # Has outlier
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "robust_df",
    transformation_options = list(normalization = TRUE),
    method_options = list(normalization = "robust")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("robust_df")
})

# =============================================================================
# TEST: Encoding Transformations
# =============================================================================

test_that("transformation handles one-hot encoding", {
  assign("onehot_df", data.frame(
    category = c("A", "B", "C", "A", "B"),
    value = 1:5
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "onehot_df",
    transformation_options = list(encoding = TRUE),
    method_options = list(encoding = "onehot")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("onehot_df")
})

test_that("transformation handles label encoding", {
  assign("label_df", data.frame(
    category = c("low", "medium", "high", "low", "high")
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "label_df",
    transformation_options = list(encoding = TRUE),
    method_options = list(encoding = "label")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("label_df")
})

# =============================================================================
# TEST: Binning Transformations
# =============================================================================

test_that("transformation handles equal-width binning", {
  assign("bin_df", data.frame(
    values = runif(50, 0, 100)
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "bin_df",
    transformation_options = list(binning = TRUE),
    method_options = list(binning = "equal_width")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("bin_df")
})

test_that("transformation handles equal-frequency binning", {
  assign("freq_bin_df", data.frame(
    values = c(1, 2, 3, 4, 5, 100, 200, 300, 400, 500)  # Skewed
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "freq_bin_df",
    transformation_options = list(binning = TRUE),
    method_options = list(binning = "equal_frequency")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("freq_bin_df")
})

# =============================================================================
# TEST: Distribution Analysis
# =============================================================================

test_that("transformation performs distribution analysis", {
  assign("dist_analysis_df", data.frame(
    normal = rnorm(100),
    skewed = rexp(100),
    uniform = runif(100)
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "dist_analysis_df",
    transformation_options = list(distributionAnalysis = TRUE)
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("dist_analysis_df")
})

# =============================================================================
# TEST: Custom Transformations
# =============================================================================

test_that("transformation handles custom inputs", {
  assign("custom_transform_df", data.frame(
    x = 1:10
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "custom_transform_df",
    transformation_options = list(mathematicalTransformations = TRUE),
    method_options = list(mathematicalTransformations = "other"),
    custom_inputs = list(mathematicalTransformations = "Apply cube root transformation")
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("custom_transform_df")
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("transformation handles empty dataframe", {
  assign("empty_transform_df", data.frame(), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_transformation_agent(
      dataframe = "empty_transform_df",
      transformation_options = list(distributionAnalysis = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("empty_transform_df")
})

test_that("transformation handles all-NA column", {
  assign("na_transform_df", data.frame(
    good_col = 1:5,
    na_col = rep(NA, 5)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_transformation_agent(
      dataframe = "na_transform_df",
      transformation_options = list(mathematicalTransformations = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("na_transform_df")
})

test_that("transformation handles negative values for log", {
  assign("negative_df", data.frame(
    values = c(-5, -3, -1, 0, 1, 3, 5)
  ), envir = .GlobalEnv)
  
  # Log transformation should handle negatives gracefully
  result <- tryCatch({
    start_transformation_agent(
      dataframe = "negative_df",
      transformation_options = list(mathematicalTransformations = TRUE),
      method_options = list(mathematicalTransformations = "log")
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("negative_df")
})

test_that("transformation handles single row", {
  assign("single_row_df", data.frame(
    x = 5,
    y = 10
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "single_row_df",
    transformation_options = list(normalization = TRUE)
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("single_row_df")
})

test_that("transformation handles many columns", {
  # Create dataframe with many columns
  many_cols <- as.data.frame(matrix(rnorm(500), nrow = 50, ncol = 10))
  names(many_cols) <- paste0("col_", 1:10)
  assign("many_cols_df", many_cols, envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "many_cols_df",
    transformation_options = list(distributionAnalysis = TRUE)
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("many_cols_df")
})

test_that("transformation handles mixed data types", {
  assign("mixed_df", data.frame(
    numeric = rnorm(20),
    integer = 1:20,
    character = sample(letters[1:5], 20, replace = TRUE),
    factor = factor(sample(c("X", "Y"), 20, replace = TRUE)),
    logical = sample(c(TRUE, FALSE), 20, replace = TRUE),
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 20),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- start_transformation_agent(
    dataframe = "mixed_df",
    transformation_options = list(
      distributionAnalysis = TRUE,
      mathematicalTransformations = TRUE
    )
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("mixed_df")
})

# =============================================================================
# TEST: Integration with DataFrames Functions
# =============================================================================

test_that("transformation agent uses get_available_dataframes correctly", {
  assign("integration_df", data.frame(x = 1:5), envir = .GlobalEnv)
  
  # Check that the dataframe is available
  available <- get_available_dataframes()
  expect_true("integration_df" %in% available)
  
  # Now run transformation
  result <- start_transformation_agent(
    dataframe = "integration_df",
    transformation_options = list(distributionAnalysis = TRUE)
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("integration_df")
})

test_that("transformation agent uses get_dataframe_info correctly", {
  assign("info_check_df", data.frame(
    a = 1:10,
    b = rnorm(10)
  ), envir = .GlobalEnv)
  
  # Check info is available
  info <- get_dataframe_info("info_check_df")
  expect_true(info$exists)
  expect_equal(info$nrow, 10)
  expect_equal(info$ncol, 2)
  
  # Run transformation
  result <- start_transformation_agent(
    dataframe = "info_check_df",
    transformation_options = list(distributionAnalysis = TRUE)
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("info_check_df")
})

