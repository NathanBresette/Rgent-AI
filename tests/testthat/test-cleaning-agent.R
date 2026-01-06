# =============================================================================
# Test Cleaning Agent Functions
# These tests validate the data cleaning agent workflow and code generation
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
# TEST: start_cleaning_agent()
# =============================================================================

test_that("start_cleaning_agent validates dataframe existence", {
  result <- start_cleaning_agent("nonexistent_df_xyz123")
  
  expect_false(result$success)
  expect_true(grepl("not found", result$error, ignore.case = TRUE))
})

test_that("start_cleaning_agent creates valid workflow with default options", {
  assign("clean_test_df", data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c("a", "b", "c", "d", "e"),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- start_cleaning_agent("clean_test_df")
  
  expect_true(result$success)
  expect_true(!is.null(result$workflow_id))
  expect_true(!is.null(result$workflow_steps))
  expect_gt(result$total_steps, 0)
  expect_true(!is.null(result$current_step))
  expect_true(!is.null(result$current_step$code))
  
  cleanup_test_objects("clean_test_df")
})

test_that("start_cleaning_agent respects custom cleaning options", {
  assign("options_test_df", data.frame(x = 1:5), envir = .GlobalEnv)
  
  # Only enable data types analysis
  result <- start_cleaning_agent(
    "options_test_df",
    cleaning_options = list(
      dataTypes = TRUE,
      missingValues = FALSE,
      duplicates = FALSE,
      outliers = FALSE,
      columnNames = FALSE
    )
  )
  
  expect_true(result$success)
  expect_equal(result$total_steps, 1)
  expect_equal(result$workflow_steps[[1]]$operation, "data_types")
  
  cleanup_test_objects("options_test_df")
})

test_that("start_cleaning_agent handles all cleaning options enabled", {
  assign("all_options_df", data.frame(
    num = c(1, 2, NA, 100, 5),
    char = c("A", "A", "B", "B", "B"),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- start_cleaning_agent(
    "all_options_df",
    cleaning_options = list(
      dataTypes = TRUE,
      missingValues = TRUE,
      duplicates = TRUE,
      outliers = TRUE,
      columnNames = TRUE
    )
  )
  
  expect_true(result$success)
  expect_equal(result$total_steps, 5)
  
  # Verify each operation type is present
  operations <- sapply(result$workflow_steps, function(s) s$operation)
  expect_true("data_types" %in% operations)
  expect_true("missing_values" %in% operations)
  expect_true("duplicates" %in% operations)
  expect_true("outliers" %in% operations)
  expect_true("column_names" %in% operations)
  
  cleanup_test_objects("all_options_df")
})

test_that("start_cleaning_agent fails gracefully with no options selected", {
  assign("no_options_df", data.frame(x = 1:5), envir = .GlobalEnv)
  
  result <- start_cleaning_agent(
    "no_options_df",
    cleaning_options = list(
      dataTypes = FALSE,
      missingValues = FALSE,
      duplicates = FALSE,
      outliers = FALSE,
      columnNames = FALSE
    )
  )
  
  expect_false(result$success)
  expect_true(!is.null(result$error) || !is.null(result$message))  # Has some message
  
  cleanup_test_objects("no_options_df")
})

test_that("start_cleaning_agent handles custom operations", {
  assign("custom_ops_df", data.frame(x = 1:5), envir = .GlobalEnv)
  
  result <- start_cleaning_agent(
    "custom_ops_df",
    cleaning_options = list(
      dataTypes = FALSE,
      missingValues = FALSE,
      duplicates = FALSE,
      outliers = FALSE,
      columnNames = FALSE
    ),
    other_operations = "Remove all rows where x > 3"
  )
  
  expect_true(result$success)
  expect_equal(result$total_steps, 1)
  expect_equal(result$workflow_steps[[1]]$operation, "custom")
  expect_true(grepl("Remove all rows", result$workflow_steps[[1]]$custom_request))
  
  cleanup_test_objects("custom_ops_df")
})

# =============================================================================
# TEST: execute_cleaning_agent()
# =============================================================================

test_that("execute_cleaning_agent validates dataframe existence", {
  result <- execute_cleaning_agent("fake_dataframe_123")
  
  expect_false(result$success)
  expect_true(grepl("not found", result$error, ignore.case = TRUE))
})

test_that("execute_cleaning_agent returns sequential steps", {
  assign("exec_test_df", data.frame(
    x = c(1, 2, NA, 4, 5),
    y = rnorm(5)
  ), envir = .GlobalEnv)
  
  # Step 1
  result1 <- execute_cleaning_agent("exec_test_df", iteration = 1)
  expect_true(result1$success)
  expect_equal(result1$step, 1)
  expect_false(result1$completed)
  expect_true(!is.null(result1$code))
  
  # Step 2
  result2 <- execute_cleaning_agent("exec_test_df", iteration = 2)
  expect_equal(result2$step, 2)
  
  # Final step
  result5 <- execute_cleaning_agent("exec_test_df", iteration = 5)
  expect_true(result5$completed)
  
  cleanup_test_objects("exec_test_df")
})

test_that("execute_cleaning_agent handles completion", {
  assign("complete_df", data.frame(x = 1:3), envir = .GlobalEnv)
  
  # Request step beyond total steps
  result <- execute_cleaning_agent("complete_df", iteration = 10)
  
  expect_true(result$success)
  expect_true(result$completed)
  
  cleanup_test_objects("complete_df")
})

# =============================================================================
# TEST: generate_na_handling_code()
# =============================================================================

test_that("generate_na_handling_code skips when method is 'dont'", {
  assign("na_skip_df", data.frame(x = c(1, NA, 3)), envir = .GlobalEnv)
  df_info <- get_dataframe_info("na_skip_df")
  
  code <- generate_na_handling_code("na_skip_df", "dont", df_info)
  
  expect_true(grepl("No NA handling", code))
  
  cleanup_test_objects("na_skip_df")
})

test_that("generate_na_handling_code generates remove rows code", {
  assign("na_remove_df", data.frame(x = c(1, NA, 3)), envir = .GlobalEnv)
  df_info <- get_dataframe_info("na_remove_df")
  
  code <- generate_na_handling_code("na_remove_df", "remove", df_info)
  
  expect_true(grepl("complete.cases", code))
  
  cleanup_test_objects("na_remove_df")
})

test_that("generate_na_handling_code generates mean imputation code", {
  assign("na_mean_df", data.frame(
    num = c(1, NA, 3, 4, 5),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  df_info <- get_dataframe_info("na_mean_df")
  
  code <- generate_na_handling_code("na_mean_df", "mean", df_info)
  
  expect_true(grepl("mean", code))
  expect_true(grepl("na.rm = TRUE", code))
  
  cleanup_test_objects("na_mean_df")
})

test_that("generate_na_handling_code generates median imputation code", {
  assign("na_median_df", data.frame(
    num = c(1, NA, 3, 4, 5),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  df_info <- get_dataframe_info("na_median_df")
  
  code <- generate_na_handling_code("na_median_df", "median", df_info)
  
  expect_true(grepl("median", code))
  
  cleanup_test_objects("na_median_df")
})

test_that("generate_na_handling_code handles no missing values", {
  assign("na_none_df", data.frame(x = 1:5), envir = .GlobalEnv)
  df_info <- get_dataframe_info("na_none_df")
  
  code <- generate_na_handling_code("na_none_df", "mean", df_info)
  
  expect_true(grepl("No missing values", code))
  
  cleanup_test_objects("na_none_df")
})

# =============================================================================
# TEST: generate_type_conversion_code()
# =============================================================================

test_that("generate_type_conversion_code identifies conversion candidates", {
  assign("type_conv_df", data.frame(
    char_col = c("a", "b", "c", "d", "e"),  # Low unique, might suggest factor
    num_col = 1:5,
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  df_info <- get_dataframe_info("type_conv_df")
  
  code <- generate_type_conversion_code("type_conv_df", df_info)
  
  # Should return some code (either suggestion or "looks appropriate")
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
  
  cleanup_test_objects("type_conv_df")
})

# =============================================================================
# TEST: generate_outlier_removal_code()
# =============================================================================

test_that("generate_outlier_removal_code generates IQR-based code", {
  assign("outlier_df", data.frame(
    value = c(1, 2, 3, 100, 5)  # 100 is an outlier
  ), envir = .GlobalEnv)
  df_info <- get_dataframe_info("outlier_df")
  
  code <- generate_outlier_removal_code("outlier_df", df_info)
  
  expect_true(grepl("IQR", code))
  expect_true(grepl("quantile", code))
  
  cleanup_test_objects("outlier_df")
})

test_that("generate_outlier_removal_code handles no numeric columns", {
  assign("no_numeric_df", data.frame(
    a = c("x", "y", "z"),
    b = factor(c("a", "b", "c")),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  df_info <- get_dataframe_info("no_numeric_df")
  
  code <- generate_outlier_removal_code("no_numeric_df", df_info)
  
  expect_true(grepl("No numeric columns", code))
  
  cleanup_test_objects("no_numeric_df")
})

# =============================================================================
# TEST: generate_step_code()
# =============================================================================

test_that("generate_step_code generates correct code for data_types operation", {
  assign("step_df", data.frame(x = 1:5), envir = .GlobalEnv)
  
  step_info <- list(operation = "data_types")
  code <- generate_step_code(step_info, "step_df", "median", NULL, NULL)
  
  expect_true(grepl("str\\(step_df\\)", code))
  expect_true(grepl("Data Types", code))
  
  cleanup_test_objects("step_df")
})

test_that("generate_step_code generates correct code for duplicates operation", {
  assign("dup_step_df", data.frame(x = 1:5), envir = .GlobalEnv)
  
  step_info <- list(operation = "duplicates")
  code <- generate_step_code(step_info, "dup_step_df", "median", NULL, NULL)
  
  expect_true(grepl("duplicated", code))
  expect_true(grepl("Duplicate", code))
  
  cleanup_test_objects("dup_step_df")
})

test_that("generate_step_code handles different outlier methods", {
  assign("outlier_method_df", data.frame(x = 1:10), envir = .GlobalEnv)
  
  step_info <- list(operation = "outliers")
  
  # IQR method (default)
  code_iqr <- generate_step_code(
    step_info, "outlier_method_df", "median",
    method_options = list(outliers = "iqr"), NULL
  )
  expect_true(grepl("IQR", code_iqr))
  
  # Z-score method
  code_zscore <- generate_step_code(
    step_info, "outlier_method_df", "median",
    method_options = list(outliers = "zscore"), NULL
  )
  expect_true(grepl("Z-Score", code_zscore) || grepl("z-score", code_zscore))
  
  # Modified Z-score method
  code_mod_z <- generate_step_code(
    step_info, "outlier_method_df", "median",
    method_options = list(outliers = "modified_zscore"), NULL
  )
  expect_true(grepl("Modified Z-Score", code_mod_z) || grepl("MAD", code_mod_z))
  
  cleanup_test_objects("outlier_method_df")
})

test_that("generate_step_code handles different column naming conventions", {
  assign("naming_df", data.frame(x = 1:5), envir = .GlobalEnv)
  
  step_info <- list(operation = "column_names")
  
  # snake_case (default)
  code_snake <- generate_step_code(
    step_info, "naming_df", "median",
    method_options = list(columnNames = "snake_case"), NULL
  )
  expect_true(grepl("snake_case", code_snake))
  
  # camelCase
  code_camel <- generate_step_code(
    step_info, "naming_df", "median",
    method_options = list(columnNames = "camelCase"), NULL
  )
  expect_true(grepl("camelCase", code_camel))
  
  # PascalCase
  code_pascal <- generate_step_code(
    step_info, "naming_df", "median",
    method_options = list(columnNames = "PascalCase"), NULL
  )
  expect_true(grepl("PascalCase", code_pascal))
  
  cleanup_test_objects("naming_df")
})

test_that("generate_step_code handles custom inputs", {
  assign("custom_input_df", data.frame(x = 1:5), envir = .GlobalEnv)
  
  step_info <- list(operation = "outliers")
  
  code <- generate_step_code(
    step_info, "custom_input_df", "median",
    method_options = list(outliers = "other"),
    custom_inputs = list(outliers = "Use 2.5 sigma threshold")
  )
  
  expect_true(grepl("Custom", code))
  expect_true(grepl("2.5 sigma", code))
  
  cleanup_test_objects("custom_input_df")
})

# =============================================================================
# TEST: Selected Variables Validation
# =============================================================================

test_that("start_cleaning_agent validates selected variables", {
  assign("var_valid_df", data.frame(
    a = 1:5,
    b = letters[1:5],
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  # Invalid variable should cause error
  result <- start_cleaning_agent(
    "var_valid_df",
    selected_variables = list(
      outliers = list(list(name = "nonexistent_column", type = "numeric"))
    )
  )
  
  expect_false(result$success)
  expect_true(grepl("not found", result$error, ignore.case = TRUE))
  
  cleanup_test_objects("var_valid_df")
})

test_that("start_cleaning_agent accepts valid selected variables", {
  assign("valid_var_df", data.frame(
    numeric_col = 1:5,
    char_col = letters[1:5],
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- start_cleaning_agent(
    "valid_var_df",
    selected_variables = list(
      outliers = list(list(name = "numeric_col", type = "numeric"))
    )
  )
  
  expect_true(result$success)
  
  cleanup_test_objects("valid_var_df")
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("cleaning agent handles special characters in column names", {
  assign("special_col_df", data.frame(
    `column with spaces` = 1:5,
    `column.with.dots` = 1:5,
    check.names = FALSE
  ), envir = .GlobalEnv)
  
  result <- start_cleaning_agent("special_col_df")
  
  expect_true(result$success)
  
  cleanup_test_objects("special_col_df")
})

test_that("cleaning agent handles large dataframes efficiently", {
  # Create larger dataframe
  assign("large_df", data.frame(
    a = rnorm(1000),
    b = sample(letters, 1000, replace = TRUE),
    c = sample(1:100, 1000, replace = TRUE)
  ), envir = .GlobalEnv)
  
  # Should complete in reasonable time
  start_time <- Sys.time()
  result <- start_cleaning_agent("large_df")
  end_time <- Sys.time()
  
  expect_true(result$success)
  expect_lt(as.numeric(difftime(end_time, start_time, units = "secs")), 5)
  
  cleanup_test_objects("large_df")
})

test_that("cleaning agent handles all-NA columns", {
  assign("all_na_df", data.frame(
    normal = 1:5,
    all_na = rep(NA, 5)
  ), envir = .GlobalEnv)
  
  result <- start_cleaning_agent("all_na_df")
  
  expect_true(result$success)
  
  cleanup_test_objects("all_na_df")
})

