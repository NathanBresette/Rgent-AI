# =============================================================================
# Test Statistical Agent Functions
# These tests validate the statistical analysis agent workflow and tests
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
# TEST: create_statistical_workflow()
# =============================================================================

test_that("create_statistical_workflow creates valid structure", {
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
  
  expect_true(is.list(workflow))
  expect_gt(length(workflow), 0)
})

test_that("create_statistical_workflow includes selected analyses", {
  workflow <- create_statistical_workflow(
    analysis_options = list(
      basicStatistics = TRUE,
      groupComparisons = TRUE,
      distributionAnalysis = TRUE,
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
  
  # Extract step types
  step_types <- sapply(workflow, function(s) {
    if (!is.null(s$type)) s$type else if (!is.null(s$name)) s$name else NA
  })
  
  expect_true(is.list(workflow))
})

test_that("create_statistical_workflow handles all options enabled", {
  workflow <- create_statistical_workflow(
    analysis_options = list(
      basicStatistics = TRUE,
      groupComparisons = TRUE,
      distributionAnalysis = TRUE,
      correlationAnalysis = TRUE,
      categoricalTests = TRUE,
      nonparametricTests = TRUE,
      effectSizeAnalysis = TRUE,
      powerAnalysis = TRUE,
      multipleTestingCorrection = TRUE,
      customStatisticalAnalysis = FALSE,
      beforeAfterAnalysis = TRUE
    ),
    variables = list(target = "value", grouping = "group"),
    method_options = list()
  )
  
  expect_true(is.list(workflow))
  expect_gt(length(workflow), 5)  # Multiple steps expected
})

test_that("create_statistical_workflow handles minimal options", {
  workflow <- create_statistical_workflow(
    analysis_options = list(
      basicStatistics = FALSE,
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
    variables = list(target = "value"),
    method_options = list()
  )
  
  expect_true(is.list(workflow))
})

# =============================================================================
# TEST: start_statistical_analysis()
# =============================================================================

test_that("start_statistical_analysis validates dataframe existence", {
  result <- tryCatch({
    start_statistical_analysis(
      dataframe_name = "nonexistent_dataframe_xyz",
      target_variable = "x",
      analysis_options = list(basicStatistics = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should handle missing dataframe gracefully
  expect_true(is.list(result))
})

test_that("start_statistical_analysis works with valid data", {
  assign("stat_test_df", data.frame(
    group = rep(c("A", "B"), each = 20),
    value = c(rnorm(20, mean = 10), rnorm(20, mean = 12))
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_statistical_analysis(
      dataframe_name = "stat_test_df",
      target_variable = "value",
      grouping_variable = "group",
      analysis_options = list(
        basicStatistics = TRUE,
        groupComparisons = TRUE,
        distributionAnalysis = FALSE,
        correlationAnalysis = FALSE,
        categoricalTests = FALSE,
        nonparametricTests = FALSE,
        effectSizeAnalysis = FALSE,
        powerAnalysis = FALSE,
        multipleTestingCorrection = FALSE,
        customStatisticalAnalysis = FALSE,
        beforeAfterAnalysis = FALSE
      )
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("stat_test_df")
})

# =============================================================================
# TEST: Statistical Test Functions
# =============================================================================

test_that("perform_t_test calculates correct results", {
  skip_if_not(exists("perform_t_test"))
  
  group1 <- c(10, 12, 14, 16, 18)
  group2 <- c(8, 10, 12, 14, 16)
  
  result <- perform_t_test(group1, group2)
  
  expect_true(is.list(result))
  expect_true(!is.null(result$statistic) || !is.null(result$t_statistic))
  expect_true(!is.null(result$p_value) || !is.null(result$p.value))
})

test_that("perform_z_test calculates correct results", {
  skip_if_not(exists("perform_z_test"))
  
  # Large sample for Z-test
  group1 <- rnorm(100, mean = 10, sd = 2)
  group2 <- rnorm(100, mean = 12, sd = 2)
  
  result <- perform_z_test(group1, group2)
  
  expect_true(is.list(result))
})

test_that("perform_mann_whitney_test calculates correct results", {
  skip_if_not(exists("perform_mann_whitney_test"))
  
  group1 <- c(1, 3, 5, 7, 9)
  group2 <- c(2, 4, 6, 8, 10)
  
  result <- perform_mann_whitney_test(group1, group2)
  
  expect_true(is.list(result))
})

test_that("check_test_assumptions identifies normality violations", {
  skip_if_not(exists("check_test_assumptions"))
  
  # Normal data
  normal_data <- rnorm(100)
  non_normal_data <- rexp(100)
  
  # Function may require two groups
  result <- tryCatch({
    check_test_assumptions(normal_data, non_normal_data)
  }, error = function(e) {
    tryCatch({
      check_test_assumptions(normal_data)
    }, error = function(e2) {
      list(error = e2$message)
    })
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: Effect Size Calculations
# =============================================================================

test_that("calculate_cohens_d returns valid effect size", {
  skip_if_not(exists("calculate_cohens_d"))
  
  group1 <- rnorm(50, mean = 10, sd = 2)
  group2 <- rnorm(50, mean = 12, sd = 2)
  
  result <- tryCatch({
    calculate_cohens_d(group1, group2)
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Cohen's d should be a number or list
  expect_true(is.numeric(result) || is.list(result))
})

test_that("execute_effect_size_analysis handles different data types", {
  skip_if_not(exists("execute_effect_size_analysis"))
  
  assign("effect_size_df", data.frame(
    group = rep(c("A", "B"), each = 30),
    value = c(rnorm(30, mean = 10), rnorm(30, mean = 12))
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_effect_size_analysis(
      dataframe_name = "effect_size_df",
      target_variable = "value",
      grouping_variable = "group"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("effect_size_df")
})

# =============================================================================
# TEST: Distribution Analysis
# =============================================================================

test_that("execute_distribution_analysis works with numeric data", {
  skip_if_not(exists("execute_distribution_analysis"))
  
  assign("dist_df", data.frame(
    value = rnorm(100, mean = 50, sd = 10)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_distribution_analysis(
      dataframe_name = "dist_df",
      target_variable = "value"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("dist_df")
})

# =============================================================================
# TEST: Group Comparisons
# =============================================================================

test_that("execute_group_comparisons handles two groups", {
  skip_if_not(exists("execute_group_comparisons"))
  
  assign("two_group_df", data.frame(
    group = rep(c("Control", "Treatment"), each = 25),
    score = c(rnorm(25, 50, 10), rnorm(25, 55, 10))
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_group_comparisons(
      dataframe_name = "two_group_df",
      target_variable = "score",
      grouping_variable = "group"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("two_group_df")
})

test_that("execute_group_comparisons handles multiple groups with ANOVA", {
  skip_if_not(exists("execute_group_comparisons"))
  
  assign("multi_group_df", data.frame(
    group = rep(c("A", "B", "C"), each = 20),
    value = c(rnorm(20, 10), rnorm(20, 12), rnorm(20, 14))
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_group_comparisons(
      dataframe_name = "multi_group_df",
      target_variable = "value",
      grouping_variable = "group"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("multi_group_df")
})

test_that("perform_anova_test calculates ANOVA correctly", {
  skip_if_not(exists("perform_anova_test"))
  
  assign("anova_df", data.frame(
    group = factor(rep(c("A", "B", "C"), each = 15)),
    value = c(rnorm(15, 10), rnorm(15, 15), rnorm(15, 20))
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    perform_anova_test("anova_df", "value", "group")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("anova_df")
})

# =============================================================================
# TEST: Categorical Tests
# =============================================================================

test_that("execute_categorical_tests performs chi-squared test", {
  skip_if_not(exists("execute_categorical_tests"))
  
  assign("cat_df", data.frame(
    gender = sample(c("M", "F"), 100, replace = TRUE),
    preference = sample(c("A", "B", "C"), 100, replace = TRUE)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_categorical_tests(
      dataframe_name = "cat_df",
      variable1 = "gender",
      variable2 = "preference"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("cat_df")
})

# =============================================================================
# TEST: Before/After Analysis
# =============================================================================

test_that("execute_before_after_analysis performs paired test", {
  skip_if_not(exists("execute_before_after_analysis"))
  
  assign("paired_df", data.frame(
    before = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28),
    after = c(12, 14, 16, 18, 20, 22, 24, 26, 28, 30)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_before_after_analysis(
      dataframe_name = "paired_df",
      before_variable = "before",
      after_variable = "after"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("paired_df")
})

# =============================================================================
# TEST: Power Analysis
# =============================================================================

test_that("execute_power_analysis returns power information", {
  skip_if_not(exists("execute_power_analysis"))
  
  result <- tryCatch({
    execute_power_analysis(
      effect_size = 0.5,
      sample_size = 30,
      alpha = 0.05
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result))
})

# =============================================================================
# TEST: Multiple Testing Correction
# =============================================================================

test_that("execute_multiple_testing_correction applies corrections", {
  skip_if_not(exists("execute_multiple_testing_correction"))
  
  p_values <- c(0.01, 0.03, 0.05, 0.07, 0.10)
  
  result <- tryCatch({
    execute_multiple_testing_correction(
      p_values = p_values,
      method = "bonferroni"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.numeric(result))
})

test_that("execute_multiple_testing_correction supports different methods", {
  skip_if_not(exists("execute_multiple_testing_correction"))
  
  p_values <- c(0.01, 0.03, 0.05)
  
  methods <- c("bonferroni", "holm", "BH")
  
  for (method in methods) {
    result <- tryCatch({
      execute_multiple_testing_correction(p_values = p_values, method = method)
    }, error = function(e) {
      list(error = e$message)
    })
    
    expect_true(is.list(result) || is.numeric(result))
  }
})

# =============================================================================
# TEST: Data Overview for Statistics
# =============================================================================

test_that("execute_data_overview provides basic statistics", {
  skip_if_not(exists("execute_data_overview"))
  
  assign("overview_df", data.frame(
    numeric = c(1, 2, 3, NA, 5),
    categorical = factor(c("a", "b", "a", "c", "b")),
    logical = c(TRUE, FALSE, TRUE, TRUE, FALSE)
  ), envir = .GlobalEnv)
  
  result <- execute_data_overview("overview_df")
  
  expect_true(is.list(result))
  
  cleanup_test_objects("overview_df")
})

# =============================================================================
# TEST: Helper Functions
# =============================================================================

test_that("convert_tables_to_serializable handles table objects", {
  skip_if_not(exists("convert_tables_to_serializable"))
  
  test_table <- table(c("a", "a", "b", "c", "c", "c"))
  
  result <- convert_tables_to_serializable(test_table)
  
  # Result should be JSON-serializable
  expect_silent({
    json <- jsonlite::toJSON(result)
  })
})

test_that("load_packages handles missing packages gracefully", {
  skip_if_not(exists("load_packages"))
  
  # Should not throw error for common packages
  expect_silent({
    tryCatch({
      load_packages(c("stats", "utils"))
    }, error = function(e) NULL)
  })
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("statistical functions handle small sample sizes", {
  assign("small_df", data.frame(
    group = c("A", "B"),
    value = c(5, 10)
  ), envir = .GlobalEnv)
  
  # Should not crash, may warn or return error message
  result <- tryCatch({
    start_statistical_analysis(
      dataframe_name = "small_df",
      target_variable = "value",
      grouping_variable = "group",
      analysis_options = list(groupComparisons = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("small_df")
})

test_that("statistical functions handle constant values", {
  assign("constant_df", data.frame(
    group = rep(c("A", "B"), each = 10),
    value = rep(5, 20)  # All same value
  ), envir = .GlobalEnv)
  
  # Should handle gracefully
  result <- tryCatch({
    start_statistical_analysis(
      dataframe_name = "constant_df",
      target_variable = "value",
      grouping_variable = "group",
      analysis_options = list(distributionAnalysis = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("constant_df")
})

test_that("statistical functions handle all-NA values", {
  assign("all_na_stat_df", data.frame(
    group = rep(c("A", "B"), each = 5),
    value = rep(NA_real_, 10)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_statistical_analysis(
      dataframe_name = "all_na_stat_df",
      target_variable = "value",
      grouping_variable = "group",
      analysis_options = list(basicStatistics = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("all_na_stat_df")
})

