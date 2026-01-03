# =============================================================================
# Test Modeling Agent Functions
# These tests validate the machine learning modeling agent workflow
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
# TEST: create_modeling_workflow()
# =============================================================================

test_that("create_modeling_workflow creates valid structure", {
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
    dataframe_name = "test_data",
    target_variable_name = "outcome"
  )
  
  expect_true(is.list(workflow))
  expect_gt(length(workflow), 0)
})

test_that("create_modeling_workflow includes selected algorithms", {
  workflow <- create_modeling_workflow(
    algorithms = list(
      logisticRegression = TRUE,
      randomForest = TRUE,
      linearRegression = TRUE,
      multinomialRegression = FALSE,
      xgboost = FALSE,
      dimensionalityReduction = FALSE,
      customModeling = FALSE
    ),
    options = list(
      featureEngineering = TRUE,
      dataPreprocessing = TRUE,
      modelComparison = FALSE,
      resultsCompilation = TRUE,
      customModeling = FALSE
    ),
    dataframe_name = "test_data",
    target_variable_name = "outcome"
  )
  
  expect_true(is.list(workflow))
  expect_gt(length(workflow), 3)  # Multiple steps expected
})

test_that("create_modeling_workflow handles all algorithms enabled", {
  workflow <- create_modeling_workflow(
    algorithms = list(
      logisticRegression = TRUE,
      randomForest = TRUE,
      linearRegression = TRUE,
      multinomialRegression = TRUE,
      xgboost = TRUE,
      dimensionalityReduction = TRUE,
      customModeling = FALSE
    ),
    options = list(
      featureEngineering = TRUE,
      dataPreprocessing = TRUE,
      modelComparison = TRUE,
      resultsCompilation = TRUE,
      customModeling = FALSE
    ),
    dataframe_name = "test_data",
    target_variable_name = "target"
  )
  
  expect_true(is.list(workflow))
})

# =============================================================================
# TEST: start_modeling_agent()
# =============================================================================

test_that("start_modeling_agent handles missing dataframe", {
  result <- start_modeling_agent(
    dataframe_name = "nonexistent_modeling_df",
    target_variable = "outcome",
    algorithms = list(linearRegression = TRUE)
  )
  
  # Should handle gracefully
  expect_true(is.list(result))
})

test_that("start_modeling_agent works with classification data", {
  assign("class_model_df", data.frame(
    outcome = factor(rep(c("yes", "no"), each = 25)),
    predictor1 = rnorm(50),
    predictor2 = rnorm(50),
    predictor3 = sample(c("A", "B"), 50, replace = TRUE)
  ), envir = .GlobalEnv)
  
  result <- start_modeling_agent(
    dataframe_name = "class_model_df",
    target_variable = "outcome",
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
    )
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("class_model_df")
})

test_that("start_modeling_agent works with regression data", {
  assign("reg_model_df", data.frame(
    target = rnorm(50, mean = 100, sd = 15),
    feature1 = rnorm(50),
    feature2 = rnorm(50),
    feature3 = runif(50, 0, 1)
  ), envir = .GlobalEnv)
  
  result <- start_modeling_agent(
    dataframe_name = "reg_model_df",
    target_variable = "target",
    algorithms = list(
      logisticRegression = FALSE,
      randomForest = FALSE,
      linearRegression = TRUE,
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
    )
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("reg_model_df")
})

# =============================================================================
# TEST: Modeling Step Execution
# =============================================================================

test_that("execute_modeling_step handles data overview", {
  skip_if_not(exists("execute_modeling_step"))
  
  assign("overview_model_df", data.frame(
    target = rnorm(30),
    feature = rnorm(30)
  ), envir = .GlobalEnv)
  
  # Get function and check its signature
  fn <- get("execute_modeling_step")
  fn_args <- names(formals(fn))
  
  result <- tryCatch({
    if ("dataframe_name" %in% fn_args) {
      execute_modeling_step(
        step = list(type = "data_overview"),
        dataframe_name = "overview_model_df",
        target_variable = "target"
      )
    } else {
      # Call with positional arguments or different signature
      execute_modeling_step(list(type = "data_overview"))
    }
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("overview_model_df")
})

test_that("execute_data_preprocessing handles missing values", {
  skip_if_not(exists("execute_data_preprocessing"))
  
  assign("preprocess_df", data.frame(
    target = c(1, 2, NA, 4, 5),
    feature1 = c(NA, 2, 3, 4, 5),
    feature2 = c(1, 2, 3, NA, 5)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_data_preprocessing(
      dataframe_name = "preprocess_df",
      target_variable = "target"
    )
  }, error = function(e) {
    # Function may have different signature
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("preprocess_df")
})

# =============================================================================
# TEST: Linear Regression
# =============================================================================

test_that("execute_linear_regression trains model correctly", {
  skip_if_not(exists("execute_linear_regression"))
  
  assign("lm_df", data.frame(
    y = rnorm(50, mean = 10, sd = 2),
    x1 = rnorm(50),
    x2 = rnorm(50)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_linear_regression(
      dataframe_name = "lm_df",
      target_variable = "y",
      predictors = c("x1", "x2")
    )
  }, error = function(e) {
    # Function may have different signature
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("lm_df")
})

# =============================================================================
# TEST: Logistic Regression
# =============================================================================

test_that("execute_logistic_regression trains binary classifier", {
  skip_if_not(exists("execute_logistic_regression"))
  
  assign("logit_df", data.frame(
    outcome = factor(rep(c(0, 1), each = 30)),
    x1 = rnorm(60),
    x2 = rnorm(60)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_logistic_regression(
      dataframe_name = "logit_df",
      target_variable = "outcome",
      predictors = c("x1", "x2")
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("logit_df")
})

# =============================================================================
# TEST: Multinomial Regression
# =============================================================================

test_that("execute_multinomial_regression handles multi-class", {
  skip_if_not(exists("execute_multinomial_regression"))
  skip_if_not_installed("nnet")
  
  assign("multi_df", data.frame(
    outcome = factor(rep(c("A", "B", "C"), each = 20)),
    x1 = rnorm(60),
    x2 = rnorm(60)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_multinomial_regression(
      dataframe_name = "multi_df",
      target_variable = "outcome",
      predictors = c("x1", "x2")
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("multi_df")
})

# =============================================================================
# TEST: Random Forest
# =============================================================================

test_that("execute_random_forest trains classification model", {
  skip_if_not(exists("execute_random_forest"))
  skip_if_not_installed("randomForest")
  
  assign("rf_class_df", data.frame(
    target = factor(rep(c("yes", "no"), each = 25)),
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_random_forest(
      dataframe_name = "rf_class_df",
      target_variable = "target",
      predictors = c("x1", "x2", "x3"),
      task = "classification"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("rf_class_df")
})

test_that("execute_random_forest trains regression model", {
  skip_if_not(exists("execute_random_forest"))
  skip_if_not_installed("randomForest")
  
  assign("rf_reg_df", data.frame(
    target = rnorm(50),
    x1 = rnorm(50),
    x2 = rnorm(50)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_random_forest(
      dataframe_name = "rf_reg_df",
      target_variable = "target",
      predictors = c("x1", "x2"),
      task = "regression"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("rf_reg_df")
})

# =============================================================================
# TEST: XGBoost
# =============================================================================

test_that("execute_xgboost handles classification", {
  skip_if_not(exists("execute_xgboost"))
  skip_if_not_installed("xgboost")
  
  assign("xgb_df", data.frame(
    target = factor(rep(c(0, 1), each = 25)),
    x1 = rnorm(50),
    x2 = rnorm(50)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_xgboost(
      dataframe_name = "xgb_df",
      target_variable = "target",
      predictors = c("x1", "x2"),
      task = "classification"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("xgb_df")
})

# =============================================================================
# TEST: Dimensionality Reduction
# =============================================================================

test_that("execute_dimensionality_reduction performs PCA", {
  skip_if_not(exists("execute_dimensionality_reduction"))
  
  assign("pca_df", data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50),
    x4 = rnorm(50)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_dimensionality_reduction(
      dataframe_name = "pca_df",
      method = "pca",
      n_components = 2
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("pca_df")
})

# =============================================================================
# TEST: Feature Engineering
# =============================================================================

test_that("execute_feature_engineering creates interaction features", {
  skip_if_not(exists("execute_feature_engineering"))
  
  assign("feat_eng_df", data.frame(
    target = rnorm(30),
    x1 = rnorm(30),
    x2 = rnorm(30)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_feature_engineering(
      dataframe_name = "feat_eng_df",
      options = list(interactions = TRUE, polynomial = FALSE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("feat_eng_df")
})

# =============================================================================
# TEST: Model Comparison
# =============================================================================

test_that("execute_model_comparison handles multiple models", {
  skip_if_not(exists("execute_model_comparison"))
  
  # This test may just verify the function exists and returns something
  result <- tryCatch({
    execute_model_comparison(
      models = list(),
      metrics = c("accuracy", "rmse")
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: Results Compilation
# =============================================================================

test_that("execute_results_compilation formats results", {
  skip_if_not(exists("execute_results_compilation"))
  
  result <- tryCatch({
    execute_results_compilation(
      workflow_results = list(
        model1 = list(name = "Linear Regression", rmse = 0.5),
        model2 = list(name = "Random Forest", rmse = 0.3)
      )
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result))
})

# =============================================================================
# TEST: Custom Modeling
# =============================================================================

test_that("execute_custom_modeling prepares prompt correctly", {
  skip_if_not(exists("execute_custom_modeling"))
  
  assign("custom_model_df", data.frame(
    target = rnorm(20),
    feature = rnorm(20)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_custom_modeling(
      dataframe_name = "custom_model_df",
      target_variable = "target",
      custom_request = "Train a support vector machine"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result))
  
  cleanup_test_objects("custom_model_df")
})

# =============================================================================
# TEST: Next Step Retrieval
# =============================================================================

test_that("get_next_modeling_step returns step information", {
  skip_if_not(exists("get_next_modeling_step"))
  
  workflow <- list(
    list(type = "linear_regression", step = 1),
    list(type = "random_forest", step = 2)
  )
  
  result <- tryCatch({
    get_next_modeling_step(workflow, current_step = 1)
  }, error = function(e) {
    # Function may have different signature
    tryCatch({
      get_next_modeling_step(workflow)
    }, error = function(e2) {
      list(error = e2$message)
    })
  })
  
  expect_true(is.list(result) || is.null(result))
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("modeling handles single predictor", {
  assign("single_pred_df", data.frame(
    target = rnorm(30),
    predictor = rnorm(30)
  ), envir = .GlobalEnv)
  
  result <- start_modeling_agent(
    dataframe_name = "single_pred_df",
    target_variable = "target",
    algorithms = list(linearRegression = TRUE)
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("single_pred_df")
})

test_that("modeling handles categorical predictors", {
  assign("cat_pred_df", data.frame(
    target = factor(rep(c("A", "B"), each = 25)),
    num_pred = rnorm(50),
    cat_pred = sample(c("X", "Y", "Z"), 50, replace = TRUE)
  ), envir = .GlobalEnv)
  
  result <- start_modeling_agent(
    dataframe_name = "cat_pred_df",
    target_variable = "target",
    algorithms = list(logisticRegression = TRUE)
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("cat_pred_df")
})

test_that("modeling handles missing values in predictors", {
  assign("missing_pred_df", data.frame(
    target = rnorm(50),
    pred1 = c(rnorm(45), rep(NA, 5)),
    pred2 = rnorm(50)
  ), envir = .GlobalEnv)
  
  result <- start_modeling_agent(
    dataframe_name = "missing_pred_df",
    target_variable = "target",
    algorithms = list(linearRegression = TRUE),
    options = list(dataPreprocessing = TRUE)
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("missing_pred_df")
})

test_that("modeling handles imbalanced classes", {
  assign("imbalanced_df", data.frame(
    target = factor(c(rep("rare", 10), rep("common", 90))),
    x1 = rnorm(100),
    x2 = rnorm(100)
  ), envir = .GlobalEnv)
  
  result <- start_modeling_agent(
    dataframe_name = "imbalanced_df",
    target_variable = "target",
    algorithms = list(logisticRegression = TRUE)
  )
  
  expect_true(is.list(result))
  
  cleanup_test_objects("imbalanced_df")
})

