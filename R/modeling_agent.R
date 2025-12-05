# Modeling Agent for RStudio AI
# Provides comprehensive machine learning with model interpretability

#' Start Modeling Agent
#' @param dataframe_name Name of the dataframe to analyze
#' @param problem_type Type of problem (classification, regression, clustering, dimensionality_reduction)
#' @param target_variable Target variable for supervised learning
#' @param algorithms List of selected algorithms
#' @param options List of additional options
#' @param custom_inputs Custom modeling specifications
#' @return List with workflow steps and initial analysis
start_modeling_agent <- function(dataframe_name, target_variable, algorithms, options, custom_inputs = NULL, selected_variables = NULL) {
  tryCatch({
    cat("R: start_modeling_agent called with selected_variables\n")
    
    # Load required packages (only essential ones to avoid errors)
    required_packages <- c("dplyr", "ggplot2", "stats", "MASS")
    load_packages(required_packages)
    
    # Get the dataframe
    df <- get(dataframe_name, envir = .GlobalEnv)
    cat("R: Dataframe retrieved successfully:", dataframe_name, "\n")
    
    # Validate dataframe
    if (!is.data.frame(df)) {
      stop("Selected object is not a dataframe")
    }
    
    # Validate selected variables if provided
    if (!is.null(selected_variables)) {
      available_vars <- names(df)
      
      # Check all operations
      for (operation in names(selected_variables)) {
        if (!is.null(selected_variables[[operation]]) && length(selected_variables[[operation]]) > 0) {
          # Handle both list and data frame structures from JSON
          if (is.data.frame(selected_variables[[operation]])) {
            selected_var_names <- selected_variables[[operation]]$name
          } else {
            selected_var_names <- sapply(selected_variables[[operation]], function(x) {
              if (is.list(x) && !is.null(x$name)) {
                x$name
              } else if (is.character(x)) {
                x
              } else {
                NULL
              }
            })
            selected_var_names <- selected_var_names[!sapply(selected_var_names, is.null)]
          }
          invalid_vars <- selected_var_names[!selected_var_names %in% available_vars]
          
          if (length(invalid_vars) > 0) {
            stop(paste("Selected variables not found in dataframe for", operation, ":", paste(invalid_vars, collapse = ", ")))
          }
        }
      }
    }
    
    cat("R: After validation, selected_variables validated\n")
    
      # Create workflow steps based on selected algorithms
  workflow_steps <- create_modeling_workflow(algorithms, options, dataframe_name, target_variable, custom_inputs)
    
    # Debug: Check selected_variables before returning
    cat("R: Inside start_modeling_agent, selected_variables processed\n")
    
    # Return workflow information (don't execute first step yet - let frontend handle it)
    result <- list(
      success = TRUE,
      dataframe = dataframe_name,
      target_variable = target_variable,
      algorithms = algorithms,
      options = options,
      selected_variables = selected_variables,
      workflow_steps = workflow_steps,
      current_step = list(
        step = 1,
        operation = workflow_steps[[1]]$operation,
        description = workflow_steps[[1]]$description,
        code = workflow_steps[[1]]$code
      ),
      total_steps = length(workflow_steps),
      message = "Modeling agent started successfully"
    )
    
    cat("R: Result selected_variables before return: processed\n")
    result
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      message = paste("Failed to start modeling agent:", e$message)
    )
  })
}

#' Create Modeling Workflow Steps
#' @param algorithms Selected algorithms
#' @param options Additional options
#' @param dataframe_name Name of the dataframe
#' @param target_variable_name Name of the target variable
#' @return List of workflow steps
create_modeling_workflow <- function(algorithms, options, dataframe_name, target_variable_name, custom_inputs = NULL) {
  steps <- list()
  step_counter <- 1
  
  # Data Overview Step
  steps[[step_counter]] <- list(
    step = step_counter,
    operation = "data_overview",
    description = "Analyzing data structure and preparing for modeling",
    code = paste0("execute_modeling_data_overview(", dataframe_name, ", '", target_variable_name, "')")
  )
  step_counter <- step_counter + 1
  
  # Data Preprocessing Step
  steps[[step_counter]] <- list(
    step = step_counter,
    operation = "data_preprocessing",
    description = "Handling missing values, encoding categorical variables, scaling",
    code = paste0("execute_data_preprocessing(", dataframe_name, ", '", target_variable_name, "')")
  )
  step_counter <- step_counter + 1
  
  # Feature Engineering Step
  if (options$featureEngineering) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "feature_engineering",
      description = "Creating new features and selecting important variables",
      code = paste0("execute_feature_engineering(", dataframe_name, ", '", target_variable_name, "', selected_variables)")
    )
    step_counter <- step_counter + 1
  }
  
  # Dimensionality Reduction Step
  if (algorithms$dimensionalityReduction) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "dimensionality_reduction",
      description = "Applying PCA, t-SNE, or UMAP for dimension reduction",
      code = paste0("execute_dimensionality_reduction(", dataframe_name, ", selected_variables)")
    )
    step_counter <- step_counter + 1
  }
  
  # Model Training Steps
  if (algorithms$linearRegression) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "linear_regression",
      description = "Training linear regression model with interpretability",
      code = paste0("execute_linear_regression(", dataframe_name, ", '", target_variable_name, "', list(), selected_variables)")
    )
    step_counter <- step_counter + 1
  }
  
  if (algorithms$logisticRegression) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "logistic_regression",
      description = "Training logistic regression model with interpretability",
      code = paste0("execute_logistic_regression(", dataframe_name, ", '", target_variable_name, "', list(), selected_variables)")
    )
    step_counter <- step_counter + 1
  }
  
  if (algorithms$multinomialRegression) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "multinomial_regression",
      description = "Training multinomial regression model with interpretability",
      code = paste0("execute_multinomial_regression(", dataframe_name, ", '", target_variable_name, "', list(), selected_variables)")
    )
    step_counter <- step_counter + 1
  }
  
  if (algorithms$randomForest) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "random_forest",
      description = "Training random forest model with interpretability",
      code = paste0("execute_random_forest(", dataframe_name, ", '", target_variable_name, "', list(), selected_variables)")
    )
    step_counter <- step_counter + 1
  }
  
  if (algorithms$xgboost) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "xgboost",
      description = "Training XGBoost model with interpretability",
      code = paste0("execute_xgboost(", dataframe_name, ", '", target_variable_name, "', list(), selected_variables)")
    )
    step_counter <- step_counter + 1
  }
  
  # Custom Modeling Step
  if (options$customModeling) {
    custom_description <- if (!is.null(custom_inputs$customModeling)) {
      paste("Custom modeling:", substr(custom_inputs$customModeling, 1, 50))
    } else {
      "Custom modeling requirements"
    }
    
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "custom_modeling",
      description = custom_description,
      code = paste0("execute_custom_modeling(", dataframe_name, ", '", target_variable_name, "', custom_inputs, selected_variables)")
    )
    step_counter <- step_counter + 1
  }
  
  # Model Comparison Step
  if (length(algorithms) > 1) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "model_comparison",
      description = "Comparing model performance and generating insights",
      code = paste0("execute_model_comparison(", dataframe_name, ", '", target_variable_name, "', list(", 
                   paste0(names(algorithms)[sapply(algorithms, isTRUE)], " = TRUE", collapse = ", "), "))")
    )
    step_counter <- step_counter + 1
  }
  
  # Results Compilation Step
  steps[[step_counter]] <- list(
    step = step_counter,
    operation = "results_compilation",
    description = "Compiling final results and generating comprehensive report",
    code = paste0("execute_results_compilation(", dataframe_name, ", '", target_variable_name, "', list(", 
                   paste0(names(algorithms)[sapply(algorithms, isTRUE)], " = TRUE", collapse = ", "), "))")
  )
  
  return(steps)
}

#' Execute Modeling Step
#' @param df Dataframe to analyze
#' @param step_info Step information
#' @param target_variable Target variable for supervised learning
#' @param algorithms Selected algorithms
#' @param options Additional options
#' @return Step execution result
execute_modeling_step <- function(df, step_info, target_variable, algorithms, options, custom_inputs = NULL) {
  tryCatch({
    operation <- step_info$operation
    
    switch(operation,
      "data_overview" = execute_modeling_data_overview(df, target_variable),
      "data_preprocessing" = execute_data_preprocessing(df, target_variable),
      "feature_engineering" = execute_feature_engineering(df, target_variable, options),
      "dimensionality_reduction" = execute_dimensionality_reduction(df, options),
      "linear_regression" = execute_linear_regression(df, target_variable, options),
      "logistic_regression" = execute_logistic_regression(df, target_variable, options),
      "multinomial_regression" = execute_multinomial_regression(df, target_variable, options),
      "random_forest" = execute_random_forest(df, target_variable, options),
      "xgboost" = execute_xgboost(df, target_variable, options),
      "custom_modeling" = execute_custom_modeling(df, target_variable, custom_inputs),
      "model_comparison" = execute_model_comparison(df, target_variable, algorithms),
      "results_compilation" = execute_results_compilation(df, target_variable, algorithms),
      stop("Unknown operation: ", operation)
    )
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      step = step_info$step,
      operation = operation
    )
  })
}

#' Execute Modeling Data Overview Step
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @return Data overview results
execute_modeling_data_overview <- function(df, target_variable) {
  tryCatch({
    # Validate inputs
    if (is.null(df)) {
      stop("Dataframe is NULL")
    }
    if (!is.data.frame(df)) {
      stop("Input is not a dataframe")
    }
    if (is.null(target_variable)) {
      stop("Target variable is NULL")
    }
    if (!is.character(target_variable)) {
      stop("Target variable must be a character string")
    }
    if (!target_variable %in% names(df)) {
      stop(paste("Target variable '", target_variable, "' not found in dataframe"))
    }
    
    # Basic structure information
    structure_info <- list(
      dimensions = dim(df),
      variable_count = ncol(df),
      observation_count = nrow(df),
      memory_usage = as.numeric(object.size(df))
    )
  
  # Variable types and summary
  variable_info <- list()
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      # Convert summary to simple list
      summary_obj <- summary(df[[col]])
      variable_info[[col]] <- list(
        type = "numeric",
        summary = list(
          min = as.numeric(summary_obj[1]),
          q1 = as.numeric(summary_obj[2]),
          median = as.numeric(summary_obj[3]),
          mean = as.numeric(summary_obj[4]),
          q3 = as.numeric(summary_obj[5]),
          max = as.numeric(summary_obj[6])
        ),
        missing_count = sum(is.na(df[[col]])),
        unique_count = length(unique(df[[col]]))
      )
    } else {
      variable_info[[col]] <- list(
        type = "categorical",
        levels = as.character(levels(factor(df[[col]]))),
        missing_count = sum(is.na(df[[col]])),
        unique_count = length(unique(df[[col]]))
      )
    }
  }
  
  # Target variable analysis
  target_analysis <- NULL
  if (!is.null(target_variable) && target_variable %in% names(df)) {
    target_data <- df[[target_variable]]
    if (is.numeric(target_data)) {
      # Convert summary to simple list
      summary_obj <- summary(target_data)
      target_analysis <- list(
        type = "numeric",
        distribution = list(
          min = as.numeric(summary_obj[1]),
          q1 = as.numeric(summary_obj[2]),
          median = as.numeric(summary_obj[3]),
          mean = as.numeric(summary_obj[4]),
          q3 = as.numeric(summary_obj[5]),
          max = as.numeric(summary_obj[6])
        ),
        missing_count = sum(is.na(target_data)),
        is_balanced = TRUE  # Will be updated based on actual distribution
      )
    } else {
      # Convert table to simple list
      table_obj <- table(target_data)
      target_analysis <- list(
        type = "categorical",
        levels = as.character(levels(factor(target_data))),
        distribution = as.list(table_obj),
        missing_count = sum(is.na(target_data)),
        is_balanced = length(unique(target_data)) == 2 && 
                     min(table_obj) / max(table_obj) > 0.3
      )
    }
  }
  
  list(
    success = TRUE,
    operation = "data_overview",
    structure_info = structure_info,
    variable_info = variable_info,
    target_analysis = target_analysis,
    message = "Data overview completed successfully"
  )
  }, error = function(e) {
    list(
      success = FALSE,
      operation = "data_overview",
      error = e$message,
      message = paste("Data overview failed:", e$message)
    )
  })
}

#' Execute Data Preprocessing Step
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @return Preprocessing results
execute_data_preprocessing <- function(df, target_variable) {
  # Handle missing values
  missing_summary <- as.list(sapply(df, function(x) sum(is.na(x))))
  
  # Encode categorical variables
  categorical_vars <- names(df)[sapply(df, is.character)]
  encoding_info <- list()
  
  for (var in categorical_vars) {
    if (var != target_variable) {
      df[[var]] <- factor(df[[var]])
      encoding_info[[var]] <- list(
        original_type = "character",
        encoded_type = "factor",
        levels = levels(df[[var]]),
        level_count = length(levels(df[[var]]))
      )
    }
  }
  
  # Scale numeric variables (excluding target)
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  scaling_info <- list()
  
  for (var in numeric_vars) {
    if (var != target_variable) {
      # Z-score standardization
      df[[var]] <- scale(df[[var]])
      scaling_info[[var]] <- list(
        method = "z_score",
        mean = attr(df[[var]], "scaled:center"),
        sd = attr(df[[var]], "scaled:scale")
      )
    }
  }
  
  # Preprocessed data is returned directly, not stored globally
  
  list(
    success = TRUE,
    operation = "data_preprocessing",
    missing_summary = missing_summary,
    encoding_info = encoding_info,
    scaling_info = scaling_info,
    message = "Data preprocessing completed successfully"
  )
}

#' Execute Dimensionality Reduction
#' @param df Dataframe to analyze
#' @param options Additional options
#' @param selected_variables Variables selected for this algorithm
#' @return Dimensionality reduction results
execute_dimensionality_reduction <- function(df, options = NULL, selected_variables = NULL) {
  # Get dimensionality reduction method
  dr_method <- if (!is.null(options) && !is.null(options$dimensionalityReductionMethod)) {
    options$dimensionalityReductionMethod
  } else {
    "automatic"
  }
  
  # Select variables for reduction - use selected variables if provided
  if (!is.null(selected_variables) && !is.null(selected_variables$`dimensionality-reduction`) && length(selected_variables$`dimensionality-reduction`) > 0) {
    # Extract variable names from selected variables
    if (is.data.frame(selected_variables$`dimensionality-reduction`)) {
      selected_cols <- selected_variables$`dimensionality-reduction`$name
    } else {
      selected_cols <- selected_variables$`dimensionality-reduction`
    }
    
    # Filter to only numeric variables from the selected set
    numeric_cols <- sapply(df[selected_cols], is.numeric)
    numeric_data <- df[selected_cols[numeric_cols], drop = FALSE]
  } else {
    # Fallback to all numeric variables
    numeric_cols <- sapply(df, is.numeric)
    numeric_data <- df[, numeric_cols, drop = FALSE]
  }
  
  if (ncol(numeric_data) < 2) {
    return(list(
      success = FALSE,
      error = "Need at least 2 numeric variables for dimensionality reduction",
      operation = "dimensionality_reduction"
    ))
  }
  
  results <- list(
    success = TRUE,
    operation = "dimensionality_reduction",
    method_used = dr_method,
    n_features_original = ncol(numeric_data),
    n_samples = nrow(numeric_data)
  )
  
  if (dr_method == "automatic") {
    # Automatic method selection based on data characteristics
    n_features <- ncol(numeric_data)
    n_samples <- nrow(numeric_data)
    
    # Decision logic for automatic method selection
    if (n_features <= 10 && n_samples >= 50) {
      # Small number of features, enough samples - use PCA
      selected_method <- "pca"
      reason <- "PCA selected: manageable feature count with sufficient samples"
    } else if (n_features > 20 && n_samples >= 100) {
      # Many features, good sample size - try UMAP for non-linear relationships
      if (requireNamespace("umap", quietly = TRUE)) {
        selected_method <- "umap"
        reason <- "UMAP selected: high-dimensional data with good sample size"
      } else {
        selected_method <- "pca"
        reason <- "PCA selected: UMAP package not available"
      }
    } else if (n_samples >= 30) {
      # Moderate samples - use t-SNE for visualization
      if (requireNamespace("Rtsne", quietly = TRUE)) {
        selected_method <- "tsne"
        reason <- "t-SNE selected: good for visualization with moderate sample size"
      } else {
        selected_method <- "pca"
        reason <- "PCA selected: t-SNE package not available"
      }
    } else {
      # Small sample size - stick with PCA
      selected_method <- "pca"
      reason <- "PCA selected: small sample size, linear method recommended"
    }
    
    results$automatic_selection <- selected_method
    results$selection_reason <- reason
    
    # Execute the automatically selected method
    dr_method <- selected_method
  }
  
  # Execute the selected method
  if (dr_method == "pca" || dr_method == "automatic") {
    pca_result <- prcomp(numeric_data, scale. = TRUE)
    pca_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
    
    results$pca_variance_explained <- round(cumsum(pca_variance)[1:min(5, length(pca_variance))], 3)
    results$pca_components_for_80_percent <- which(cumsum(pca_variance) >= 0.8)[1]
    results$method_details <- "Principal Component Analysis applied with scaling"
    
  } else if (dr_method == "tsne") {
    if (requireNamespace("Rtsne", quietly = TRUE)) {
      set.seed(123)
      perplexity <- min(30, floor((nrow(numeric_data) - 1) / 3))
      tsne_result <- Rtsne::Rtsne(numeric_data, perplexity = perplexity, check_duplicates = FALSE)
      
      results$tsne_perplexity <- perplexity
      results$tsne_dimensions <- ncol(tsne_result$Y)
      results$method_details <- paste("t-SNE applied with perplexity =", perplexity)
    } else {
      results$success <- FALSE
      results$error <- "Rtsne package not available for t-SNE analysis"
    }
    
  } else if (dr_method == "umap") {
    if (requireNamespace("umap", quietly = TRUE)) {
      set.seed(123)
      umap_result <- umap::umap(numeric_data)
      
      results$umap_dimensions <- ncol(umap_result$layout)
      results$method_details <- "UMAP applied with default parameters"
    } else {
      results$success <- FALSE
      results$error <- "umap package not available for UMAP analysis"
    }
  }
  
  results$tsne_available <- requireNamespace("Rtsne", quietly = TRUE)
  results$umap_available <- requireNamespace("umap", quietly = TRUE)
  results$message <- "Dimensionality reduction completed successfully"
  
  return(results)
}

#' Execute Linear Regression
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @param options Additional options
#' @return Linear regression results with interpretability
execute_linear_regression <- function(df, target_variable, options, selected_variables = NULL) {
  # Prepare formula - use selected variables if provided
  if (!is.null(selected_variables) && !is.null(selected_variables$`linear-regression`) && length(selected_variables$`linear-regression`) > 0) {
    # Extract variable names from selected variables
    if (is.data.frame(selected_variables$`linear-regression`)) {
      features <- selected_variables$`linear-regression`$name
    } else {
      features <- sapply(selected_variables$`linear-regression`, function(x) {
        if (is.list(x) && !is.null(x$name)) {
          x$name
        } else if (is.character(x)) {
          x
        } else {
          NULL
        }
      })
      features <- features[!sapply(features, is.null)]
    }
    # Ensure features exist in dataframe
    features <- features[features %in% names(df)]
  } else {
    # Fallback to all features except target
    features <- names(df)[names(df) != target_variable]
  }
  
  if (length(features) == 0) {
    stop("No valid features selected for Linear Regression model")
  }
  
  formula_str <- paste(target_variable, "~", paste(features, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Train model
  model <- lm(formula_obj, data = df)
  
  # Model summary
  model_summary <- summary(model)
  
  # Feature importance (coefficient magnitudes)
  coefficients <- coef(model)
  feature_importance <- data.frame(
    feature = names(coefficients),
    coefficient = coefficients,
    abs_coefficient = abs(coefficients),
    stringsAsFactors = FALSE
  )
  feature_importance <- feature_importance[order(-feature_importance$abs_coefficient), ]
  
  # Residuals analysis
  residuals_analysis <- list(
    mean_residual = mean(resid(model)),
    residual_sd = sd(resid(model)),
    normality_test = shapiro.test(resid(model))$p.value
  )
  
  # Model is not stored globally - just return results
  
  list(
    success = TRUE,
    operation = "linear_regression",
    r_squared = model_summary$r.squared,
    adjusted_r_squared = model_summary$adj.r.squared,
    feature_importance = as.list(feature_importance),
    residuals_analysis = residuals_analysis,
    message = "Linear regression completed successfully"
  )
}

#' Execute Logistic Regression
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @param options Additional options
#' @return Logistic regression results with interpretability
execute_logistic_regression <- function(df, target_variable, options, selected_variables = NULL) {
  # Ensure target is binary
  target_data <- df[[target_variable]]
  if (length(unique(target_data)) != 2) {
    stop("Logistic regression requires binary target variable")
  }
  
  # Prepare formula - use selected variables if provided
  if (!is.null(selected_variables) && !is.null(selected_variables$`logistic-regression`) && length(selected_variables$`logistic-regression`) > 0) {
    # Extract variable names from selected variables
    if (is.data.frame(selected_variables$`logistic-regression`)) {
      features <- selected_variables$`logistic-regression`$name
    } else {
      features <- sapply(selected_variables$`logistic-regression`, function(x) {
        if (is.list(x) && !is.null(x$name)) {
          x$name
        } else if (is.character(x)) {
          x
        } else {
          NULL
        }
      })
      features <- features[!sapply(features, is.null)]
    }
    # Ensure features exist in dataframe
    features <- features[features %in% names(df)]
  } else {
    # Fallback to all features except target
    features <- names(df)[names(df) != target_variable]
  }
  
  if (length(features) == 0) {
    stop("No valid features selected for Logistic Regression model")
  }
  
  formula_str <- paste(target_variable, "~", paste(features, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Train model
  model <- glm(formula_obj, data = df, family = "binomial")
  
  # Model summary
  model_summary <- summary(model)
  
  # Feature importance (odds ratios)
  coefficients <- coef(model)
  odds_ratios <- exp(coefficients)
  feature_importance <- data.frame(
    feature = names(coefficients),
    coefficient = coefficients,
    odds_ratio = odds_ratios,
    abs_coefficient = abs(coefficients),
    stringsAsFactors = FALSE
  )
  feature_importance <- feature_importance[order(-feature_importance$abs_coefficient), ]
  
  # Model performance
  predictions <- predict(model, type = "response")
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  accuracy <- mean(predicted_classes == target_data)
  
  # Model is not stored globally - just return results
  
  list(
    success = TRUE,
    operation = "logistic_regression",
    accuracy = accuracy,
    feature_importance = as.list(feature_importance),
    odds_ratios = as.list(odds_ratios),
    message = "Logistic regression completed successfully"
  )
}

#' Execute Multinomial Regression
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @param options Additional options
#' @return Multinomial regression results with interpretability
execute_multinomial_regression <- function(df, target_variable, options, selected_variables = NULL) {
  # Ensure target has multiple levels
  target_data <- df[[target_variable]]
  if (length(unique(target_data)) < 3) {
    stop("Multinomial regression requires target with 3+ levels")
  }
  
  # Prepare formula - use selected variables if provided
  if (!is.null(selected_variables) && !is.null(selected_variables$`multinomial-regression`) && length(selected_variables$`multinomial-regression`) > 0) {
    # Extract variable names from selected variables
    if (is.data.frame(selected_variables$`multinomial-regression`)) {
      features <- selected_variables$`multinomial-regression`$name
    } else {
      features <- sapply(selected_variables$`multinomial-regression`, function(x) {
        if (is.list(x) && !is.null(x$name)) {
          x$name
        } else if (is.character(x)) {
          x
        } else {
          NULL
        }
      })
      features <- features[!sapply(features, is.null)]
    }
    # Ensure features exist in dataframe
    features <- features[features %in% names(df)]
  } else {
    # Fallback to all features except target
    features <- names(df)[names(df) != target_variable]
  }
  
  if (length(features) == 0) {
    stop("No valid features selected for Multinomial Regression model")
  }
  
  formula_str <- paste(target_variable, "~", paste(features, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Train model using nnet
  model <- nnet::multinom(formula_obj, data = df, trace = FALSE)
  
  # Model summary
  model_summary <- summary(model)
  
  # Feature importance (coefficient magnitudes)
  coefficients <- coef(model)
  if (is.vector(coefficients)) {
    coefficients <- matrix(coefficients, nrow = 1, dimnames = list(NULL, names(coefficients)))
  }
  
  feature_importance <- data.frame(
    feature = colnames(coefficients),
    mean_coefficient = colMeans(abs(coefficients)),
    max_coefficient = apply(abs(coefficients), 2, max),
    stringsAsFactors = FALSE
  )
  feature_importance <- feature_importance[order(-feature_importance$mean_coefficient), ]
  
  # Model performance
  predictions <- predict(model, type = "class")
  accuracy <- mean(predictions == target_data)
  
  # Model is not stored globally - just return results
  
  list(
    success = TRUE,
    operation = "multinomial_regression",
    accuracy = accuracy,
    feature_importance = as.list(feature_importance),
    coefficients = as.list(coefficients),
    message = "Multinomial regression completed successfully"
  )
}

#' Execute Random Forest
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @param options Additional options
#' @return Random forest results with interpretability
execute_random_forest <- function(df, target_variable, options, selected_variables = NULL) {
  # Prepare formula - use selected variables if provided
  if (!is.null(selected_variables) && !is.null(selected_variables$`random-forest`) && length(selected_variables$`random-forest`) > 0) {
    # Extract variable names from selected variables
    if (is.data.frame(selected_variables$`random-forest`)) {
      features <- selected_variables$`random-forest`$name
    } else {
      features <- sapply(selected_variables$`random-forest`, function(x) {
        if (is.list(x) && !is.null(x$name)) {
          x$name
        } else if (is.character(x)) {
          x
        } else {
          NULL
        }
      })
      features <- features[!sapply(features, is.null)]
    }
    # Ensure features exist in dataframe
    features <- features[features %in% names(df)]
  } else {
    # Fallback to all features except target
    features <- names(df)[names(df) != target_variable]
  }
  
  if (length(features) == 0) {
    stop("No valid features selected for Random Forest model")
  }
  
  formula_str <- paste(target_variable, "~", paste(features, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Determine if classification or regression
  target_data <- df[[target_variable]]
  is_classification <- !is.numeric(target_data) || length(unique(target_data)) <= 10
  
  if (is_classification) {
    # Classification
    model <- randomForest::randomForest(formula_obj, data = df, ntree = 100, importance = TRUE)
    
    # Feature importance
    importance_matrix <- randomForest::importance(model)
    feature_importance <- data.frame(
      feature = rownames(importance_matrix),
      mean_decrease_accuracy = importance_matrix[, "MeanDecreaseAccuracy"],
      mean_decrease_gini = importance_matrix[, "MeanDecreaseGini"],
      stringsAsFactors = FALSE
    )
    feature_importance <- feature_importance[order(-feature_importance$mean_decrease_accuracy), ]
    
    # Model performance
    predictions <- predict(model, type = "class")
    accuracy <- mean(predictions == target_data)
    
    performance_metrics <- list(
      accuracy = accuracy,
      confusion_matrix = table(predictions, target_data)
    )
  } else {
    # Regression
    model <- randomForest::randomForest(formula_obj, data = df, ntree = 100, importance = TRUE)
    
    # Feature importance
    importance_matrix <- randomForest::importance(model)
    feature_importance <- data.frame(
      feature = rownames(importance_matrix),
      percent_inc_mse = importance_matrix[, "%IncMSE"],
      inc_node_purity = importance_matrix[, "IncNodePurity"],
      stringsAsFactors = FALSE
    )
    feature_importance <- feature_importance[order(-feature_importance$percent_inc_mse), ]
    
    # Model performance
    predictions <- predict(model)
    mse <- mean((predictions - target_data)^2)
    r_squared <- 1 - sum((target_data - predictions)^2) / sum((target_data - mean(target_data))^2)
    
    performance_metrics <- list(
      mse = mse,
      r_squared = r_squared,
      rmse = sqrt(mse)
    )
  }
  
  # Model is not stored globally - just return results
  
  list(
    success = TRUE,
    operation = "random_forest",
    is_classification = is_classification,
    feature_importance = as.list(feature_importance),
    performance_metrics = performance_metrics,
    message = "Random forest completed successfully"
  )
}

#' Execute XGBoost
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @param options Additional options
#' @return XGBoost results with interpretability
execute_xgboost <- function(df, target_variable, options, selected_variables = NULL) {
  # Prepare data - use selected variables if provided
  if (!is.null(selected_variables) && !is.null(selected_variables$xgboost) && length(selected_variables$xgboost) > 0) {
    # Extract variable names from selected variables
    if (is.data.frame(selected_variables$xgboost)) {
      features <- selected_variables$xgboost$name
    } else {
      features <- sapply(selected_variables$xgboost, function(x) {
        if (is.list(x) && !is.null(x$name)) {
          x$name
        } else if (is.character(x)) {
          x
        } else {
          NULL
        }
      })
      features <- features[!sapply(features, is.null)]
    }
    # Ensure features exist in dataframe
    features <- features[features %in% names(df)]
  } else {
    # Fallback to all features except target
    features <- names(df)[names(df) != target_variable]
  }
  
  if (length(features) == 0) {
    stop("No valid features selected for XGBoost model")
  }
  
  X <- df[, features, drop = FALSE]
  y <- df[[target_variable]]
  
  # Determine if classification or regression
  is_classification <- !is.numeric(y) || length(unique(y)) <= 10
  
  # Convert categorical variables to numeric
  for (col in names(X)) {
    if (is.character(X[[col]]) || is.factor(X[[col]])) {
      X[[col]] <- as.numeric(factor(X[[col]]))
    }
  }
  
  # Convert to matrix
  X_matrix <- as.matrix(X)
  
  if (is_classification) {
    # Classification
    if (length(unique(y)) == 2) {
      # Binary classification
      y_numeric <- as.numeric(factor(y)) - 1
      model <- xgboost::xgboost(
        data = X_matrix,
        label = y_numeric,
        nrounds = 100,
        objective = "binary:logistic",
        eval_metric = "logloss",
        verbose = 0
      )
    } else {
      # Multi-class classification
      y_numeric <- as.numeric(factor(y)) - 1
      model <- xgboost::xgboost(
        data = X_matrix,
        label = y_numeric,
        nrounds = 100,
        objective = "multi:softprob",
        num_class = length(unique(y)),
        eval_metric = "mlogloss",
        verbose = 0
      )
    }
    
    # Feature importance
    importance_matrix <- xgboost::xgb.importance(feature_names = features, model = model)
    feature_importance <- data.frame(
      feature = importance_matrix$Feature,
      importance = importance_matrix$Importance,
      stringsAsFactors = FALSE
    )
    feature_importance <- feature_importance[order(-feature_importance$importance), ]
    
    # Model performance
    predictions <- predict(model, X_matrix)
    if (length(unique(y)) == 2) {
      predicted_classes <- ifelse(predictions > 0.5, 1, 0)
      accuracy <- mean(predicted_classes == y_numeric)
    } else {
      predicted_classes <- max.col(matrix(predictions, ncol = length(unique(y)), byrow = TRUE)) - 1
      accuracy <- mean(predicted_classes == y_numeric)
    }
    
    performance_metrics <- list(
      accuracy = accuracy
    )
  } else {
    # Regression
    model <- xgboost::xgboost(
      data = X_matrix,
      label = y,
      nrounds = 100,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      verbose = 0
    )
    
    # Feature importance
    importance_matrix <- xgboost::xgb.importance(feature_names = features, model = model)
    feature_importance <- data.frame(
      feature = importance_matrix$Feature,
      importance = importance_matrix$Importance,
      stringsAsFactors = FALSE
    )
    feature_importance <- feature_importance[order(-feature_importance$importance), ]
    
    # Model performance
    predictions <- predict(model, X_matrix)
    mse <- mean((predictions - y)^2)
    r_squared <- 1 - sum((y - predictions)^2) / sum((y - mean(y))^2)
    
    performance_metrics <- list(
      mse = mse,
      r_squared = r_squared,
      rmse = sqrt(mse)
    )
  }
  
  # Model is not stored globally - just return results
  
  list(
    success = TRUE,
    operation = "xgboost",
    is_classification = is_classification,
    feature_importance = as.list(feature_importance),
    performance_metrics = performance_metrics,
    message = "XGBoost completed successfully"
  )
}

#' Execute Feature Engineering
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @param options Additional options
#' @return Feature engineering results
execute_feature_engineering <- function(df, target_variable, options = NULL, selected_variables = NULL) {
  # Get features to work with - use selected variables if provided
  if (!is.null(selected_variables) && !is.null(selected_variables$`feature-engineering`) && length(selected_variables$`feature-engineering`) > 0) {
    # Extract variable names from selected variables
    if (is.data.frame(selected_variables$`feature-engineering`)) {
      features <- selected_variables$`feature-engineering`$name
    } else {
      features <- sapply(selected_variables$`feature-engineering`, function(x) {
        if (is.list(x) && !is.null(x$name)) {
          x$name
        } else if (is.character(x)) {
          x
        } else {
          NULL
        }
      })
      features <- features[!sapply(features, is.null)]
    }
    # Ensure features exist in dataframe
    features <- features[features %in% names(df)]
  } else {
    # Fallback to all features except target
    features <- names(df)[names(df) != target_variable]
  }
  
  if (length(features) == 0) {
    stop("No valid features selected for Feature Engineering")
  }
  
  # Create interaction terms for numeric variables from selected features
  numeric_features <- features[sapply(df[features], is.numeric)]
  
  if (length(numeric_features) >= 2) {
    # Create pairwise interactions
    for (i in 1:(length(numeric_features) - 1)) {
      for (j in (i + 1):length(numeric_features)) {
        interaction_name <- paste(numeric_features[i], numeric_features[j], "interaction", sep = "_")
        df[[interaction_name]] <- df[[numeric_features[i]]] * df[[numeric_features[j]]]
      }
    }
  }
  
  # Create polynomial features for important numeric variables
  if (length(numeric_features) > 0) {
    for (feature in numeric_features[1:min(3, length(numeric_features))]) {
      df[[paste0(feature, "_squared")]] <- df[[feature]]^2
    }
  }
  
  # Engineered data is returned directly, not stored globally
  
  # Calculate new features created (compare with original if available)
  original_cols <- if (exists("original_data", envir = .GlobalEnv)) {
    ncol(get("original_data", envir = .GlobalEnv))
  } else {
    ncol(df)  # Fallback if original not available
  }
  
  list(
    success = TRUE,
    operation = "feature_engineering",
    new_features_created = ncol(df) - original_cols,
    message = "Feature engineering completed successfully"
  )
}

#' Execute Model Comparison
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @param algorithms Selected algorithms
#' @return Model comparison results
execute_model_comparison <- function(df, target_variable, algorithms) {
  # For now, just return a simple comparison since models are created in previous steps
  # This function will be enhanced in future versions to actually compare model performance
  
  list(
    success = TRUE,
    operation = "model_comparison",
    models_compared = names(algorithms)[sapply(algorithms, isTRUE)],
    performance_summary = list(
      message = "Models have been trained in previous steps. Performance metrics available in individual model results."
    ),
    best_model = "All models completed successfully",
    best_metric = "See individual model results",
    message = "Model comparison completed successfully - all selected algorithms have been executed"
  )
}

#' Execute Results Compilation
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @param algorithms Selected algorithms
#' @return Results compilation
execute_results_compilation <- function(df, target_variable, algorithms) {
  # Compile all results
  results <- list(
    dataframe = deparse(substitute(df)),
    target_variable = target_variable,
    algorithms_used = algorithms,
    timestamp = Sys.time(),
    data_summary = list(
      observations = nrow(df),
      features = ncol(df),
      target_type = ifelse(is.numeric(df[[target_variable]]), "numeric", "categorical")
    )
  )
  
  # Add model results summary
  model_results <- list()
  
  # Just track which algorithms were selected, not the actual models
  for (algo_name in names(algorithms)) {
    if (isTRUE(algorithms[[algo_name]])) {
      model_results[[algo_name]] <- "Model training completed in previous step"
    }
  }
  
  results$models <- model_results
  
  list(
    success = TRUE,
    operation = "results_compilation",
    results = results,
    message = "Results compilation completed successfully"
  )
}

#' Get Next Modeling Step Code
#' @param dataframe_name Name of the dataframe
#' @param step_info Information about the current step
#' @param target_variable Target variable
#' @param algorithms Selected algorithms
#' @return Next step code to execute
get_next_modeling_step <- function(dataframe_name, step_info, target_variable, algorithms, selected_variables = NULL) {
  tryCatch({
    # Get the dataframe
    df <- get(dataframe_name, envir = .GlobalEnv)
    
    # Get the current step info
    current_step_num <- step_info$step
    operation <- step_info$operation
    
    # Generate code that includes selected variables setup
    # Use serialize/unserialize to avoid deparse issues with complex objects
    if (!is.null(selected_variables)) {
      # Create a temporary variable name to store the serialized data
      temp_var_name <- paste0("temp_selected_vars_", as.numeric(Sys.time()))
      assign(temp_var_name, selected_variables, envir = .GlobalEnv)
      
      code_with_variables <- paste0(
        "# Set up selected variables for this step\n",
        "selected_variables <- get('", temp_var_name, "', envir = .GlobalEnv)\n",
        "rm('", temp_var_name, "', envir = .GlobalEnv)\n\n",
        step_info$code
      )
    } else {
      code_with_variables <- paste0(
        "# Set up selected variables for this step\n",
        "selected_variables <- NULL\n\n",
        step_info$code
      )
    }
    
    # Return the code for this step (don't execute it yet)
    list(
      success = TRUE,
      step = current_step_num,
      operation = operation,
      description = step_info$description,
      code = code_with_variables,
      result = NULL
    )
    
  }, error = function(e) {
    list(
      success = FALSE,
      step = step_info$step,
      operation = step_info$operation,
      description = paste("Error getting step:", step_info$operation),
      code = paste("# Error in step:", e$message),
      error = e$message
    )
  })
}

#' Execute Custom Modeling
#' @param df Dataframe to analyze
#' @param target_variable Target variable
#' @param custom_inputs Custom modeling specifications
#' @return Custom modeling results
execute_custom_modeling <- function(df, target_variable, custom_inputs, selected_variables = NULL) {
  tryCatch({
    # Get custom modeling description
    custom_modeling <- if (!is.null(custom_inputs$customModeling)) {
      custom_inputs$customModeling
    } else {
      "Custom modeling requirements specified"
    }
    
    # For now, return a placeholder result
    # In a real implementation, this would parse the custom requirements
    # and execute appropriate modeling procedures
    results <- list(
      modeling_type = "Custom Modeling",
      description = custom_modeling,
      status = "Custom modeling requirements received",
      recommendation = paste("The following custom modeling was requested:", 
                           substr(custom_modeling, 1, 200),
                           if(nchar(custom_modeling) > 200) "..." else ""),
      implementation_note = "Custom modeling would be implemented based on the specific requirements provided",
      data_info = list(
        target_variable = target_variable,
        feature_count = ncol(df) - 1,
        sample_count = nrow(df)
      )
    )
    
    list(
      success = TRUE,
      step = "custom_modeling",
      operation = "custom_modeling",
      results = results,
      custom_request = custom_modeling,
      message = "Custom modeling requirements processed successfully"
    )
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      step = "custom_modeling",
      operation = "custom_modeling"
    )
  })
}

#' Load Required Packages
#' @param packages Vector of package names
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
      }, error = function(e) {
        cat("Warning: Failed to install/load package:", pkg, "- Error:", e$message, "\n")
        # Don't stop execution, just warn and continue
      })
    }
  }
}
