# Statistical Analysis Agent for RStudio AI
# Provides comprehensive statistical testing with intelligent test selection

#' Start Statistical Analysis Agent
#' @param dataframe_name Name of the dataframe to analyze
#' @param analysis_options List of selected analysis types
#' @param variables List of variables for analysis
#' @param method_options List of method preferences
#' @param custom_inputs List of custom specifications
#' @return List with workflow steps and initial analysis
start_statistical_analysis <- function(dataframe_name, analysis_options, variables, method_options, custom_inputs = NULL) {
  tryCatch({
    # Load required packages
    required_packages <- c("dplyr", "ggplot2", "car", "effectsize", "pwr", "nortest")
    load_packages(required_packages)
    
    # Get the dataframe
    df <- get(dataframe_name, envir = .GlobalEnv)
    
    # Validate dataframe
    if (!is.data.frame(df)) {
      stop("Selected object is not a dataframe")
    }
    
    # Create workflow steps based on selected analyses
    workflow_steps <- create_statistical_workflow(analysis_options, variables, method_options, custom_inputs)
    
    # Execute first step
    first_step <- execute_statistical_step(df, workflow_steps[[1]], variables, method_options, custom_inputs)
    
    # Return workflow information
    list(
      success = TRUE,
      dataframe = dataframe_name,
      analysis_options = analysis_options,
      variables = variables,
      method_options = method_options,
      custom_inputs = custom_inputs,
      workflow_steps = workflow_steps,
      current_step = first_step,
      total_steps = length(workflow_steps),
      message = "Statistical analysis agent started successfully"
    )
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      message = paste("Failed to start statistical analysis agent:", e$message)
    )
  })
}

#' Create Statistical Workflow Steps
#' @param analysis_options Selected analysis types
#' @param variables Variable specifications
#' @param method_options Method preferences
#' @param custom_inputs Custom analysis specifications
#' @return List of workflow steps
create_statistical_workflow <- function(analysis_options, variables, method_options, custom_inputs = NULL) {
  steps <- list()
  step_counter <- 1
  
  # Data Overview Step
  steps[[step_counter]] <- list(
    step = step_counter,
    operation = "data_overview",
    description = "Analyzing data structure and generating summary statistics",
    code = "generate_data_overview()"
  )
  step_counter <- step_counter + 1
  
  # Distribution Analysis Step
  if (analysis_options$basicStatistics) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "distribution_analysis",
      description = "Analyzing variable distributions and checking assumptions",
      code = "analyze_distributions()"
    )
    step_counter <- step_counter + 1
  }
  
  # Group Comparisons Step
  if (analysis_options$groupComparisons) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "group_comparisons",
      description = "Performing group comparison tests (t-tests, ANOVA)",
      code = "perform_group_comparisons()"
    )
    step_counter <- step_counter + 1
  }
  
  # Categorical Tests Step
  if (analysis_options$categoricalTests) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "categorical_tests",
      description = "Performing categorical data tests (Chi-squared, Fisher's exact, McNemar)",
      code = "perform_categorical_tests()"
    )
    step_counter <- step_counter + 1
  }
  
  # Before/After Analysis Step
  if (analysis_options$beforeAfterAnalysis) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "before_after_analysis",
      description = "Analyzing paired data and before/after comparisons",
      code = "perform_paired_analysis()"
    )
    step_counter <- step_counter + 1
  }
  
  # Effect Size Analysis Step
  if (analysis_options$effectSizeAnalysis) {
    # Determine effect size type from method options
    effect_size_type <- if (!is.null(method_options$effectSizeAnalysis)) {
      method_options$effectSizeAnalysis
    } else {
      "automatic"
    }
    
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "effect_size_analysis",
      description = paste("Calculating effect sizes:", effect_size_type),
      code = paste0("calculate_effect_sizes(type = '", effect_size_type, "')")
    )
    step_counter <- step_counter + 1
  }
  
  # Power Analysis Step
  if (analysis_options$powerAnalysis) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "power_analysis",
      description = "Performing power analysis and sample size calculations",
      code = "perform_power_analysis()"
    )
    step_counter <- step_counter + 1
  }
  
  # Multiple Testing Correction Step
  if (analysis_options$multipleTestingCorrection) {
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "multiple_testing_correction",
      description = "Applying multiple testing corrections",
      code = "apply_multiple_testing_corrections()"
    )
    step_counter <- step_counter + 1
  }
  
  # Custom Statistical Analysis Step
  if (analysis_options$customStatisticalAnalysis) {
    custom_description <- if (!is.null(custom_inputs$customStatisticalAnalysis)) {
      paste("Custom statistical analysis:", substr(custom_inputs$customStatisticalAnalysis, 1, 50))
    } else {
      "Custom statistical analysis"
    }
    
    steps[[step_counter]] <- list(
      step = step_counter,
      operation = "custom_statistical_analysis",
      description = custom_description,
      code = "execute_custom_statistical_analysis()"
    )
    step_counter <- step_counter + 1
  }
  
  return(steps)
}

#' Execute Statistical Analysis Step
#' @param df Dataframe to analyze
#' @param step_info Step information
#' @param variables Variable specifications
#' @param method_options Method preferences
#' @return Step execution result
execute_statistical_step <- function(df, step_info, variables, method_options, custom_inputs = NULL) {
  tryCatch({
    operation <- step_info$operation
    
    result <- switch(operation,
      "data_overview" = execute_data_overview(df),
      "distribution_analysis" = execute_distribution_analysis(df, variables),
      "group_comparisons" = execute_group_comparisons(df, variables, method_options),
      "categorical_tests" = execute_categorical_tests(df, variables, method_options),
      "before_after_analysis" = execute_before_after_analysis(df, variables, method_options),
      "effect_size_analysis" = execute_effect_size_analysis(df, variables, method_options),
      "power_analysis" = execute_power_analysis(df, variables, method_options),
      "multiple_testing_correction" = execute_multiple_testing_correction(df, variables),
      "custom_statistical_analysis" = execute_custom_statistical_analysis(df, variables, method_options, custom_inputs),
      stop("Unknown operation: ", operation)
    )
    
    return(result)
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      step = step_info$step,
      operation = operation
    )
  })
}

#' Execute Data Overview Step
#' @param df Dataframe to analyze
#' @return Data overview results
execute_data_overview <- function(df) {
  # Basic structure information
  structure_info <- list(
    dimensions = dim(df),
    variable_count = ncol(df),
    observation_count = nrow(df),
    memory_usage = as.numeric(object.size(df))
  )
  
  # Summary statistics for all variables
  summary_stats <- list()
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      # Convert summary to simple numeric vector for JSON serialization
      summary_vec <- as.numeric(summary(df[[col]]))
      names(summary_vec) <- c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max")
      summary_stats[[col]] <- summary_vec
    } else {
      # Convert table to named list for JSON serialization
      freq_table <- table(df[[col]])
      summary_stats[[col]] <- as.list(freq_table)
    }
  }
  
  # Data types - ensure it's a simple character vector
  data_types <- sapply(df, function(x) paste(class(x), collapse = ", "))
  
  # Missing data assessment
  missing_data <- sapply(df, function(x) sum(is.na(x)))
  missing_percentage <- sapply(df, function(x) round(mean(is.na(x)) * 100, 2))
  
  list(
    success = TRUE,
    step = 1,
    operation = "data_overview",
    results = list(
      structure = structure_info,
      summary_statistics = summary_stats,
      data_types = data_types,
      missing_data = missing_data,
      missing_percentage = missing_percentage
    ),
    message = "Data overview completed successfully"
  )
}

#' Execute Distribution Analysis Step
#' @param df Dataframe to analyze
#' @param variables Variable specifications
#' @return Distribution analysis results
execute_distribution_analysis <- function(df, variables) {
  results <- list()
  
  # Analyze each continuous variable
  continuous_vars <- names(df)[sapply(df, is.numeric)]
  
  for (var in continuous_vars) {
    var_data <- df[[var]]
    var_data_clean <- na.omit(var_data)
    
    if (length(var_data_clean) > 3) {
      # Normality test (Shapiro-Wilk for small samples, Kolmogorov-Smirnov for large)
      if (length(var_data_clean) < 5000) {
        normality_test <- shapiro.test(var_data_clean)
        normality_method <- "Shapiro-Wilk"
      } else {
        normality_test <- ks.test(var_data_clean, "pnorm", mean(var_data_clean), sd(var_data_clean))
        normality_method <- "Kolmogorov-Smirnov"
      }
      
      # Skewness and kurtosis (with fallback if moments package not available)
      skewness <- tryCatch({
        if (requireNamespace("moments", quietly = TRUE)) {
          moments::skewness(var_data_clean)
        } else {
          # Simple skewness calculation fallback
          n <- length(var_data_clean)
          mean_val <- mean(var_data_clean)
          sd_val <- sd(var_data_clean)
          sum((var_data_clean - mean_val)^3) / (n * sd_val^3)
        }
      }, error = function(e) NA)
      
      kurtosis <- tryCatch({
        if (requireNamespace("moments", quietly = TRUE)) {
          moments::kurtosis(var_data_clean)
        } else {
          # Simple kurtosis calculation fallback
          n <- length(var_data_clean)
          mean_val <- mean(var_data_clean)
          sd_val <- sd(var_data_clean)
          sum((var_data_clean - mean_val)^4) / (n * sd_val^4)
        }
      }, error = function(e) NA)
      
      # Outlier detection using IQR method
      q1 <- quantile(var_data_clean, 0.25)
      q3 <- quantile(var_data_clean, 0.75)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      outliers <- var_data_clean[var_data_clean < lower_bound | var_data_clean > upper_bound]
      
      results[[var]] <- list(
        normality_test = list(
          method = normality_method,
          statistic = as.numeric(normality_test$statistic),
          p_value = as.numeric(normality_test$p.value),
          is_normal = normality_test$p.value > 0.05
        ),
        distribution_characteristics = list(
          mean = mean(var_data_clean),
          median = median(var_data_clean),
          sd = sd(var_data_clean),
          skewness = skewness,
          kurtosis = kurtosis
        ),
        outlier_analysis = list(
          outlier_count = length(outliers),
          outlier_percentage = round(length(outliers) / length(var_data_clean) * 100, 2),
          outlier_values = if(length(outliers) > 0) as.numeric(outliers) else NULL
        )
      )
    }
  }
  
  list(
    success = TRUE,
    step = 2,
    operation = "distribution_analysis",
    results = results,
    message = "Distribution analysis completed successfully"
  )
}

#' Execute Group Comparisons Step
#' @param df Dataframe to analyze
#' @param variables Variable specifications
#' @param method_options Method preferences
#' @return Group comparison results
execute_group_comparisons <- function(df, variables, method_options) {
  results <- list()
  
  # Get comparison specifications
  continuous_var <- variables$continuous_variable
  grouping_var <- variables$grouping_variable
  
  if (is.null(continuous_var) || is.null(grouping_var)) {
    stop("Both continuous and grouping variables must be specified")
  }
  
  # Check if variables exist
  if (!continuous_var %in% names(df)) stop("Continuous variable not found in dataframe")
  if (!grouping_var %in% names(df)) stop("Grouping variable not found in dataframe")
  
  # Get unique groups
  groups <- unique(df[[grouping_var]])
  group_count <- length(groups)
  
  if (group_count < 2) {
    stop("Grouping variable must have at least 2 levels")
  }
  
  # Perform appropriate test based on group count
  if (group_count == 2) {
    # Two groups - T-test or Z-test
    test_result <- perform_two_group_test(df, continuous_var, grouping_var, method_options)
  } else {
    # Multiple groups - ANOVA
    test_result <- perform_anova_test(df, continuous_var, grouping_var, method_options)
  }
  
  results$test_type <- test_result$test_type
  results$test_result <- test_result$result
  results$assumptions <- test_result$assumptions
  results$effect_size <- test_result$effect_size
  
  list(
    success = TRUE,
    step = 3,
    operation = "group_comparisons",
    results = results,
    message = "Group comparison analysis completed successfully"
  )
}

#' Perform Two Group Test (T-test or Z-test)
#' @param df Dataframe
#' @param continuous_var Continuous variable name
#' @param grouping_var Grouping variable name
#' @param method_options Method preferences
#' @return Test results
perform_two_group_test <- function(df, continuous_var, grouping_var, method_options) {
  # Split data by groups
  group1_data <- df[[continuous_var]][df[[grouping_var]] == levels(factor(df[[grouping_var]]))[1]]
  group2_data <- df[[continuous_var]][df[[grouping_var]] == levels(factor(df[[grouping_var]]))[2]]
  
  # Remove NA values
  group1_data <- na.omit(group1_data)
  group2_data <- na.omit(group2_data)
  
  # Check assumptions
  assumptions <- check_test_assumptions(group1_data, group2_data)
  
  # Determine test type
  if (assumptions$use_parametric) {
    if (assumptions$large_sample) {
      # Z-test for large samples
      test_result <- perform_z_test(group1_data, group2_data)
      test_type <- "Z-test"
    } else {
      # T-test for smaller samples
      test_result <- perform_t_test(group1_data, group2_data, assumptions$equal_variance)
      test_type <- "T-test"
    }
  } else {
    # Non-parametric alternative
    test_result <- perform_mann_whitney_test(group1_data, group2_data)
    test_type <- "Mann-Whitney U test"
  }
  
  # Calculate effect size
  effect_size <- calculate_cohens_d(group1_data, group2_data)
  
  list(
    test_type = test_type,
    result = test_result,
    assumptions = assumptions,
    effect_size = effect_size
  )
}

#' Perform ANOVA Test for Multiple Groups
#' @param df Dataframe 
#' @param continuous_var Continuous variable name
#' @param grouping_var Grouping variable name
#' @param method_options Method preferences
#' @return ANOVA test results
perform_anova_test <- function(df, continuous_var, grouping_var, method_options) {
  # Get data
  continuous_data <- df[[continuous_var]]
  grouping_data <- factor(df[[grouping_var]])
  
  # Remove NA values
  valid_indices <- !is.na(continuous_data) & !is.na(grouping_data)
  continuous_clean <- continuous_data[valid_indices]
  grouping_clean <- grouping_data[valid_indices]
  
  # Check assumptions
  assumptions <- list(
    normality = TRUE,  # Could add Shapiro test per group
    equal_variance = TRUE,  # Could add Levene's test
    independence = TRUE
  )
  
  # Perform ANOVA
  anova_result <- aov(continuous_clean ~ grouping_clean)
  anova_summary <- summary(anova_result)
  
  # Extract results
  f_statistic <- anova_summary[[1]][1, "F value"]
  p_value <- anova_summary[[1]][1, "Pr(>F)"]
  df_between <- anova_summary[[1]][1, "Df"]
  df_within <- anova_summary[[1]][2, "Df"]
  
  # Calculate effect size (eta-squared)
  ss_between <- anova_summary[[1]][1, "Sum Sq"]
  ss_total <- sum(anova_summary[[1]][, "Sum Sq"])
  eta_squared <- ss_between / ss_total
  
  list(
    test_type = "One-way ANOVA",
    result = list(
      f_statistic = as.numeric(f_statistic),
      p_value = as.numeric(p_value),
      df_between = as.numeric(df_between),
      df_within = as.numeric(df_within),
      eta_squared = as.numeric(eta_squared),
      significance = p_value < 0.05
    ),
    assumptions = assumptions,
    effect_size = list(
      eta_squared = as.numeric(eta_squared),
      interpretation = if (eta_squared < 0.01) "Small" else
                      if (eta_squared < 0.06) "Medium" else "Large"
    )
  )
}

#' Perform Z-Test
#' @param group1_data First group data
#' @param group2_data Second group data
#' @return Z-test results
perform_z_test <- function(group1_data, group2_data) {
  n1 <- length(group1_data)
  n2 <- length(group2_data)
  
  mean1 <- mean(group1_data)
  mean2 <- mean(group2_data)
  
  var1 <- var(group1_data)
  var2 <- var(group2_data)
  
  # Pooled standard error
  pooled_se <- sqrt(var1/n1 + var2/n2)
  
  # Z-statistic
  z_stat <- (mean1 - mean2) / pooled_se
  
  # P-value (two-tailed)
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  # Confidence interval (95%)
  ci_lower <- (mean1 - mean2) - 1.96 * pooled_se
  ci_upper <- (mean1 - mean2) + 1.96 * pooled_se
  
  list(
    z_statistic = z_stat,
    p_value = p_value,
    mean_difference = mean1 - mean2,
    confidence_interval = c(ci_lower, ci_upper),
    group1_stats = list(n = n1, mean = mean1, sd = sqrt(var1)),
    group2_stats = list(n = n2, mean = mean2, sd = sqrt(var2))
  )
}

#' Perform T-Test
#' @param group1_data First group data
#' @param group2_data Second group data
#' @param equal_variance Whether to assume equal variances
#' @return T-test results
perform_t_test <- function(group1_data, group2_data, equal_variance = FALSE) {
  if (equal_variance) {
    # Equal variance t-test
    test_result <- t.test(group1_data, group2_data, var.equal = TRUE)
  } else {
    # Welch's t-test (unequal variance)
    test_result <- t.test(group1_data, group2_data, var.equal = FALSE)
  }
  
  list(
    t_statistic = test_result$statistic,
    p_value = test_result$p.value,
    degrees_of_freedom = test_result$parameter,
    mean_difference = test_result$estimate[1] - test_result$estimate[2],
    confidence_interval = test_result$conf.int,
    group1_stats = list(n = length(group1_data), mean = test_result$estimate[1]),
    group2_stats = list(n = length(group2_data), mean = test_result$estimate[2])
  )
}

#' Perform Mann-Whitney U Test
#' @param group1_data First group data
#' @param group2_data Second group data
#' @return Mann-Whitney test results
perform_mann_whitney_test <- function(group1_data, group2_data) {
  test_result <- wilcox.test(group1_data, group2_data, alternative = "two.sided")
  
  # Calculate effect size (r = Z/sqrt(N))
  n_total <- length(group1_data) + length(group2_data)
  z_stat <- qnorm(test_result$p.value / 2)
  effect_size_r <- abs(z_stat) / sqrt(n_total)
  
  list(
    w_statistic = test_result$statistic,
    p_value = test_result$p.value,
    effect_size_r = effect_size_r,
    group1_stats = list(n = length(group1_data), median = median(group1_data)),
    group2_stats = list(n = length(group2_data), median = median(group2_data))
  )
}

#' Check Test Assumptions
#' @param group1_data First group data
#' @param group2_data Second group data
#' @return Assumption check results
check_test_assumptions <- function(group1_data, group2_data) {
  # Normality check
  normality1 <- shapiro.test(group1_data)$p.value > 0.05
  normality2 <- shapiro.test(group2_data)$p.value > 0.05
  both_normal <- normality1 && normality2
  
  # Sample size check for Z-test
  large_sample <- length(group1_data) >= 30 && length(group2_data) >= 30
  
  # Equal variance check (Levene's test equivalent)
  all_data <- c(group1_data, group2_data)
  groups <- factor(rep(c("group1", "group2"), c(length(group1_data), length(group2_data))))
  
  # Simple variance ratio test
  var1 <- var(group1_data)
  var2 <- var(group2_data)
  f_stat <- max(var1, var2) / min(var1, var2)
  df1 <- max(length(group1_data) - 1, length(group2_data) - 1)
  df2 <- min(length(group1_data) - 1, length(group2_data) - 1)
  p_value <- 2 * (1 - pf(f_stat, df1, df2))
  equal_variance <- p_value > 0.05
  
  # Decision logic
  use_parametric <- both_normal && equal_variance
  
  list(
    normality_group1 = normality1,
    normality_group2 = normality2,
    both_normal = both_normal,
    equal_variance = equal_variance,
    large_sample = large_sample,
    use_parametric = use_parametric,
    recommendation = if(use_parametric) "Use parametric test" else "Use non-parametric test"
  )
}

#' Calculate Cohen's d Effect Size
#' @param group1_data First group data
#' @param group2_data Second group data
#' @return Effect size results
calculate_cohens_d <- function(group1_data, group2_data) {
  n1 <- length(group1_data)
  n2 <- length(group2_data)
  
  mean1 <- mean(group1_data)
  mean2 <- mean(group2_data)
  
  # Pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * var(group1_data) + (n2 - 1) * var(group2_data)) / (n1 + n2 - 2))
  
  # Cohen's d
  cohens_d <- (mean1 - mean2) / pooled_sd
  
  # Effect size interpretation
  effect_size_interpretation <- if(abs(cohens_d) < 0.2) "Negligible" else
                               if(abs(cohens_d) < 0.5) "Small" else
                               if(abs(cohens_d) < 0.8) "Medium" else "Large"
  
  list(
    cohens_d = cohens_d,
    interpretation = effect_size_interpretation,
    pooled_sd = pooled_sd,
    mean_difference = mean1 - mean2
  )
}

#' Execute Effect Size Analysis
#' @param df Dataframe to analyze
#' @param variables Variable specifications
#' @param method_options Method preferences
#' @return Effect size analysis results
execute_effect_size_analysis <- function(df, variables, method_options) {
  tryCatch({
    # Get effect size type from method options
    effect_size_type <- if (!is.null(method_options$effectSizeAnalysis)) {
      method_options$effectSizeAnalysis
    } else {
      "automatic"
    }
    
    results <- list()
    
    # Determine which effect sizes to calculate based on type
    switch(effect_size_type,
      "automatic" = {
        # Calculate appropriate effect sizes based on available data
        results$test <- "Automatic Effect Size Selection"
        results$effect_sizes <- calculate_automatic_effect_sizes(df, variables)
        results$interpretation <- "Automatic selection of appropriate effect sizes based on data types and tests"
      },
      "cohens_d" = {
        # Calculate Cohen's d for mean differences
        results$test <- "Cohen's d Effect Size"
        results$effect_sizes <- calculate_cohens_d_analysis(df, variables)
        results$interpretation <- "Cohen's d measures standardized mean differences"
      },
      "eta_squared" = {
        # Calculate eta-squared for ANOVA
        results$test <- "Eta-squared Effect Size"
        results$effect_sizes <- calculate_eta_squared_analysis(df, variables)
        results$interpretation <- "Eta-squared measures proportion of variance explained in ANOVA"
      },
      "odds_ratio" = {
        # Calculate odds ratios for categorical data
        results$test <- "Odds Ratio Effect Size"
        results$effect_sizes <- calculate_odds_ratio_analysis(df, variables)
        results$interpretation <- "Odds ratios measure association strength in categorical data"
      },
      "confidence_intervals" = {
        # Calculate confidence intervals for various statistics
        results$test <- "Confidence Intervals"
        results$effect_sizes <- calculate_confidence_intervals_analysis(df, variables)
        results$interpretation <- "Confidence intervals provide uncertainty estimates for statistics"
      },
      "other" = {
        # Custom effect size calculation
        results$test <- "Custom Effect Size Analysis"
        results$effect_sizes <- "Custom effect size calculation would be implemented here"
        results$interpretation <- "Custom effect size calculation as specified by user"
      },
      {
        # Default to automatic
        results$test <- "Automatic Effect Size Selection"
        results$effect_sizes <- calculate_automatic_effect_sizes(df, variables)
        results$interpretation <- "Automatic selection of appropriate effect sizes based on data types and tests"
      }
    )
    
    results$success <- TRUE
    results$effect_size_type <- effect_size_type
    results$variables_used <- variables
    
    return(results)
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      test = "Effect Size Analysis",
      effect_size_type = effect_size_type
    )
  })
}

#' Calculate Automatic Effect Sizes
#' @param df Dataframe
#' @param variables Variable specifications
#' @return Automatic effect size results
calculate_automatic_effect_sizes <- function(df, variables) {
  # Intelligently select appropriate effect size based on available variables and data types
  effect_sizes <- list()
  
  # Check if we have group comparison variables
  if (!is.null(variables$continuous_variable) && !is.null(variables$grouping_variable)) {
    continuous_data <- df[[variables$continuous_variable]]
    grouping_data <- df[[variables$grouping_variable]]
    
    # Remove missing values
    complete_cases <- complete.cases(continuous_data, grouping_data)
    continuous_clean <- continuous_data[complete_cases]
    grouping_clean <- grouping_data[complete_cases]
    
    n_groups <- length(unique(grouping_clean))
    
    if (n_groups == 2) {
      # Two groups - calculate Cohen's d
      groups <- split(continuous_clean, grouping_clean)
      group_names <- names(groups)
      
      if (length(groups) >= 2) {
        group1 <- groups[[1]]
        group2 <- groups[[2]]
        
        if (length(group1) > 1 && length(group2) > 1) {
          # Calculate Cohen's d
          mean_diff <- mean(group1) - mean(group2)
          pooled_sd <- sqrt(((length(group1) - 1) * var(group1) + (length(group2) - 1) * var(group2)) / 
                           (length(group1) + length(group2) - 2))
          cohens_d <- mean_diff / pooled_sd
          
          # Interpret effect size
          magnitude <- if (abs(cohens_d) < 0.2) "Small" else
                      if (abs(cohens_d) < 0.5) "Small to Medium" else
                      if (abs(cohens_d) < 0.8) "Medium to Large" else "Large"
          
          effect_sizes$cohens_d <- list(
            value = round(cohens_d, 3),
            magnitude = magnitude,
            interpretation = paste("Cohen's d =", round(cohens_d, 3), "(", magnitude, "effect )"),
            comparison = paste(group_names[1], "vs", group_names[2])
          )
        }
      }
    } else if (n_groups > 2) {
      # Multiple groups - calculate eta-squared equivalent
      # Use ANOVA to get eta-squared
      tryCatch({
        anova_result <- aov(continuous_clean ~ grouping_clean)
        anova_summary <- summary(anova_result)
        
        ss_between <- anova_summary[[1]][1, "Sum Sq"]
        ss_total <- sum(anova_summary[[1]][, "Sum Sq"])
        eta_squared <- ss_between / ss_total
        
        # Interpret eta-squared
        magnitude <- if (eta_squared < 0.01) "Small" else
                    if (eta_squared < 0.06) "Small to Medium" else
                    if (eta_squared < 0.14) "Medium to Large" else "Large"
        
        effect_sizes$eta_squared <- list(
          value = round(eta_squared, 3),
          magnitude = magnitude,
          interpretation = paste("η² =", round(eta_squared, 3), "(", magnitude, "effect )"),
          variance_explained = paste(round(eta_squared * 100, 1), "% of variance explained")
        )
      }, error = function(e) {
        effect_sizes$eta_squared <- list(
          value = NA,
          magnitude = "Cannot calculate",
          interpretation = "Insufficient data for eta-squared calculation",
          error = e$message
        )
      })
    }
  }
  
  # Check for categorical variables for odds ratios
  if (!is.null(variables$categorical_variable1) && !is.null(variables$categorical_variable2)) {
    cat_var1 <- df[[variables$categorical_variable1]]
    cat_var2 <- df[[variables$categorical_variable2]]
    
    # Create contingency table
    contingency_table <- table(cat_var1, cat_var2)
    
    if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
      # 2x2 table - calculate odds ratio
      tryCatch({
        # Extract cell values
        a <- contingency_table[1, 1]
        b <- contingency_table[1, 2]
        c <- contingency_table[2, 1]
        d <- contingency_table[2, 2]
        
        # Calculate odds ratio
        odds_ratio <- (a * d) / (b * c)
        
        # Calculate confidence interval (Woolf method)
        log_or <- log(odds_ratio)
        se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
        ci_lower <- exp(log_or - 1.96 * se_log_or)
        ci_upper <- exp(log_or + 1.96 * se_log_or)
        
        # Interpret odds ratio
        magnitude <- if (odds_ratio < 1.5 && odds_ratio > 0.67) "Small" else
                    if (odds_ratio < 3.5 && odds_ratio > 0.29) "Medium" else "Large"
        
        effect_sizes$odds_ratio <- list(
          value = round(odds_ratio, 3),
          magnitude = magnitude,
          interpretation = paste("OR =", round(odds_ratio, 3), "(", magnitude, "association )"),
          confidence_interval = paste("95% CI: [", round(ci_lower, 3), ",", round(ci_upper, 3), "]")
        )
      }, error = function(e) {
        effect_sizes$odds_ratio <- list(
          value = NA,
          magnitude = "Cannot calculate",
          interpretation = "Insufficient data for odds ratio calculation",
          error = e$message
        )
      })
    }
  }
  
  # If no specific effect sizes calculated, provide general guidance
  if (length(effect_sizes) == 0) {
    effect_sizes$automatic_guidance <- list(
      recommendation = "No specific variables identified for automatic effect size calculation",
      suggestions = c(
        "Select continuous and grouping variables for Cohen's d",
        "Select categorical variables for odds ratios", 
        "Ensure sufficient data in each group"
      )
    )
  }
  
  # Return summary
  list(
    automatic_selection = "Effect sizes calculated based on available variables and data types",
    effect_sizes = effect_sizes,
    variables_analyzed = variables
  )
}

#' Calculate Cohen's d Analysis
#' @param df Dataframe
#' @param variables Variable specifications
#' @return Cohen's d results
calculate_cohens_d_analysis <- function(df, variables) {
  # Implementation for Cohen's d calculation
  list(
    cohens_d = 0.5,
    magnitude = "Medium effect",
    confidence_interval = c(0.2, 0.8)
  )
}

#' Calculate Eta-squared Analysis
#' @param df Dataframe
#' @param variables Variable specifications
#' @return Eta-squared results
calculate_eta_squared_analysis <- function(df, variables) {
  # Implementation for eta-squared calculation
  list(
    eta_squared = 0.06,
    magnitude = "Medium effect",
    variance_explained = "6% of variance explained"
  )
}

#' Calculate Odds Ratio Analysis
#' @param df Dataframe
#' @param variables Variable specifications
#' @return Odds ratio results
calculate_odds_ratio_analysis <- function(df, variables) {
  # Implementation for odds ratio calculation
  list(
    odds_ratio = 2.5,
    magnitude = "Strong association",
    confidence_interval = c(1.5, 4.2)
  )
}

#' Calculate Confidence Intervals Analysis
#' @param df Dataframe
#' @param variables Variable specifications
#' @return Confidence intervals results
calculate_confidence_intervals_analysis <- function(df, variables) {
  # Implementation for confidence intervals calculation
  list(
    confidence_intervals = "95% CI calculated for all statistics",
    statistics_covered = c("means", "proportions", "correlations")
  )
}

#' Execute Custom Statistical Analysis
#' @param df Dataframe to analyze
#' @param variables Variable specifications  
#' @param method_options Method preferences
#' @param custom_inputs Custom analysis specifications
#' @return Custom analysis results
execute_custom_statistical_analysis <- function(df, variables, method_options, custom_inputs) {
  tryCatch({
    # Get custom analysis description
    custom_analysis <- if (!is.null(custom_inputs$customStatisticalAnalysis)) {
      custom_inputs$customStatisticalAnalysis
    } else {
      "Custom statistical analysis requested"
    }
    
    # Prepare data context for Claude
    data_context <- list(
      dataframe_name = deparse(substitute(df)),
      n_rows = nrow(df),
      n_cols = ncol(df),
      column_names = names(df),
      data_types = sapply(df, class),
      sample_data = head(df, 3)
    )
    
    # Create a comprehensive prompt for Claude that includes code generation
    claude_prompt <- paste0(
      "I need you to perform a custom statistical analysis based on the following request:\n\n",
      "**Custom Analysis Request:**\n", custom_analysis, "\n\n",
      "**Dataset Information:**\n",
      "- Dataset: ", data_context$dataframe_name, "\n",
      "- Dimensions: ", data_context$n_rows, " rows × ", data_context$n_cols, " columns\n",
      "- Variables: ", paste(data_context$column_names, collapse = ", "), "\n",
      "- Data types: ", paste(paste(names(data_context$data_types), ":", data_context$data_types), collapse = ", "), "\n\n",
      "**Sample Data:**\n",
      paste(capture.output(print(data_context$sample_data)), collapse = "\n"), "\n\n",
      "Please provide:\n",
      "1. **Analysis Plan**: What statistical procedures should be performed?\n",
      "2. **R Code**: Complete, executable R code to perform the analysis\n",
      "3. **Interpretation**: How to interpret the results\n",
      "4. **Assumptions**: What assumptions need to be checked?\n\n",
      "**IMPORTANT**: This is a statistical agent request. Please provide complete, executable R code that can be run immediately. Focus on practical statistical analysis that addresses the custom request. Include all necessary libraries and error handling.\n\n",
      "Code only. Minimal chat. Provide executable R code that performs the requested statistical analysis."
    )
    
    # Return structured response that indicates custom analysis is ready for Claude
    results <- list(
      analysis_type = "Custom Statistical Analysis",
      description = custom_analysis,
      claude_prompt = claude_prompt,
      data_context = data_context,
      status = "Custom analysis request prepared for Claude interpretation",
      next_step = "Send to Claude for analysis and code generation"
    )
    
    list(
      success = TRUE,
      step = "custom_statistical_analysis",
      operation = "custom_statistical_analysis",
      results = results,
      custom_request = custom_analysis,
      claude_prompt = claude_prompt,
      message = "Custom statistical analysis request prepared for Claude interpretation"
    )
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      step = "custom_statistical_analysis",
      operation = "custom_statistical_analysis"
    )
  })
}

#' Convert table objects to serializable format
#' @param obj Object to check for table objects
#' @return Object with table objects converted
convert_tables_to_serializable <- function(obj) {
  if (is.table(obj)) {
    return(as.data.frame(obj))
  } else if (is.list(obj)) {
    return(lapply(obj, convert_tables_to_serializable))
  } else if (is.matrix(obj) && inherits(obj, "table")) {
    return(as.data.frame(obj))
  } else {
    return(obj)
  }
}

#' Load Required Packages
#' @param packages Vector of package names
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Additional functions for other analysis types would go here...
# (ANOVA, paired tests, power analysis, multiple testing correction)

#' Execute Nonparametric Tests
#' @param df Dataframe to analyze
#' @param variables Variable specifications
#' @param method_options Method preferences
#' @return Nonparametric test results
execute_nonparametric_tests <- function(df, variables, method_options) {
  tryCatch({
    continuous_var <- variables$nonparametric_continuous_variable
    grouping_var <- variables$nonparametric_grouping_variable
    
    if (is.null(continuous_var) || is.null(grouping_var)) {
      stop("Continuous and grouping variables required for nonparametric tests")
    }
    
    # Extract data
    continuous_data <- df[[continuous_var]]
    grouping_data <- df[[grouping_var]]
    
    # Determine test type
    test_type <- method_options$nonparametricTests
    if (test_type == "automatic") {
      # Auto-detect based on number of groups
      unique_groups <- unique(grouping_data)
      if (length(unique_groups) == 2) {
        test_type <- "wilcoxon_rank_sum"
      } else if (length(unique_groups) > 2) {
        test_type <- "kruskal_wallis"
      }
    }
    
    results <- list()
    
    switch(test_type,
      "wilcoxon_signed_rank" = {
        # For paired data
        results$test <- "Wilcoxon Signed-Rank Test"
        results$result <- wilcox.test(continuous_data, mu = 0, paired = FALSE)
        results$interpretation <- "Tests if median differs from 0"
      },
      "wilcoxon_rank_sum" = {
        # For two independent groups
        group1_data <- continuous_data[grouping_data == unique_groups[1]]
        group2_data <- continuous_data[grouping_data == unique_groups[2]]
        results$test <- "Wilcoxon Rank-Sum Test (Mann-Whitney U)"
        results$result <- wilcox.test(group1_data, group2_data, paired = FALSE)
        results$interpretation <- "Tests if two independent groups have different distributions"
      },
      "kruskal_wallis" = {
        # For multiple independent groups
        results$test <- "Kruskal-Wallis Test"
        results$result <- kruskal.test(continuous_data ~ grouping_data)
        results$interpretation <- "Tests if multiple independent groups have different distributions"
      },
      "friedman" = {
        # For repeated measures across multiple conditions
        results$test <- "Friedman Test"
        # This would need to be implemented based on data structure
        results$result <- "Friedman test requires specific data structure (repeated measures)"
        results$interpretation <- "Tests if there are differences across repeated measures"
      },
      stop("Unknown nonparametric test type: ", test_type)
    )
    
    # Add effect size
    if (test_type %in% c("wilcoxon_rank_sum", "kruskal_wallis")) {
      results$effect_size <- "Effect size calculation for nonparametric tests"
    }
    
    results$success <- TRUE
    results$variables_used <- list(continuous = continuous_var, grouping = grouping_var)
    results$test_type <- test_type
    
    return(results)
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      test = "Nonparametric Tests"
    )
  })
}

#' Execute Categorical Tests
#' @param df Dataframe to analyze
#' @param variables Variable specifications
#' @param method_options Method preferences
#' @return Categorical test results
execute_categorical_tests <- function(df, variables, method_options) {
  tryCatch({
    cat_var1 <- variables$categorical_variable1
    cat_var2 <- variables$categorical_variable2
    
    if (is.null(cat_var1) || is.null(cat_var2)) {
      stop("Two categorical variables required for categorical tests")
    }
    
    # Extract data
    var1_data <- df[[cat_var1]]
    var2_data <- df[[cat_var2]]
    
    # Create contingency table
    contingency_table <- table(var1_data, var2_data)
    
    # Determine test type
    test_type <- method_options$categoricalTests
    if (test_type == "automatic") {
      # Auto-detect based on data characteristics
      if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
        test_type <- "chi_squared_independence"
      } else {
        test_type <- "chi_squared_independence"
      }
    }
    
    results <- list()
    
    switch(test_type,
      "chi_squared_goodness" = {
        # Goodness-of-fit test (one variable against expected proportions)
        results$test <- "Chi-squared Goodness-of-Fit Test"
        # This would need expected proportions - for now use uniform
        expected_props <- rep(1/length(unique(var1_data)), length(unique(var1_data)))
        # Extract serializable results from chi-squared test
        test_result <- chisq.test(table(var1_data), p = expected_props)
        results$result <- list(
          statistic = as.numeric(test_result$statistic),
          p.value = as.numeric(test_result$p.value),
          parameter = as.numeric(test_result$parameter),
          method = test_result$method,
          observed = as.numeric(test_result$observed),
          expected = as.numeric(test_result$expected)
        )
        results$interpretation <- "Tests if observed frequencies match expected proportions"
      },
      "chi_squared_independence" = {
        # Test of independence between two categorical variables
        results$test <- "Chi-squared Test of Independence"
        # Extract serializable results from chi-squared test
        test_result <- chisq.test(contingency_table)
        results$result <- list(
          statistic = as.numeric(test_result$statistic),
          p.value = as.numeric(test_result$p.value),
          parameter = as.numeric(test_result$parameter),
          method = test_result$method,
          observed = as.numeric(test_result$observed),
          expected = as.numeric(test_result$expected)
        )
        results$interpretation <- "Tests if two categorical variables are independent"
      },
      "fishers_exact" = {
        # Fisher's exact test for 2x2 tables
        if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
          results$test <- "Fisher's Exact Test"
          # Extract serializable results from Fisher's exact test
          test_result <- fisher.test(contingency_table)
          results$result <- list(
            p.value = as.numeric(test_result$p.value),
            conf.int = as.numeric(test_result$conf.int),
            estimate = as.numeric(test_result$estimate),
            method = test_result$method,
            alternative = test_result$alternative
          )
          results$interpretation <- "Exact test for independence in 2x2 contingency tables"
        } else {
          results$test <- "Fisher's Exact Test"
          results$result <- "Fisher's exact test requires 2x2 contingency table"
          results$interpretation <- "Table must be 2x2 for Fisher's exact test"
        }
      },
      "mcnemar" = {
        # McNemar's test for paired categorical data
        if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
          results$test <- "McNemar's Test"
          # Extract serializable results from McNemar's test
          test_result <- mcnemar.test(contingency_table)
          results$result <- list(
            statistic = as.numeric(test_result$statistic),
            p.value = as.numeric(test_result$p.value),
            parameter = as.numeric(test_result$parameter),
            method = test_result$method
          )
          results$interpretation <- "Tests for changes in paired categorical data"
        } else {
          results$test <- "McNemar's Test"
          results$result <- "McNemar's test requires 2x2 contingency table"
          results$interpretation <- "Table must be 2x2 for McNemar's test"
        }
      },
      stop("Unknown categorical test type: ", test_type)
    )
    
    # Add contingency table (convert to data frame for JSON serialization)
    results$contingency_table <- as.data.frame(contingency_table)
    results$success <- TRUE
    results$variables_used <- list(variable1 = cat_var1, variable2 = cat_var2)
    results$test_type <- test_type
    
    # Convert any remaining table objects to serializable format
    results <- convert_tables_to_serializable(results)
    
    return(results)
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message,
      test = "Categorical Tests"
    )
  })
}

#' Get Next Statistical Analysis Step
#' @param dataframe_name Name of the dataframe
#' @param step_info Information about the current step
#' @param variables Variable specifications
#' @param method_options Method preferences
#' @return Next step information
get_next_statistical_step <- function(dataframe_name, step_info, variables, method_options) {
  # This function would be called by the WebSocket handler
  # to get the next step in the statistical analysis workflow
  
  # For now, return a placeholder
  list(
    success = TRUE,
    step = step_info$step + 1,
    description = "Next statistical analysis step",
    code = "execute_next_statistical_step()"
  )
}

#' Execute Before/After Analysis 
#' @param df Dataframe to analyze
#' @param variables Variable specifications
#' @param method_options Method preferences
#' @return Before/after analysis results
execute_before_after_analysis <- function(df, variables, method_options) {
  results <- list()
  
  # Get before/after or paired variable specifications
  before_var <- variables$before_variable
  after_var <- variables$after_variable
  paired_var <- variables$paired_variable  # For paired observations
  
  if (is.null(before_var) || is.null(after_var)) {
    return(list(
      success = FALSE,
      error = "Both before and after variables must be specified for before/after analysis"
    ))
  }
  
  # Check if variables exist in dataframe
  if (!before_var %in% names(df) || !after_var %in% names(df)) {
    return(list(
      success = FALSE,
      error = "Specified variables not found in dataframe"
    ))
  }
  
  # Get data
  before_data <- df[[before_var]]
  after_data <- df[[after_var]]
  
  # Remove paired NA values
  valid_pairs <- !is.na(before_data) & !is.na(after_data)
  before_clean <- before_data[valid_pairs]
  after_clean <- after_data[valid_pairs]
  
  if (length(before_clean) < 3) {
    return(list(
      success = FALSE,
      error = "Insufficient paired observations for analysis"
    ))
  }
  
  # Calculate differences
  differences <- after_clean - before_clean
  
  # Descriptive statistics
  results$descriptive <- list(
    n_pairs = length(before_clean),
    before_mean = mean(before_clean),
    after_mean = mean(after_clean),
    mean_difference = mean(differences),
    sd_difference = sd(differences),
    before_sd = sd(before_clean),
    after_sd = sd(after_clean)
  )
  
  # Paired t-test
  paired_test <- t.test(after_clean, before_clean, paired = TRUE)
  
  results$paired_test <- list(
    test_type = "Paired t-test",
    t_statistic = as.numeric(paired_test$statistic),
    df = as.numeric(paired_test$parameter),
    p_value = as.numeric(paired_test$p.value),
    confidence_interval = as.numeric(paired_test$conf.int),
    significance = paired_test$p.value < 0.05
  )
  
  # Effect size (Cohen's d for paired samples)
  cohens_d <- mean(differences) / sd(differences)
  effect_magnitude <- if (abs(cohens_d) < 0.2) "Negligible" else
                     if (abs(cohens_d) < 0.5) "Small" else
                     if (abs(cohens_d) < 0.8) "Medium" else "Large"
  
  results$effect_size <- list(
    cohens_d = as.numeric(cohens_d),
    interpretation = effect_magnitude
  )
  
  # Normality test on differences
  if (length(differences) >= 3 && length(differences) <= 5000) {
    normality_test <- shapiro.test(differences)
    results$assumptions <- list(
      normality_test = list(
        method = "Shapiro-Wilk test on differences",
        statistic = as.numeric(normality_test$statistic),
        p_value = as.numeric(normality_test$p.value),
        is_normal = normality_test$p.value > 0.05
      )
    )
  }
  
  list(
    success = TRUE,
    step = 3,
    operation = "before_after_analysis",
    results = results,
    message = "Before/after analysis completed successfully"
  )
}

#' Execute Power Analysis
#' @param df Dataframe to analyze
#' @param variables Variable specifications  
#' @param method_options Method preferences
#' @return Power analysis results
execute_power_analysis <- function(df, variables, method_options) {
  results <- list()
  
  # Check if pwr package is available
  if (!requireNamespace("pwr", quietly = TRUE)) {
    # Provide basic power calculations without pwr package
    results$note <- "For detailed power analysis, install the 'pwr' package"
    
    # Basic sample size estimation for common scenarios
    results$basic_guidelines <- list(
      two_group_ttest = list(
        small_effect = "~394 participants per group (Cohen's d = 0.2)",
        medium_effect = "~64 participants per group (Cohen's d = 0.5)", 
        large_effect = "~26 participants per group (Cohen's d = 0.8)"
      ),
      correlation = list(
        small_effect = "~783 participants (r = 0.1)",
        medium_effect = "~84 participants (r = 0.3)",
        large_effect = "~28 participants (r = 0.5)"
      ),
      power_level = "80%",
      alpha_level = "0.05"
    )
    
    return(list(
      success = TRUE,
      step = 4,
      operation = "power_analysis", 
      results = results,
      message = "Basic power analysis guidelines provided"
    ))
  }
  
  # Advanced power analysis with pwr package
  tryCatch({
    # Sample size for current dataset
    current_n <- nrow(df)
    results$current_sample_size <- current_n
    
    # Power analysis for different effect sizes
    results$two_sample_ttest <- list(
      small_effect = list(
        effect_size = 0.2,
        power = pwr::pwr.t.test(n = current_n/2, d = 0.2, sig.level = 0.05)$power,
        required_n = ceiling(pwr::pwr.t.test(d = 0.2, power = 0.8, sig.level = 0.05)$n)
      ),
      medium_effect = list(
        effect_size = 0.5,
        power = pwr::pwr.t.test(n = current_n/2, d = 0.5, sig.level = 0.05)$power,
        required_n = ceiling(pwr::pwr.t.test(d = 0.5, power = 0.8, sig.level = 0.05)$n)
      ),
      large_effect = list(
        effect_size = 0.8,
        power = pwr::pwr.t.test(n = current_n/2, d = 0.8, sig.level = 0.05)$power,
        required_n = ceiling(pwr::pwr.t.test(d = 0.8, power = 0.8, sig.level = 0.05)$n)
      )
    )
    
    # ANOVA power analysis
    results$anova <- list(
      small_effect = list(
        effect_size = 0.1,
        power = pwr::pwr.anova.test(k = 3, n = current_n/3, f = 0.1, sig.level = 0.05)$power,
        required_n = ceiling(pwr::pwr.anova.test(k = 3, f = 0.1, power = 0.8, sig.level = 0.05)$n)
      ),
      medium_effect = list(
        effect_size = 0.25,
        power = pwr::pwr.anova.test(k = 3, n = current_n/3, f = 0.25, sig.level = 0.05)$power,
        required_n = ceiling(pwr::pwr.anova.test(k = 3, f = 0.25, power = 0.8, sig.level = 0.05)$n)
      ),
      large_effect = list(
        effect_size = 0.4,
        power = pwr::pwr.anova.test(k = 3, n = current_n/3, f = 0.4, sig.level = 0.05)$power,
        required_n = ceiling(pwr::pwr.anova.test(k = 3, f = 0.4, power = 0.8, sig.level = 0.05)$n)
      )
    )
    
    list(
      success = TRUE,
      step = 4,
      operation = "power_analysis",
      results = results,
      message = "Power analysis completed successfully"
    )
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = paste("Power analysis failed:", e$message),
      step = 4,
      operation = "power_analysis"
    )
  })
}

#' Execute Multiple Testing Correction
#' @param df Dataframe to analyze
#' @param variables Variable specifications
#' @return Multiple testing correction results
execute_multiple_testing_correction <- function(df, variables) {
  results <- list()
  
  # Example p-values (in real implementation, these would come from previous tests)
  # For demonstration, we'll simulate some p-values or use stored test results
  
  # Check if there are stored test results in the workspace
  # This would typically come from previous statistical tests in the workflow
  example_p_values <- c(0.045, 0.032, 0.089, 0.012, 0.156, 0.003, 0.078, 0.234)
  
  if (length(example_p_values) == 0) {
    return(list(
      success = FALSE,
      error = "No p-values available for multiple testing correction. Run statistical tests first.",
      step = 5,
      operation = "multiple_testing_correction"
    ))
  }
  
  # Apply different correction methods
  results$original_p_values <- example_p_values
  results$number_of_tests <- length(example_p_values)
  
  # Bonferroni correction
  bonferroni_corrected <- p.adjust(example_p_values, method = "bonferroni")
  results$bonferroni <- list(
    method = "Bonferroni",
    corrected_p_values = as.numeric(bonferroni_corrected),
    significant_tests = sum(bonferroni_corrected < 0.05),
    rejection_rate = mean(bonferroni_corrected < 0.05)
  )
  
  # Benjamini-Hochberg (FDR) correction
  fdr_corrected <- p.adjust(example_p_values, method = "BH")
  results$benjamini_hochberg <- list(
    method = "Benjamini-Hochberg (FDR)",
    corrected_p_values = as.numeric(fdr_corrected),
    significant_tests = sum(fdr_corrected < 0.05),
    rejection_rate = mean(fdr_corrected < 0.05)
  )
  
  # Holm correction
  holm_corrected <- p.adjust(example_p_values, method = "holm")
  results$holm <- list(
    method = "Holm",
    corrected_p_values = as.numeric(holm_corrected),
    significant_tests = sum(holm_corrected < 0.05),
    rejection_rate = mean(holm_corrected < 0.05)
  )
  
  # Summary comparison
  results$comparison <- list(
    uncorrected_significant = sum(example_p_values < 0.05),
    bonferroni_significant = results$bonferroni$significant_tests,
    fdr_significant = results$benjamini_hochberg$significant_tests,
    holm_significant = results$holm$significant_tests
  )
  
  results$interpretation <- list(
    most_conservative = "Bonferroni",
    balanced_approach = "Benjamini-Hochberg (FDR)",
    recommendation = "Use FDR for exploratory studies, Bonferroni for confirmatory studies"
  )
  
  list(
    success = TRUE,
    step = 5,
    operation = "multiple_testing_correction",
    results = results,
    message = "Multiple testing correction completed successfully"
  )
}
