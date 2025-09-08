# Transformation Agent for RStudio AI
# Handles data transformations like log/sqrt, new variables, merging, etc.

library(base64enc)

# Get available dataframes for transformation
get_available_dataframes <- function() {
  objects <- ls(envir = .GlobalEnv)
  dataframes <- objects[sapply(objects, function(x) {
    obj <- get(x, envir = .GlobalEnv)
    is.data.frame(obj) && nrow(obj) > 0 && ncol(obj) > 0
  })]
  return(dataframes)
}

# Get dataframe information for transformation planning
get_dataframe_info <- function(dataframe_name) {
  tryCatch({
    df <- get(dataframe_name, envir = .GlobalEnv)
    
    if (!is.data.frame(df)) {
      return(list(exists = FALSE, error = "Object is not a dataframe"))
    }
    
    # Get column information
    column_info <- list()
    numeric_cols <- c()
    categorical_cols <- c()
    date_cols <- c()
    
    for (col in names(df)) {
      col_data <- df[[col]]
      
      # Determine column type
      if (is.numeric(col_data)) {
        numeric_cols <- c(numeric_cols, col)
        # Check for skewness and distribution
        if (length(na.omit(col_data)) > 3) {
          skewness <- mean((col_data - mean(col_data, na.rm = TRUE))^3, na.rm = TRUE) / 
                     (sd(col_data, na.rm = TRUE)^3)
          column_info[[col]] <- list(
            type = "numeric",
            missing = sum(is.na(col_data)),
            unique = length(unique(col_data)),
            min = min(col_data, na.rm = TRUE),
            max = max(col_data, na.rm = TRUE),
            mean = mean(col_data, na.rm = TRUE),
            median = median(col_data, na.rm = TRUE),
            skewness = round(skewness, 3),
            needs_transformation = abs(skewness) > 1
          )
        } else {
          column_info[[col]] <- list(
            type = "numeric",
            missing = sum(is.na(col_data)),
            unique = length(unique(col_data)),
            min = min(col_data, na.rm = TRUE),
            max = max(col_data, na.rm = TRUE),
            mean = mean(col_data, na.rm = TRUE),
            median = median(col_data, na.rm = TRUE),
            skewness = NA,
            needs_transformation = FALSE
          )
        }
      } else if (is.factor(col_data) || is.character(col_data)) {
        categorical_cols <- c(categorical_cols, col)
        unique_values <- unique(col_data)
        column_info[[col]] <- list(
          type = "categorical",
          missing = sum(is.na(col_data)),
          unique = length(unique_values),
          levels = if(length(unique_values) <= 20) as.character(unique_values) else paste(length(unique_values), "unique values"),
          most_common = names(sort(table(col_data), decreasing = TRUE))[1]
        )
      } else if (inherits(col_data, "Date") || inherits(col_data, "POSIXt")) {
        date_cols <- c(date_cols, col)
        column_info[[col]] <- list(
          type = "date",
          missing = sum(is.na(col_data)),
          unique = length(unique(col_data)),
          min_date = as.character(min(col_data, na.rm = TRUE)),
          max_date = as.character(max(col_data, na.rm = TRUE))
        )
      } else {
        column_info[[col]] <- list(
          type = "other",
          missing = sum(is.na(col_data)),
          unique = length(unique(col_data))
        )
      }
    }
    
               # Calculate size in MB
           size_mb <- round(as.numeric(object.size(df)) / (2^20), 2)
    
    return(list(
      exists = TRUE,
      name = dataframe_name,
      nrow = nrow(df),
      ncol = ncol(df),
      size_mb = size_mb,
      column_info = column_info,
      numeric_cols = numeric_cols,
      categorical_cols = categorical_cols,
      date_cols = date_cols
    ))
    
  }, error = function(e) {
    return(list(exists = FALSE, error = e$message))
  })
}

# Start transformation agent
start_transformation_agent <- function(dataframe, transformation_options, method_options = NULL, custom_inputs = NULL, selected_variables = NULL) {
  tryCatch({
    # Validate dataframe exists
    if (!exists(dataframe, envir = .GlobalEnv)) {
      return(list(success = FALSE, error = "Dataframe not found"))
    }
    
    df <- get(dataframe, envir = .GlobalEnv)
    if (!is.data.frame(df)) {
      return(list(success = FALSE, error = "Object is not a dataframe"))
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
            return(list(
              success = FALSE,
              error = paste("Selected variables not found in dataframe for", operation, ":", paste(invalid_vars, collapse = ", "))
            ))
          }
        }
      }
    }
    
    # Build workflow steps based on selected options
    workflow_steps <- list()
    step_num <- 1
    
    # Distribution Analysis
    if (transformation_options$distributionAnalysis) {
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = "Analyze variable distributions and identify transformation needs",
        operation = "distribution_analysis"
      )
      step_num <- step_num + 1
    }
    
    # Mathematical Transformations
    if (transformation_options$mathematicalTransformations) {
      # Get the request text for the description
      request_desc <- if (!is.null(method_options) && !is.null(method_options$mathematicalTransformations)) {
        if (method_options$mathematicalTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$mathematicalTransformations)) {
          paste0("Apply mathematical transformations: ", custom_inputs$mathematicalTransformations)
        } else {
          paste0("Apply mathematical transformations: ", method_options$mathematicalTransformations)
        }
      } else {
        "Apply mathematical transformations (log, sqrt, power)"
      }
      
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = request_desc,
        operation = "mathematical_transformations"
      )
      step_num <- step_num + 1
    }
    
    # New Variables
    if (transformation_options$newVariables) {
      # Get the request text for the description
      request_desc <- if (!is.null(method_options) && !is.null(method_options$newVariables)) {
        if (method_options$newVariables == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$newVariables)) {
          paste0("Create new variables: ", custom_inputs$newVariables)
        } else {
          paste0("Create new variables: ", method_options$newVariables)
        }
      } else {
        "Create new variables from existing ones"
      }
      
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = request_desc,
        operation = "new_variables"
      )
      step_num <- step_num + 1
    }
    
    # Categorical Transformations
    if (transformation_options$categoricalTransformations) {
      # Get the request text for the description
      request_desc <- if (!is.null(method_options) && !is.null(method_options$categoricalTransformations)) {
        if (method_options$categoricalTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$categoricalTransformations)) {
          paste0("Transform categorical variables: ", custom_inputs$categoricalTransformations)
        } else {
          paste0("Transform categorical variables: ", method_options$categoricalTransformations)
        }
      } else {
        "Transform categorical variables (recode, combine, dummy)"
      }
      
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = request_desc,
        operation = "categorical_transformations"
      )
      step_num <- step_num + 1
    }
    
    # Date/Time Transformations
    if (transformation_options$dateTimeTransformations) {
      # Get the request text for the description
      request_desc <- if (!is.null(method_options) && !is.null(method_options$dateTimeTransformations)) {
        if (method_options$dateTimeTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$dateTimeTransformations)) {
          paste0("Extract date/time components: ", custom_inputs$dateTimeTransformations)
        } else {
          paste0("Extract date/time components: ", method_options$dateTimeTransformations)
        }
      } else {
        "Extract date/time components and create time variables"
      }
      
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = request_desc,
        operation = "datetime_transformations"
      )
      step_num <- step_num + 1
    }
    
    # Merging & Combining
    if (transformation_options$mergingCombining) {
      # Get the request text for the description
      request_desc <- if (!is.null(method_options) && !is.null(method_options$mergingCombining)) {
        if (method_options$mergingCombining == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$mergingCombining)) {
          paste0("Merge variables: ", custom_inputs$mergingCombining)
        } else {
          paste0("Merge variables: ", method_options$mergingCombining)
        }
      } else {
        "Merge variables and reshape data structure"
      }
      
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = request_desc,
        operation = "merging_combining"
      )
      step_num <- step_num + 1
    }
    
    # Aggregation & Grouping
    if (transformation_options$aggregationGrouping) {
      # Get the request text for the description
      request_desc <- if (!is.null(method_options) && !is.null(method_options$aggregationGrouping)) {
        if (method_options$aggregationGrouping == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$aggregationGrouping)) {
          paste0("Create group summaries: ", custom_inputs$aggregationGrouping)
        } else {
          paste0("Create group summaries: ", method_options$aggregationGrouping)
        }
      } else {
        "Create group summaries and rolling statistics"
      }
      
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = request_desc,
        operation = "aggregation_grouping"
      )
      step_num <- step_num + 1
    }
    
    # Statistical Transformations
    if (transformation_options$statisticalTransformations) {
      # Get the request text for the description
      request_desc <- if (!is.null(method_options) && !is.null(method_options$statisticalTransformations)) {
        if (method_options$statisticalTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$statisticalTransformations)) {
          paste0("Apply statistical transformations: ", custom_inputs$statisticalTransformations)
        } else {
          paste0("Apply statistical transformations: ", method_options$statisticalTransformations)
        }
      } else {
        "Apply statistical transformations (z-score, normalization)"
      }
      
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = request_desc,
        operation = "statistical_transformations"
      )
      step_num <- step_num + 1
    }
    
    # Text Transformations
    if (transformation_options$textTransformations) {
      # Get the request text for the description
      request_desc <- if (!is.null(method_options) && !is.null(method_options$textTransformations)) {
        if (method_options$textTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$textTransformations)) {
          paste0("Transform text variables: ", custom_inputs$textTransformations)
        } else {
          paste0("Transform text variables: ", method_options$textTransformations)
        }
      } else {
        "Transform text variables (case, patterns, extraction)"
      }
      
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = request_desc,
        operation = "text_transformations"
      )
      step_num <- step_num + 1
    }
    
    # Spatial Transformations
    if (transformation_options$spatialTransformations) {
      # Get the request text for the description
      request_desc <- if (!is.null(method_options) && !is.null(method_options$spatialTransformations)) {
        if (method_options$spatialTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$spatialTransformations)) {
          paste0("Apply spatial transformations: ", custom_inputs$spatialTransformations)
        } else {
          paste0("Apply spatial transformations: ", method_options$spatialTransformations)
        }
      } else {
        "Apply spatial and geographic transformations"
      }
      
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = request_desc,
        operation = "spatial_transformations"
      )
      step_num <- step_num + 1
    }
    
    # Custom Transformations
    if (transformation_options$customTransformations) {
      workflow_steps[[step_num]] <- list(
        step = step_num,
        description = "Apply custom user-specified transformations",
        operation = "custom_transformations"
      )
      step_num <- step_num + 1
    }
    
    if (length(workflow_steps) == 0) {
      return(list(success = FALSE, error = "No transformation options selected"))
    }
    
    # Generate first step code
    first_step_code <- generate_transformation_step_code(workflow_steps[[1]], dataframe, method_options, custom_inputs)
    
    # Create workflow ID
    workflow_id <- paste0("transformation_", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    
    return(list(
      success = TRUE,
      message = "Transformation Agent started successfully",
      workflow_id = workflow_id,
      dataframe = dataframe,
      transformation_options = transformation_options,
      method_options = method_options,
      custom_inputs = custom_inputs,
      workflow_steps = workflow_steps,
      total_steps = length(workflow_steps),
      current_step = list(
        step = 1,
        description = workflow_steps[[1]]$description,
        operation = workflow_steps[[1]]$operation,
        code = first_step_code
      )
    ))
    
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# Generate code for a specific transformation step
generate_transformation_step_code <- function(step_info, dataframe, method_options = NULL, custom_inputs = NULL) {
  operation <- step_info$operation
  
  switch(operation,
    "distribution_analysis" = {
      paste0(
        "cat('Distribution Analysis for ", dataframe, "\\n')\n",
        "cat('=====================================\\n\\n')\n",
        "df <- ", dataframe, "\n",
        "numeric_cols <- sapply(df, is.numeric)\n",
        "if(any(numeric_cols)) {\n",
        "  cat('Numeric Variables:\\n')\n",
        "  for(col in names(df)[numeric_cols]) {\n",
        "    cat('\\n', col, ':\\n')\n",
        "    cat('  Missing:', sum(is.na(df[[col]])), '\\n')\n",
        "    cat('  Mean:', round(mean(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "    cat('  Median:', round(median(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "    cat('  SD:', round(sd(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "    if(length(na.omit(df[[col]])) > 3) {\n",
        "      skewness <- mean((df[[col]] - mean(df[[col]], na.rm=TRUE))^3, na.rm=TRUE) / (sd(df[[col]], na.rm=TRUE)^3)\n",
        "      cat('  Skewness:', round(skewness, 3), '\\n')\n",
        "      if(abs(skewness) > 1) {\n",
        "        cat('  ** SUGGESTION: Consider log/sqrt transformation **\\n')\n",
        "      }\n",
        "    }\n",
        "  }\n",
        "} else {\n",
        "  cat('No numeric columns found\\n')\n",
        "}\n",
        "cat('\\nCategorical Variables:\\n')\n",
        "cat_cols <- sapply(df, function(x) is.factor(x) || is.character(x))\n",
        "if(any(cat_cols)) {\n",
        "  for(col in names(df)[cat_cols]) {\n",
        "    cat('\\n', col, ':\\n')\n",
        "    cat('  Missing:', sum(is.na(df[[col]])), '\\n')\n",
        "    cat('  Unique values:', length(unique(df[[col]])), '\\n')\n",
        "    if(length(unique(df[[col]])) <= 10) {\n",
        "      cat('  Values:', paste(unique(df[[col]]), collapse=', '), '\\n')\n",
        "    }\n",
        "  }\n",
        "} else {\n",
        "  cat('No categorical columns found\\n')\n",
        "}"
      )
    },
    
    "mathematical_transformations" = {
      # Get the request text - use custom input if "other" is selected
      request_text <- if (!is.null(method_options) && !is.null(method_options$mathematicalTransformations)) {
        if (method_options$mathematicalTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$mathematicalTransformations)) {
          custom_inputs$mathematicalTransformations
        } else {
          method_options$mathematicalTransformations
        }
      } else {
        "Apply mathematical transformations (log, sqrt, power, polynomial)"
      }
      
      paste0(
        "cat('🔧 Mathematical Transformation Request\\n')\n",
        "cat('Request: ", request_text, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n📊 Analyzing numeric variables for mathematical transformations...\\n')\n",
        "df <- ", dataframe, "\n",
        "numeric_cols <- sapply(df, is.numeric)\n",
        "if(any(numeric_cols)) {\n",
        "  cat('Numeric columns found:', sum(numeric_cols), '\\n')\n",
        "  for(col in names(df)[numeric_cols]) {\n",
        "    cat('\\n--- ', col, ' ---\\n')\n",
        "    cat('Missing values:', sum(is.na(df[[col]])), '\\n')\n",
        "    if(sum(!is.na(df[[col]])) > 0) {\n",
        "      cat('Range:', round(min(df[[col]], na.rm=TRUE), 3), 'to', round(max(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "      cat('Mean:', round(mean(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "      cat('SD:', round(sd(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "      cat('Median:', round(median(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "      # Check for skewness\n",
        "      if(sum(!is.na(df[[col]])) > 3) {\n",
        "        skewness <- mean((df[[col]] - mean(df[[col]], na.rm=TRUE))^3, na.rm=TRUE) / (sd(df[[col]], na.rm=TRUE)^3)\n",
        "        cat('Skewness:', round(skewness, 3), '\\n')\n",
        "        if(abs(skewness) > 1.5) {\n",
        "          cat('SUGGESTION: Highly skewed - consider log/sqrt transformation\\n')\n",
        "        } else if(abs(skewness) > 0.8) {\n",
        "          cat('SUGGESTION: Moderately skewed - consider mild transformation\\n')\n",
        "        } else {\n",
        "          cat('SUGGESTION: Low skewness - transformation may not be needed\\n')\n",
        "        }\n",
        "        # Check for zero/negative values\n",
        "        if(min(df[[col]], na.rm=TRUE) <= 0) {\n",
        "          cat('SUGGESTION: Contains zero/negative values - use sqrt or power transformations\\n')\n",
        "        } else {\n",
        "          cat('SUGGESTION: All positive values - log transformation suitable\\n')\n",
        "        }\n",
        "      }\n",
        "    }\n",
        "  }\n",
        "} else {\n",
        "  cat('No numeric columns found for mathematical transformations\\n')\n",
        "}\n",
        "cat('\\n🤖 Sending detailed mathematical transformation analysis to Claude...\\n')\n",
        "cat('Claude will provide smart transformation code based on this analysis.\\n')"
      )
    },
    
    "new_variables" = {
      # Get the request text - use custom input if "other" is selected
      request_text <- if (!is.null(method_options) && !is.null(method_options$newVariables)) {
        if (method_options$newVariables == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$newVariables)) {
          custom_inputs$newVariables
        } else {
          method_options$newVariables
        }
      } else {
        "Create new variables from existing ones (ratios, combinations, indicators)"
      }
      
      paste0(
        "cat('🔧 New Variables Request\\n')\n",
        "cat('Request: ", request_text, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n📊 Analyzing data structure for new variable creation...\\n')\n",
        "df <- ", dataframe, "\n",
        "numeric_cols <- sapply(df, is.numeric)\n",
        "categorical_cols <- sapply(df, function(x) is.factor(x) || is.character(x))\n",
        "date_cols <- sapply(df, function(x) inherits(x, c('Date', 'POSIXct', 'POSIXt')))\n",
        "cat('Numeric columns found:', sum(numeric_cols), '\\n')\n",
        "cat('Categorical columns found:', sum(categorical_cols), '\\n')\n",
        "cat('Date columns found:', sum(date_cols), '\\n')\n",
        "if(any(numeric_cols)) {\n",
        "  cat('\\nNumeric variables available for combinations:\\n')\n",
        "  for(col in names(df)[numeric_cols]) {\n",
        "    cat('  ', col, '\\n')\n",
        "    cat('    Range:', round(min(df[[col]], na.rm=TRUE), 3), 'to', round(max(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "    cat('    Mean:', round(mean(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "  }\n",
        "  if(sum(numeric_cols) >= 2) {\n",
        "    cat('\\nSUGGESTION: Multiple numeric variables - can create ratios, sums, differences\\n')\n",
        "  }\n",
        "  cat('\\nSUGGESTION: Can create quartile-based categorical variables\\n')\n",
        "  cat('SUGGESTION: Can create high/low indicator variables\\n')\n",
        "}\n",
        "if(any(categorical_cols)) {\n",
        "  cat('\\nCategorical variables available for combinations:\\n')\n",
        "  for(col in names(df)[categorical_cols]) {\n",
        "    cat('  ', col, ' (', length(unique(df[[col]])), ' unique values)\\n')\n",
        "  }\n",
        "  cat('\\nSUGGESTION: Can create interaction terms between categorical variables\\n')\n",
        "}\n",
        "if(any(date_cols)) {\n",
        "  cat('\\nDate variables available for extraction:\\n')\n",
        "  for(col in names(df)[date_cols]) {\n",
        "    cat('  ', col, '\\n')\n",
        "  }\n",
        "  cat('\\nSUGGESTION: Can extract year, month, day, weekday, quarter\\n')\n",
        "}\n",
        "cat('\\n🤖 Sending detailed new variables analysis to Claude...\\n')\n",
        "cat('Claude will provide smart variable creation code based on this analysis.\\n')"
      )
    },
    
    "categorical_transformations" = {
      # Get the request text - use custom input if "other" is selected
      request_text <- if (!is.null(method_options) && !is.null(method_options$categoricalTransformations)) {
        if (method_options$categoricalTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$categoricalTransformations)) {
          custom_inputs$categoricalTransformations
        } else {
          method_options$categoricalTransformations
        }
      } else {
        "Apply categorical transformations (recode, combine, dummy variables)"
      }
      
      paste0(
        "cat('🔧 Categorical Transformation Request\\n')\n",
        "cat('Request: ", request_text, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n📊 Analyzing categorical variables for transformations...\\n')\n",
        "df <- ", dataframe, "\n",
        "categorical_cols <- sapply(df, function(x) is.factor(x) || is.character(x))\n",
        "if(any(categorical_cols)) {\n",
        "  cat('Categorical columns found:', sum(categorical_cols), '\\n')\n",
        "  for(col in names(df)[categorical_cols]) {\n",
        "    cat('\\n--- ', col, ' ---\\n')\n",
        "    cat('Type:', class(df[[col]])[1], '\\n')\n",
        "    cat('Unique values:', length(unique(df[[col]])), '\\n')\n",
        "    cat('Missing values:', sum(is.na(df[[col]])), '\\n')\n",
        "    cat('Sample values:', paste(head(unique(df[[col]]), 5), collapse=', '), '\\n')\n",
        "    if(length(unique(df[[col]])) <= 10) {\n",
        "      freq_table <- table(df[[col]], useNA='ifany')\n",
        "      cat('Frequency table:\\n')\n",
        "      for(val in names(freq_table)) {\n",
        "        cat('  ', val, ':', freq_table[val], '\\n')\n",
        "      }\n",
        "    }\n",
        "    if(length(unique(df[[col]])) > 10) {\n",
        "      cat('SUGGESTION: High cardinality - consider grouping small categories\\n')\n",
        "    } else if(length(unique(df[[col]])) <= 5) {\n",
        "      cat('SUGGESTION: Low cardinality - suitable for dummy encoding\\n')\n",
        "    }\n",
        "    cat('SUGGESTION: Can recode values, combine categories, create dummy variables\\n')\n",
        "  }\n",
        "} else {\n",
        "  cat('No categorical columns found\\n')\n",
        "}\n",
        "cat('\\n🤖 Sending detailed categorical transformation analysis to Claude...\\n')\n",
        "cat('Claude will provide smart transformation code based on this analysis.\\n')"
      )
    },
    
    "datetime_transformations" = {
      # Get the request text - use custom input if "other" is selected
      request_text <- if (!is.null(method_options) && !is.null(method_options$dateTimeTransformations)) {
        if (method_options$dateTimeTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$dateTimeTransformations)) {
          custom_inputs$dateTimeTransformations
        } else {
          method_options$dateTimeTransformations
        }
      } else {
        "Extract date/time components and create time variables"
      }
      
      paste0(
        "cat('🔧 DateTime Transformation Request\\n')\n",
        "cat('Request: ", request_text, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n📊 Analyzing date/time variables for transformations...\\n')\n",
        "df <- ", dataframe, "\n",
        "date_cols <- sapply(df, function(x) inherits(x, c('Date', 'POSIXct', 'POSIXt')))\n",
        "if(any(date_cols)) {\n",
        "  cat('Date/time columns found:', sum(date_cols), '\\n')\n",
        "  for(col in names(df)[date_cols]) {\n",
        "    cat('\\n--- ', col, ' ---\\n')\n",
        "    cat('Type:', class(df[[col]])[1], '\\n')\n",
        "    cat('Missing values:', sum(is.na(df[[col]])), '\\n')\n",
        "    if(sum(!is.na(df[[col]])) > 0) {\n",
        "      cat('Range:', as.character(min(df[[col]], na.rm=TRUE)), 'to', as.character(max(df[[col]], na.rm=TRUE)), '\\n')\n",
        "      cat('Time span:', as.numeric(max(df[[col]], na.rm=TRUE) - min(df[[col]], na.rm=TRUE)), 'days\\n')\n",
        "      cat('Sample values:', paste(as.character(head(df[[col]], 3)), collapse=', '), '\\n')\n",
        "    }\n",
        "    cat('SUGGESTION: Can extract year, month, day, weekday, quarter, hour, minute\\n')\n",
        "    cat('SUGGESTION: Can create time differences, age calculations, seasonal indicators\\n')\n",
        "  }\n",
        "} else {\n",
        "  cat('No date/time columns found\\n')\n",
        "  cat('\\nChecking for potential date columns (character/numeric)...\\n')\n",
        "  potential_date_cols <- names(df)[sapply(df, function(x) is.character(x) || is.numeric(x))]\n",
        "  if(length(potential_date_cols) > 0) {\n",
        "    cat('Potential date columns to check:', paste(potential_date_cols, collapse=', '), '\\n')\n",
        "    cat('SUGGESTION: Convert character/numeric columns to date format if they contain dates\\n')\n",
        "  }\n",
        "}\n",
        "cat('\\n🤖 Sending detailed datetime transformation analysis to Claude...\\n')\n",
        "cat('Claude will provide smart transformation code based on this analysis.\\n')"
      )
    },
    
    "merging_combining" = {
      # Get the request text - use custom input if "other" is selected
      request_text <- if (!is.null(method_options) && !is.null(method_options$mergingCombining)) {
        if (method_options$mergingCombining == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$mergingCombining)) {
          custom_inputs$mergingCombining
        } else {
          method_options$mergingCombining
        }
      } else {
        "Merge variables and reshape data structure"
      }
      
      paste0(
        "cat('🔧 Merging & Combining Request\\n')\n",
        "cat('Request: ", request_text, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n📊 Analyzing data structure for merging and combining opportunities...\\n')\n",
        "df <- ", dataframe, "\n",
        "cat('Current structure:\\n')\n",
        "cat('  Rows:', nrow(df), '\\n')\n",
        "cat('  Columns:', ncol(df), '\\n')\n",
        "cat('  Column types:\\n')\n",
        "for(col in names(df)) {\n",
        "  cat('    ', col, ':', class(df[[col]])[1], '\\n')\n",
        "}\n",
        "cat('\\nColumn analysis for combining opportunities:\\n')\n",
        "categorical_cols <- sapply(df, function(x) is.factor(x) || is.character(x))\n",
        "numeric_cols <- sapply(df, is.numeric)\n",
        "if(any(categorical_cols)) {\n",
        "  cat('\\nCategorical columns (potential for combining):\\n')\n",
        "  for(col in names(df)[categorical_cols]) {\n",
        "    unique_vals <- length(unique(df[[col]]))\n",
        "    cat('  ', col, ' (', unique_vals, ' unique values)\\n')\n",
        "  }\n",
        "}\n",
        "if(any(numeric_cols)) {\n",
        "  cat('\\nNumeric columns (potential for combining):\\n')\n",
        "  for(col in names(df)[numeric_cols]) {\n",
        "    range_vals <- paste(round(range(df[[col]], na.rm=TRUE), 2), collapse=' to ')\n",
        "    cat('  ', col, ' (range: ', range_vals, ')\\n')\n",
        "  }\n",
        "}\n",
        "cat('\\nSUGGESTION: Can combine/split columns, reshape data (wide/long), join datasets\\n')\n",
        "cat('SUGGESTION: Can create composite variables, interaction terms, grouping variables\\n')\n",
        "cat('\\n🤖 Sending detailed merging & combining analysis to Claude...\\n')\n",
        "cat('Claude will provide smart transformation code based on this analysis.\\n')"
      )
    },
    
    "aggregation_grouping" = {
      # Get the request text - use custom input if "other" is selected
      request_text <- if (!is.null(method_options) && !is.null(method_options$aggregationGrouping)) {
        if (method_options$aggregationGrouping == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$aggregationGrouping)) {
          custom_inputs$aggregationGrouping
        } else {
          method_options$aggregationGrouping
        }
      } else {
        "Create group summaries and rolling statistics"
      }
      
      paste0(
        "cat('🔧 Aggregation & Grouping Request\\n')\n",
        "cat('Request: ", request_text, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n📊 Analyzing data for aggregation and grouping opportunities...\\n')\n",
        "df <- ", dataframe, "\n",
        "categorical_cols <- sapply(df, function(x) is.factor(x) || is.character(x))\n",
        "numeric_cols <- sapply(df, is.numeric)\n",
        "if(any(categorical_cols)) {\n",
        "  cat('\\nCategorical grouping variables:\\n')\n",
        "  for(col in names(df)[categorical_cols]) {\n",
        "    unique_groups <- length(unique(df[[col]]))\n",
        "    cat('  ', col, ' (', unique_groups, ' groups)\\n')\n",
        "    if(unique_groups <= 20) {\n",
        "      group_sizes <- table(df[[col]])\n",
        "      cat('    Group sizes:', paste(names(group_sizes), ':', group_sizes, collapse=', '), '\\n')\n",
        "    }\n",
        "  }\n",
        "}\n",
        "if(any(numeric_cols)) {\n",
        "  cat('\\nNumeric variables for aggregation:\\n')\n",
        "  for(col in names(df)[numeric_cols]) {\n",
        "    cat('  ', col, '\\n')\n",
        "    cat('    Range:', round(min(df[[col]], na.rm=TRUE), 3), 'to', round(max(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "    cat('    Mean:', round(mean(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "    cat('    SD:', round(sd(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "  }\n",
        "}\n",
        "cat('\\nSUGGESTION: Can create group summaries (mean, sum, count, min, max)\\n')\n",
        "cat('SUGGESTION: Can create rolling statistics, cumulative measures, lag variables\\n')\n",
        "cat('SUGGESTION: Can create pivot tables, cross-tabulations, frequency tables\\n')\n",
        "cat('\\n🤖 Sending detailed aggregation & grouping analysis to Claude...\\n')\n",
        "cat('Claude will provide smart transformation code based on this analysis.\\n')"
      )
    },
    
    "statistical_transformations" = {
      # Get the request text - use custom input if "other" is selected
      request_text <- if (!is.null(method_options) && !is.null(method_options$statisticalTransformations)) {
        if (method_options$statisticalTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$statisticalTransformations)) {
          custom_inputs$statisticalTransformations
        } else {
          method_options$statisticalTransformations
        }
      } else {
        "Apply statistical transformations (z-score, normalization, ranking, scaling)"
      }
      
      paste0(
        "cat('🔧 Statistical Transformation Request\\n')\n",
        "cat('Request: ", request_text, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n📊 Analyzing numeric variables for statistical transformations...\\n')\n",
        "df <- ", dataframe, "\n",
        "numeric_cols <- sapply(df, is.numeric)\n",
        "if(any(numeric_cols)) {\n",
        "  cat('Numeric columns found:', sum(numeric_cols), '\\n')\n",
        "  for(col in names(df)[numeric_cols]) {\n",
        "    cat('\\n--- ', col, ' ---\\n')\n",
        "    cat('Missing values:', sum(is.na(df[[col]])), '\\n')\n",
        "    if(sum(!is.na(df[[col]])) > 0) {\n",
        "      cat('Range:', round(min(df[[col]], na.rm=TRUE), 3), 'to', round(max(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "      cat('Mean:', round(mean(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "      cat('SD:', round(sd(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "      cat('Median:', round(median(df[[col]], na.rm=TRUE), 3), '\\n')\n",
        "      # Check for skewness\n",
        "      if(sum(!is.na(df[[col]])) > 3) {\n",
        "        skewness <- mean((df[[col]] - mean(df[[col]], na.rm=TRUE))^3, na.rm=TRUE) / (sd(df[[col]], na.rm=TRUE)^3)\n",
        "        cat('Skewness:', round(skewness, 3), '\\n')\n",
        "        if(abs(skewness) > 1.5) {\n",
        "          cat('SUGGESTION: Highly skewed - consider robust scaling or rank transformation\\n')\n",
        "        } else if(abs(skewness) > 0.8) {\n",
        "          cat('SUGGESTION: Moderately skewed - consider z-score or min-max scaling\\n')\n",
        "        } else {\n",
        "          cat('SUGGESTION: Low skewness - standard z-score transformation suitable\\n')\n",
        "        }\n",
        "        # Check for outliers\n",
        "        q1 <- quantile(df[[col]], 0.25, na.rm=TRUE)\n",
        "        q3 <- quantile(df[[col]], 0.75, na.rm=TRUE)\n",
        "        iqr <- q3 - q1\n",
        "        outlier_threshold <- 1.5 * iqr\n",
        "        outliers <- sum(df[[col]] < (q1 - outlier_threshold) | df[[col]] > (q3 + outlier_threshold), na.rm=TRUE)\n",
        "        if(outliers > 0) {\n",
        "          cat('SUGGESTION: Contains', outliers, 'outliers - consider robust scaling\\n')\n",
        "        }\n",
        "      }\n",
        "    }\n",
        "    cat('SUGGESTION: Can apply z-score, min-max, robust scaling, rank transformation\\n')\n",
        "  }\n",
        "} else {\n",
        "  cat('No numeric columns found for statistical transformations\\n')\n",
        "}\n",
        "cat('\\n🤖 Sending detailed statistical transformation analysis to Claude...\\n')\n",
        "cat('Claude will provide smart transformation code based on this analysis.\\n')"
      )
    },
    
    "text_transformations" = {
      # Get the request text - use custom input if "other" is selected
      request_text <- if (!is.null(method_options) && !is.null(method_options$textTransformations)) {
        if (method_options$textTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$textTransformations)) {
          custom_inputs$textTransformations
        } else {
          method_options$textTransformations
        }
      } else {
        "Transform text variables (case changes, pattern replacement, text extraction)"
      }
      
      paste0(
        "cat('🔧 Text Transformation Request\\n')\n",
        "cat('Request: ", request_text, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n📊 Analyzing text variables for transformations...\\n')\n",
        "df <- ", dataframe, "\n",
        "text_cols <- sapply(df, function(x) is.character(x) || is.factor(x))\n",
        "if(any(text_cols)) {\n",
        "  cat('Text columns found:', sum(text_cols), '\\n')\n",
        "  for(col in names(df)[text_cols]) {\n",
        "    cat('\\n--- ', col, ' ---\\n')\n",
        "    cat('Type:', class(df[[col]])[1], '\\n')\n",
        "    cat('Missing values:', sum(is.na(df[[col]])), '\\n')\n",
        "    cat('Unique values:', length(unique(df[[col]])), '\\n')\n",
        "    if(sum(!is.na(df[[col]])) > 0) {\n",
        "      # Text length analysis\n",
        "      text_lengths <- nchar(as.character(df[[col]]), type='chars')\n",
        "      cat('Text length - Mean:', round(mean(text_lengths, na.rm=TRUE), 1), 'chars, Range:', min(text_lengths, na.rm=TRUE), 'to', max(text_lengths, na.rm=TRUE), '\\n')\n",
        "      # Sample values\n",
        "      sample_vals <- head(unique(df[[col]]), 3)\n",
        "      cat('Sample values:', paste(sample_vals, collapse=', '), '\\n')\n",
        "      # Pattern detection\n",
        "      sample_text <- paste(sample_vals, collapse=' ')\n",
        "      has_uppercase <- any(grepl('[A-Z]', sample_text))\n",
        "      has_lowercase <- any(grepl('[a-z]', sample_text))\n",
        "      has_numbers <- any(grepl('[0-9]', sample_text))\n",
        "      has_special <- any(grepl('[^A-Za-z0-9[:space:]]', sample_text))\n",
        "      cat('Patterns detected:\\n')\n",
        "      cat('  Uppercase:', if(has_uppercase) 'Yes' else 'No', '\\n')\n",
        "      cat('  Lowercase:', if(has_lowercase) 'Yes' else 'No', '\\n')\n",
        "      cat('  Numbers:', if(has_numbers) 'Yes' else 'No', '\\n')\n",
        "      cat('  Special chars:', if(has_special) 'Yes' else 'No', '\\n')\n",
        "      # Word count analysis\n",
        "      word_counts <- sapply(strsplit(as.character(df[[col]]), '[[:space:]]+'), length)\n",
        "      cat('Word count - Mean:', round(mean(word_counts, na.rm=TRUE), 1), 'words, Range:', min(word_counts, na.rm=TRUE), 'to', max(word_counts, na.rm=TRUE), '\\n')\n",
        "    }\n",
        "    cat('SUGGESTION: Can change case (toupper, tolower, toTitleCase)\\n')\n",
        "    cat('SUGGESTION: Can clean whitespace, remove special characters, extract patterns\\n')\n",
        "    cat('SUGGESTION: Can split text, extract words, create word counts\\n')\n",
        "  }\n",
        "} else {\n",
        "  cat('No text columns found\\n')\n",
        "}\n",
        "cat('\\n🤖 Sending detailed text transformation analysis to Claude...\\n')\n",
        "cat('Claude will provide smart transformation code based on this analysis.\\n')"
      )
    },
    
    "spatial_transformations" = {
      # Get the request text - use custom input if "other" is selected
      request_text <- if (!is.null(method_options) && !is.null(method_options$spatialTransformations)) {
        if (method_options$spatialTransformations == "other" && !is.null(custom_inputs) && !is.null(custom_inputs$spatialTransformations)) {
          custom_inputs$spatialTransformations
        } else {
          method_options$spatialTransformations
        }
      } else {
        "Apply spatial and geographic transformations"
      }
      
      paste0(
        "cat('🔧 Spatial Transformation Request\\n')\n",
        "cat('Request: ", request_text, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n📊 Analyzing data for spatial transformation opportunities...\\n')\n",
        "df <- ", dataframe, "\n",
        "cat('Current columns:', paste(names(df), collapse=', '), '\\n')\n",
        "cat('\\nChecking for potential coordinate columns...\\n')\n",
        "numeric_cols <- sapply(df, is.numeric)\n",
        "if(any(numeric_cols)) {\n",
        "  cat('Numeric columns (potential coordinates):\\n')\n",
        "  for(col in names(df)[numeric_cols]) {\n",
        "    cat('  ', col, '\\n')\n",
        "    cat('    Range:', round(min(df[[col]], na.rm=TRUE), 6), 'to', round(max(df[[col]], na.rm=TRUE), 6), '\\n')\n",
        "    # Check if values look like coordinates\n",
        "    if(min(df[[col]], na.rm=TRUE) >= -180 && max(df[[col]], na.rm=TRUE) <= 180) {\n",
        "      cat('    SUGGESTION: Values in longitude range (-180 to 180)\\n')\n",
        "    } else if(min(df[[col]], na.rm=TRUE) >= -90 && max(df[[col]], na.rm=TRUE) <= 90) {\n",
        "      cat('    SUGGESTION: Values in latitude range (-90 to 90)\\n')\n",
        "    } else if(min(df[[col]], na.rm=TRUE) >= 0 && max(df[[col]], na.rm=TRUE) <= 1000000) {\n",
        "      cat('    SUGGESTION: Values in UTM/projected coordinate range\\n')\n",
        "    }\n",
        "  }\n",
        "}\n",
        "cat('\\nChecking for spatial package availability...\\n')\n",
        "spatial_packages <- c('sf', 'sp', 'rgdal', 'rgeos')\n",
        "available_packages <- spatial_packages[sapply(spatial_packages, function(pkg) requireNamespace(pkg, quietly=TRUE))]\n",
        "if(length(available_packages) > 0) {\n",
        "  cat('Available spatial packages:', paste(available_packages, collapse=', '), '\\n')\n",
        "} else {\n",
        "  cat('No spatial packages detected - may need to install sf, sp, or rgdal\\n')\n",
        "}\n",
        "cat('\\nSUGGESTION: Can convert coordinates between systems (lat/lon, UTM, etc.)\\n')\n",
        "cat('SUGGESTION: Can calculate distances, buffers, spatial joins\\n')\n",
        "cat('SUGGESTION: Can create spatial objects, perform geometric operations\\n')\n",
        "cat('\\n🤖 Sending detailed spatial transformation analysis to Claude...\\n')\n",
        "cat('Claude will provide smart transformation code based on this analysis.\\n')"
      )
    },
    
    "custom_transformations" = {
      paste0(
        "cat('🔧 Custom Transformation Request\\n')\n",
        "cat('Request: ", if (!is.null(custom_inputs) && !is.null(custom_inputs$customTransformations)) custom_inputs$customTransformations else "Custom transformation request", "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n🤖 Sending custom transformation request to Claude...\\n')\n",
        "cat('Claude will provide smart transformation code based on your specific request.\\n')"
      )
    },
    
    # Default case for unknown operations
    {
      paste0(
        "cat('🔧 Unknown Transformation Request\\n')\n",
        "cat('Request: ", operation, "\\n')\n",
        "cat('Dataset: ", dataframe, " (", nrow(get(dataframe, envir = .GlobalEnv)), " rows × ", ncol(get(dataframe, envir = .GlobalEnv)), " columns)\\n')\n",
        "cat('\\n⚠️  Unknown operation: ", operation, "\\n')\n",
        "cat('Available operations: distribution_analysis, mathematical_transformations, new_variables, categorical_transformations, datetime_transformations, merging_combining, aggregation_grouping, statistical_transformations, text_transformations, spatial_transformations, custom_transformations\\n')\n",
        "cat('\\n🤖 Sending request to Claude for custom handling...\\n')\n",
        "cat('Claude will provide appropriate transformation code for this request.\\n')"
      )
    }
  )
}

# Get next transformation step code
get_next_transformation_step_code <- function(step_info, dataframe, method_options = NULL, custom_inputs = NULL, selected_variables = NULL) {
  # Handle case where step_info is just a string (operation name)
  if (is.character(step_info)) {
    step_info <- list(operation = step_info)
  }
  
  return(generate_transformation_step_code(step_info, dataframe, method_options, custom_inputs))
}
