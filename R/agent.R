# RgentAI Agent Functions
# Specialized agent functionality for autonomous R workflows

# Source the statistical agent functions
if (file.exists("statistical_agent.R")) {
  source("statistical_agent.R")
}

#' Get Available DataFrames in Global Environment
#' @return Character vector of DataFrame names
get_available_dataframes <- function() {
  tryCatch({
    env_objects <- ls(globalenv())
    dataframes <- character(0)
    
    for (obj_name in env_objects) {
      obj <- get(obj_name, envir = globalenv())
      if (is.data.frame(obj)) {
        dataframes <- c(dataframes, obj_name)
      }
    }
    
    return(dataframes)
  }, error = function(e) {
    return(character(0))
  })
}

#' Get All Available Objects in Global Environment with Types
#' @return List with objects categorized by type, dataframes first
get_available_objects <- function() {
  tryCatch({
    env_objects <- ls(globalenv())
    objects_list <- list()
    
    # Categorize objects by type
    for (obj_name in env_objects) {
      tryCatch({
        obj <- get(obj_name, envir = globalenv())
        obj_type <- class(obj)[1]
        
        # Determine category
        if (is.data.frame(obj)) {
          category <- "dataframe"
        } else if (is.list(obj) && !is.data.frame(obj)) {
          category <- "list"
        } else if (is.vector(obj) && !is.list(obj)) {
          category <- "vector"
        } else if (is.function(obj)) {
          category <- "function"
        } else if (is.matrix(obj)) {
          category <- "matrix"
        } else if (is.array(obj) && !is.matrix(obj)) {
          category <- "array"
        } else if (inherits(obj, "ts")) {
          category <- "ts"
        } else if (is.environment(obj)) {
          category <- "environment"
        } else {
          category <- "other"
        }
        
        # Get size/length info
        size_info <- tryCatch({
          if (is.data.frame(obj)) {
            list(nrow = nrow(obj), ncol = ncol(obj))
          } else if (is.list(obj) || is.vector(obj) || is.matrix(obj) || is.array(obj)) {
            list(length = length(obj))
          } else if (is.function(obj)) {
            list(formals = length(formals(obj)))
          } else {
            list()
          }
        }, error = function(e) {
          list()
        })
        
        objects_list[[obj_name]] <- list(
          name = obj_name,
          type = obj_type,
          category = category,
          size_info = size_info
        )
      }, error = function(e) {
        # Skip objects that can't be accessed
        NULL
      })
    }
    
    # Sort: dataframes first, then by name
    df_names <- names(objects_list)[sapply(objects_list, function(x) x$category == "dataframe")]
    other_names <- names(objects_list)[sapply(objects_list, function(x) x$category != "dataframe")]
    
    # Sort each group alphabetically
    df_names <- sort(df_names)
    other_names <- sort(other_names)
    
    # Return dataframes first, then others
    result <- c(df_names, other_names)
    return(result)
  }, error = function(e) {
    return(character(0))
  })
}

#' Get Basic Information About a DataFrame
#' @param df_name Name of the DataFrame
#' @return List with DataFrame information
get_dataframe_info <- function(df_name) {
  if (!exists(df_name, envir = globalenv())) {
    return(list(
      exists = FALSE,
      error = paste("DataFrame", df_name, "not found")
    ))
  }
  
  tryCatch({
    df <- get(df_name, envir = globalenv())
    
    if (!is.data.frame(df)) {
      return(list(
        exists = FALSE,
        error = paste(df_name, "is not a data.frame")
      ))
    }
    
    # Get column information
    col_info <- sapply(df, function(x) {
      list(
        type = class(x)[1],
        missing = sum(is.na(x)),
        unique_values = length(unique(x))
      )
    }, simplify = FALSE)
    
    return(list(
      exists = TRUE,
      name = df_name,
      nrow = nrow(df),
      ncol = ncol(df),
      columns = names(df),
      column_info = col_info,
      size_mb = round(as.numeric(object.size(df)) / 1024^2, 2)
    ))
  }, error = function(e) {
    return(list(
      exists = FALSE,
      error = paste("Error analyzing", df_name, ":", e$message)
    ))
  })
}

#' Enhanced Context Capture for Agent Execution
#' @return List with current R environment state optimized for agents
capture_agent_context <- function() {
  base_context <- capture_context()  # Reuse existing function
  
  # Add agent-specific information
  agent_context <- list(
    base_context = base_context,
    available_dataframes = get_available_dataframes(),
    dataframe_info = list()
  )
  
  # Get info for each available DataFrame (lightweight summary only)
  for (df_name in agent_context$available_dataframes) {
    df_info <- get_dataframe_info(df_name)
    if (df_info$exists) {
      # Store only essential info to minimize token usage
      agent_context$dataframe_info[[df_name]] <- list(
        nrow = df_info$nrow,
        ncol = df_info$ncol,
        columns = df_info$columns,
        missing_counts = sapply(df_info$column_info, function(x) x$missing)
      )
    }
  }
  
  return(agent_context)
}

#' Execute Cleaning Agent Workflow
#' @param dataframe Name of the DataFrame to clean
#' @param na_handling Method for handling NAs: "dont", "mean", "median", "mode", "remove"
#' @param iteration Current iteration number (1-5)
#' @return List with generated R code and description
execute_cleaning_agent <- function(dataframe, na_handling = "median", iteration = 1) {
  # Validate inputs
  if (!exists(dataframe, envir = globalenv())) {
    return(list(
      success = FALSE,
      error = paste("DataFrame", dataframe, "not found in environment"),
      code = NULL
    ))
  }
  
  # Get DataFrame info for context
  df_info <- get_dataframe_info(dataframe)
  if (!df_info$exists) {
    return(list(
      success = FALSE,
      error = df_info$error,
      code = NULL
    ))
  }
  
  # Define the cleaning workflow steps
  cleaning_steps <- list(
    list(
      step = 1,
      name = "examine_structure",
      description = "Examine data structure and types",
      code = paste0("str(", dataframe, ")")
    ),
    list(
      step = 2,
      name = "analyze_summary",
      description = "Analyze summary statistics and missing values",
      code = paste0("summary(", dataframe, ")")
    ),
    list(
      step = 3,
      name = "handle_nas",
      description = paste("Handle missing values using", na_handling, "method"),
      code = generate_na_handling_code(dataframe, na_handling, df_info)
    ),
    list(
      step = 4,
      name = "fix_data_types",
      description = "Convert data types where appropriate",
      code = generate_type_conversion_code(dataframe, df_info)
    ),
    list(
      step = 5,
      name = "remove_outliers",
      description = "Remove outliers using IQR method",
      code = generate_outlier_removal_code(dataframe, df_info)
    )
  )
  
  # Return current step
  if (iteration <= length(cleaning_steps)) {
    current_step <- cleaning_steps[[iteration]]
    return(list(
      success = TRUE,
      step = iteration,
      total_steps = length(cleaning_steps),
      name = current_step$name,
      description = current_step$description,
      code = current_step$code,
      completed = iteration >= length(cleaning_steps)
    ))
  } else {
    return(list(
      success = TRUE,
      step = iteration,
      total_steps = length(cleaning_steps),
      name = "completed",
      description = "Data cleaning workflow completed",
      code = paste0("print('Data cleaning completed for ", dataframe, "')"),
      completed = TRUE
    ))
  }
}

#' Generate NA Handling Code
#' @param dataframe Name of the DataFrame
#' @param method Method for handling NAs
#' @param df_info DataFrame information
#' @return Character string with R code
generate_na_handling_code <- function(dataframe, method, df_info) {
  if (method == "dont") {
    return(paste0("# No NA handling requested"))
  }
  
  # Find columns with missing values
  cols_with_nas <- names(df_info$column_info)[
    sapply(df_info$column_info, function(x) x$missing > 0)
  ]
  
  if (length(cols_with_nas) == 0) {
    return(paste0("# No missing values found in ", dataframe))
  }
  
  if (method == "remove") {
    return(paste0(dataframe, " <- ", dataframe, "[complete.cases(", dataframe, "), ]"))
  }
  
  # Generate code for each column with NAs
  code_lines <- c()
  for (col in cols_with_nas) {
    col_type <- df_info$column_info[[col]]$type
    
    if (col_type %in% c("numeric", "integer")) {
      if (method == "mean") {
        code_lines <- c(code_lines, 
          paste0(dataframe, "$", col, "[is.na(", dataframe, "$", col, ")] <- mean(", 
                 dataframe, "$", col, ", na.rm = TRUE)"))
      } else if (method == "median") {
        code_lines <- c(code_lines,
          paste0(dataframe, "$", col, "[is.na(", dataframe, "$", col, ")] <- median(", 
                 dataframe, "$", col, ", na.rm = TRUE)"))
      }
    } else if (col_type %in% c("character", "factor")) {
      if (method == "mode") {
        code_lines <- c(code_lines,
          paste0("mode_val <- names(sort(table(", dataframe, "$", col, "), decreasing = TRUE))[1]; ",
                 dataframe, "$", col, "[is.na(", dataframe, "$", col, ")] <- mode_val"))
      }
    }
  }
  
  if (length(code_lines) == 0) {
    return(paste0("# No suitable columns for ", method, " imputation"))
  }
  
  return(paste(code_lines, collapse = "; "))
}

#' Generate Data Type Conversion Code
#' @param dataframe Name of the DataFrame
#' @param df_info DataFrame information
#' @return Character string with R code
generate_type_conversion_code <- function(dataframe, df_info) {
  # This is a simplified version - in practice, you'd want more sophisticated detection
  code_lines <- c()
  
  for (col in names(df_info$column_info)) {
    col_info <- df_info$column_info[[col]]
    
    # Example: Convert character columns that look like numbers
    if (col_info$type == "character" && col_info$unique_values < (df_info$nrow * 0.8)) {
      code_lines <- c(code_lines,
        paste0("# Consider converting ", col, " to factor if appropriate"))
    }
  }
  
  if (length(code_lines) == 0) {
    return(paste0("# Data types look appropriate for ", dataframe))
  }
  
  return(paste(code_lines, collapse = "; "))
}

#' Generate Outlier Removal Code
#' @param dataframe Name of the DataFrame
#' @param df_info DataFrame information
#' @return Character string with R code
generate_outlier_removal_code <- function(dataframe, df_info) {
  # Find numeric columns
  numeric_cols <- names(df_info$column_info)[
    sapply(df_info$column_info, function(x) x$type %in% c("numeric", "integer"))
  ]
  
  if (length(numeric_cols) == 0) {
    return(paste0("# No numeric columns found for outlier removal in ", dataframe))
  }
  
  # For simplicity, apply IQR method to all numeric columns
  code_lines <- c()
  for (col in numeric_cols) {
    code_lines <- c(code_lines,
      paste0("Q1_", col, " <- quantile(", dataframe, "$", col, ", 0.25, na.rm = TRUE); ",
             "Q3_", col, " <- quantile(", dataframe, "$", col, ", 0.75, na.rm = TRUE); ",
             "IQR_", col, " <- Q3_", col, " - Q1_", col, "; ",
             dataframe, " <- ", dataframe, "[", dataframe, "$", col, " >= Q1_", col, " - 1.5*IQR_", col, 
             " & ", dataframe, "$", col, " <= Q3_", col, " + 1.5*IQR_", col, " | is.na(", dataframe, "$", col, "), ]"))
  }
  
  return(paste(code_lines, collapse = "; "))
}

#' Generate R Code for a Specific Cleaning Step
#' @param step_info List containing step information
#' @param dataframe Name of the DataFrame
#' @param na_handling Method for handling NAs
#' @param method_options List of method choices for each operation
#' @param custom_inputs List of custom text inputs for "Other" methods
#' @return Character string with R code
generate_step_code <- function(step_info, dataframe, na_handling, method_options = NULL, custom_inputs = NULL, selected_variables = NULL) {
  operation <- step_info$operation
  
  switch(operation,
    "data_types" = {
      # Get data type method from options
      data_type_method <- if (!is.null(method_options) && !is.null(method_options$dataTypes)) method_options$dataTypes else "automatic"
      custom_data_type_input <- if (!is.null(custom_inputs) && !is.null(custom_inputs$dataTypes)) custom_inputs$dataTypes else ""
      
      if (data_type_method == "other" && nchar(custom_data_type_input) > 0) {
        # Use custom data type conversion
        paste0("cat('Custom Data Type Analysis\\n')\n",
               "cat('Custom method: ", custom_data_type_input, "\\n')\n",
               "str(", dataframe, ")\n",
               "cat('Current column types:', paste(sapply(", dataframe, ", class), collapse=', '), '\\n')\n",
               "cat('Custom type conversion logic would be implemented here based on: ", custom_data_type_input, "\\n')")
      } else {
        # Default automatic detection
        paste0("cat('Data Types Analysis (Automatic Detection)\\n')\n",
               "str(", dataframe, ")\n",
               "cat('Column types:', paste(sapply(", dataframe, ", class), collapse=', '), '\\n')\n",
               "cat('Recommendations for type conversion:\\n')\n",
               "for(col in names(", dataframe, ")) {\n",
               "  current_type <- class(", dataframe, "[[col]])[1]\n",
               "  if(current_type == 'character' && all(!is.na(as.numeric(", dataframe, "[[col]]))) && !any(grepl('[a-zA-Z]', ", dataframe, "[[col]]))) {\n",
               "    cat('  ', col, ': Consider converting to numeric\\n')\n",
               "  } else if(current_type == 'numeric' && length(unique(", dataframe, "[[col]])) <= 10) {\n",
               "    cat('  ', col, ': Consider converting to factor\\n')\n",
               "  }\n",
               "}")
      }
    },
    "missing_values" = {
      # Get missing values method from options
      missing_method <- if (!is.null(method_options) && !is.null(method_options$missingValues)) method_options$missingValues else "dont"
      custom_missing_input <- if (!is.null(custom_inputs) && !is.null(custom_inputs$missingValues)) custom_inputs$missingValues else ""
      
      # Get selected variables for analysis
      selected_vars <- if (!is.null(selected_variables) && !is.null(selected_variables$`missing-values`) && length(selected_variables$`missing-values`) > 0) {
        if (is.data.frame(selected_variables$`missing-values`)) {
          selected_variables$`missing-values`$name
        } else {
          sapply(selected_variables$`missing-values`, function(x) {
            if (is.list(x) && !is.null(x$name)) x$name else x
          })
        }
      } else {
        names(get(dataframe, envir = globalenv()))
      }
      
      if (missing_method == "other" && nchar(custom_missing_input) > 0) {
        # Use custom missing values method
        paste0("cat('Custom Missing Values Analysis\\n')\n",
               "cat('Custom method: ", custom_missing_input, "\\n')\n",
               "cat('Missing values per column:\\n')\n",
               "missing_counts <- colSums(is.na(", dataframe, "))\n",
               "missing_percentages <- round(missing_counts / nrow(", dataframe, ") * 100, 2)\n",
               "for(col in ", if (is.character(selected_vars)) paste0("c(", paste0("'", selected_vars, "'", collapse = ", "), ")") else selected_vars, ") {\n",
               "  if(missing_counts[col] > 0) {\n",
               "    cat('  ', col, ':', missing_counts[col], '(', missing_percentages[col], '%)\\n')\n",
               "  }\n",
               "}\n",
               "cat('Custom missing value handling logic would be implemented here based on: ", custom_missing_input, "\\n')")
      } else {
        # Standard missing values analysis
        paste0("cat('Missing Values Analysis\\n')\n",
               "cat('Missing values per column:\\n')\n",
               "missing_counts <- colSums(is.na(", dataframe, "))\n",
               "missing_percentages <- round(missing_counts / nrow(", dataframe, ") * 100, 2)\n",
               "for(col in ", if (is.character(selected_vars)) paste0("c(", paste0("'", selected_vars, "'", collapse = ", "), ")") else selected_vars, ") {\n",
               "  if(missing_counts[col] > 0) {\n",
               "    cat('  ', col, ':', missing_counts[col], '(', missing_percentages[col], '%)\\n')\n",
               "  }\n",
               "}\n",
               "cat('Total rows with any missing values:', sum(rowSums(is.na(", dataframe, ")) > 0), '\\n')")
      }
    },
    "duplicates" = {
      paste0("cat('Duplicate Analysis\\n')\n",
             "cat('Total duplicates:', sum(duplicated(", dataframe, ")), '\\n')\n",
             "cat('Duplicate rows:', nrow(", dataframe, "[duplicated(", dataframe, "), ]), '\\n')")
    },
    "outliers" = {
      # Get outlier method from options
      outlier_method <- if (!is.null(method_options) && !is.null(method_options$outliers)) method_options$outliers else "iqr"
      custom_outlier_input <- if (!is.null(custom_inputs) && !is.null(custom_inputs$outliers)) custom_inputs$outliers else ""
      
      # Get selected numeric variables for outlier analysis
      selected_numeric_vars <- if (!is.null(selected_variables) && !is.null(selected_variables$outliers) && length(selected_variables$outliers) > 0) {
        if (is.data.frame(selected_variables$outliers)) {
          numeric_vars <- selected_variables$outliers$name[selected_variables$outliers$type == "numeric"]
        } else {
          numeric_vars <- sapply(selected_variables$outliers, function(x) {
            if (is.list(x) && !is.null(x$type) && x$type == "numeric") {
              x$name
            } else {
              NULL
            }
          })
          numeric_vars <- numeric_vars[!sapply(numeric_vars, is.null)]
        }
        if (length(numeric_vars) > 0) numeric_vars else NULL
      } else {
        NULL
      }
      
      if (outlier_method == "other" && nchar(custom_outlier_input) > 0) {
        # Use custom outlier method
        paste0("cat('Custom Outlier Analysis\\n')\n",
               "cat('Custom method: ", custom_outlier_input, "\\n')\n",
               "numeric_cols <- sapply(", dataframe, ", is.numeric)\n",
               "if(any(numeric_cols)) {\n",
               "  cat('Applying custom outlier detection to numeric columns\\n')\n",
               "  for(col in names(", dataframe, ")[numeric_cols]) {\n",
               "    cat('Analyzing column:', col, '\\n')\n",
               "    # Custom outlier logic would be implemented here based on: ", custom_outlier_input, "\\n')\n",
               "  }\n",
               "} else { cat('No numeric columns for outlier analysis\\n') }")
      } else if (outlier_method == "zscore") {
        # Z-Score method
        paste0("cat('Z-Score Outlier Analysis (3 sigma threshold)\\n')\\n",
               "numeric_cols <- sapply(", dataframe, ", is.numeric)\n",
               "if(any(numeric_cols)) {\n",
               "  for(col in names(", dataframe, ")[numeric_cols]) {\n",
               "    z_scores <- abs(scale(", dataframe, "[[col]])[!is.na(", dataframe, "[[col]])])\n",
               "    outliers <- sum(z_scores > 3, na.rm = TRUE)\n",
               "    cat(col, ':', outliers, 'outliers (z-score > 3)\\n')\n",
               "  }\n",
               "} else { cat('No numeric columns for outlier analysis\\n') }")
      } else if (outlier_method == "modified_zscore") {
        # Modified Z-Score method
        paste0("cat('Modified Z-Score Outlier Analysis (3.5 MAD threshold)\\n')\n",
               "numeric_cols <- sapply(", dataframe, ", is.numeric)\n",
               "if(any(numeric_cols)) {\n",
               "  for(col in names(", dataframe, ")[numeric_cols]) {\n",
               "    x <- ", dataframe, "[[col]]\n",
               "    x_clean <- x[!is.na(x)]\n",
               "    if(length(x_clean) > 0) {\n",
               "      median_x <- median(x_clean)\n",
               "      mad_x <- median(abs(x_clean - median_x))\n",
               "      if(mad_x > 0) {\n",
               "        modified_z <- 0.6745 * (x_clean - median_x) / mad_x\n",
               "        outliers <- sum(abs(modified_z) > 3.5, na.rm = TRUE)\n",
               "        cat(col, ':', outliers, 'outliers (modified z-score > 3.5)\\n')\n",
               "      } else { cat(col, ': MAD is 0, cannot compute outliers\\n') }\n",
               "    } else { cat(col, ': No non-NA values\\n') }\n",
               "  }\n",
               "} else { cat('No numeric columns for outlier analysis\\n') }")
      } else {
        # Default IQR method
        if (!is.null(selected_numeric_vars)) {
          # Use selected numeric variables
          paste0("cat('IQR Outlier Analysis (1.5x multiplier)\\n')\n",
                 "for(col in c(", paste0("'", selected_numeric_vars, "'", collapse = ", "), ")) {\n",
                 "  Q1 <- quantile(", dataframe, "[[col]], 0.25, na.rm = TRUE)\n",
                 "  Q3 <- quantile(", dataframe, "[[col]], 0.75, na.rm = TRUE)\n",
                 "  IQR <- Q3 - Q1\n",
                 "  outliers <- sum(", dataframe, "[[col]] < Q1 - 1.5*IQR | ", dataframe, "[[col]] > Q3 + 1.5*IQR, na.rm = TRUE)\n",
                 "  cat(col, ':', outliers, 'outliers\\n')\n",
                 "}")
        } else {
          # Use all numeric columns
          paste0("cat('IQR Outlier Analysis (1.5x multiplier)\\n')\n",
                 "numeric_cols <- sapply(", dataframe, ", is.numeric)\n",
                 "if(any(numeric_cols)) {\n",
                 "  for(col in names(", dataframe, ")[numeric_cols]) {\n",
                 "    Q1 <- quantile(", dataframe, "[[col]], 0.25, na.rm = TRUE)\n",
                 "    Q3 <- quantile(", dataframe, "[[col]], 0.75, na.rm = TRUE)\n",
                 "    IQR <- Q3 - Q1\n",
                 "    outliers <- sum(", dataframe, "[[col]] < Q1 - 1.5*IQR | ", dataframe, "[[col]] > Q3 + 1.5*IQR, na.rm = TRUE)\n",
                 "    cat(col, ':', outliers, 'outliers\\n')\n",
                 "  }\n",
                 "} else { cat('No numeric columns for outlier analysis\\n') }")
        }
      }
    },
    "column_names" = {
      # Get column naming method from options
      naming_method <- if (!is.null(method_options) && !is.null(method_options$columnNames)) method_options$columnNames else "snake_case"
      custom_naming_input <- if (!is.null(custom_inputs) && !is.null(custom_inputs$columnNames)) custom_inputs$columnNames else ""
      
      if (naming_method == "other" && nchar(custom_naming_input) > 0) {
        # Use custom naming convention
        paste0("cat('Custom Column Naming Analysis\\n')\n",
               "cat('Custom convention: ", custom_naming_input, "\\n')\n",
               "cat('Current names:', paste(names(", dataframe, "), collapse=', '), '\\n')\n",
               "cat('Custom naming logic would be implemented here based on: ", custom_naming_input, "\\n')")
      } else if (naming_method == "camelCase") {
        # camelCase naming
        paste0("cat('camelCase Column Naming Analysis\\n')\n",
               "cat('Current names:', paste(names(", dataframe, "), collapse=', '), '\\n')\n",
               "suggested_names <- sapply(names(", dataframe, "), function(x) {\n",
               "  # Convert to camelCase: first word lowercase, subsequent words capitalized\n",
               "  words <- strsplit(tolower(x), '[^a-zA-Z0-9]+')[[1]]\n",
               "  if(length(words) > 1) {\n",
               "    paste0(words[1], paste0(toupper(substring(words[-1], 1, 1)), tolower(substring(words[-1], 2))))\n",
               "  } else {\n",
               "    tolower(words[1])\n",
               "  }\n",
               "})\n",
               "cat('Suggested camelCase names:', paste(suggested_names, collapse=', '), '\\n')")
      } else if (naming_method == "PascalCase") {
        # PascalCase naming
        paste0("cat('PascalCase Column Naming Analysis\\n')\n",
               "cat('Current names:', paste(names(", dataframe, "), collapse=', '), '\\n')\n",
               "suggested_names <- sapply(names(", dataframe, "), function(x) {\n",
               "  # Convert to PascalCase: all words capitalized\n",
               "  words <- strsplit(tolower(x), '[^a-zA-Z0-9]+')[[1]]\n",
               "  paste0(toupper(substring(words, 1, 1)), tolower(substring(words, 2)), collapse = '')\n",
               "})\n",
               "cat('Suggested PascalCase names:', paste(suggested_names, collapse=', '), '\\n')")
      } else if (naming_method == "lowercase") {
        # lowercase naming
        paste0("cat('lowercase Column Naming Analysis\\n')\n",
               "cat('Current names:', paste(names(", dataframe, "), collapse=', '), '\\n')\n",
               "suggested_names <- tolower(names(", dataframe, "))\n",
               "suggested_names <- gsub('[^a-z0-9]', '_', suggested_names)\n",
               "suggested_names <- gsub('_+', '_', suggested_names)\n",
               "suggested_names <- gsub('^_|_$', '', suggested_names)\n",
               "cat('Suggested lowercase names:', paste(suggested_names, collapse=', '), '\\n')")
      } else if (naming_method == "UPPERCASE") {
        # UPPERCASE naming
        paste0("cat('UPPERCASE Column Naming Analysis\\n')\n",
               "cat('Current names:', paste(names(", dataframe, "), collapse=', '), '\\n')\n",
               "suggested_names <- toupper(names(", dataframe, "))\n",
               "suggested_names <- gsub('[^A-Z0-9]', '_', suggested_names)\n",
               "suggested_names <- gsub('_+', '_', suggested_names)\n",
               "suggested_names <- gsub('^_|_$', '', suggested_names)\n",
               "cat('Suggested UPPERCASE names:', paste(suggested_names, collapse=', '), '\\n')")
      } else {
        # Default snake_case naming
        paste0("cat('snake_case Column Naming Analysis\\n')\n",
               "cat('Current names:', paste(names(", dataframe, "), collapse=', '), '\\n')\n",
               "suggested_names <- gsub('[^A-Za-z0-9_]', '_', tolower(names(", dataframe, ")))\n",
               "suggested_names <- gsub('_+', '_', suggested_names)\n",
               "suggested_names <- gsub('^_|_$', '', suggested_names)\n",
               "cat('Suggested snake_case names:', paste(suggested_names, collapse=', '), '\\n')")
      }
    },
    "custom" = {
      # For custom operations, we'll send the request to Claude to generate appropriate code
      paste0("cat('Custom Operations Analysis\\n')\n",
             "cat('User request: ", step_info$custom_request, "\\n')\n",
             "cat('This step requires AI-generated code based on your custom request.\\n')\n",
             "cat('The system will generate appropriate R code for: ", step_info$custom_request, "\\n')")
    },
    {
      paste0("# Unknown operation: ", operation)
    }
  )
}

#' Start Cleaning Agent Workflow
#' @param dataframe Name of the DataFrame to clean
#' @param na_handling Method for handling NAs
#' @param cleaning_options List of cleaning operations to perform
#' @param method_options List of method choices for each operation
#' @param custom_inputs List of custom text inputs for "Other" methods
#' @param other_operations Custom cleaning operations specified by user
#' @return List with workflow information
start_cleaning_agent <- function(dataframe, na_handling = "median", cleaning_options = NULL, method_options = NULL, custom_inputs = NULL, other_operations = NULL, selected_variables = NULL) {
  # Validate DataFrame exists
  if (!exists(dataframe, envir = globalenv())) {
    return(list(
      success = FALSE,
      error = paste("DataFrame", dataframe, "not found")
    ))
  }
  
  # Validate selected variables exist in dataframe
  if (!is.null(selected_variables)) {
    df <- get(dataframe, envir = globalenv())
    if (is.data.frame(df)) {
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
  }
  
  # Default cleaning options if not provided
  if (is.null(cleaning_options)) {
    cleaning_options <- list(
      dataTypes = TRUE,
      missingValues = TRUE,
      duplicates = TRUE,
      outliers = TRUE,
      columnNames = TRUE
    )
  }
  
  # Build dynamic workflow based on selected options
  workflow_steps <- list()
  step_num <- 1
  
  if (cleaning_options$dataTypes) {
    # Get data type method for description
    data_type_method <- if (!is.null(method_options) && !is.null(method_options$dataTypes)) method_options$dataTypes else "automatic"
    data_type_desc <- switch(data_type_method,
      "other" = "Examine data types and structure using custom method",
      "Examine data types and structure (automatic detection)"
    )
    
    workflow_steps[[step_num]] <- list(
      step = step_num,
      description = data_type_desc,
      operation = "data_types"
    )
    step_num <- step_num + 1
  }
  
  if (cleaning_options$missingValues) {
    # Get missing values method for description
    missing_method <- if (!is.null(method_options) && !is.null(method_options$missingValues)) method_options$missingValues else "dont"
    missing_desc <- switch(missing_method,
      "mean" = "Analyze missing values patterns (replace with mean)",
      "median" = "Analyze missing values patterns (replace with median)",
      "mode" = "Analyze missing values patterns (replace with mode)",
      "remove" = "Analyze missing values patterns (remove rows)",
      "other" = "Analyze missing values patterns using custom method",
      "Analyze missing values patterns (analysis only)"
    )
    
    workflow_steps[[step_num]] <- list(
      step = step_num,
      description = missing_desc,
      operation = "missing_values"
    )
    step_num <- step_num + 1
  }
  
  if (cleaning_options$duplicates) {
    workflow_steps[[step_num]] <- list(
      step = step_num,
      description = "Detect duplicate records",
      operation = "duplicates"
    )
    step_num <- step_num + 1
  }
  
  if (cleaning_options$outliers) {
    # Get outlier method for description
    outlier_method <- if (!is.null(method_options) && !is.null(method_options$outliers)) method_options$outliers else "iqr"
    outlier_desc <- switch(outlier_method,
      "zscore" = "Identify outliers using Z-Score method (3 sigma threshold)",
      "modified_zscore" = "Identify outliers using Modified Z-Score method (3.5 MAD)",
      "other" = "Identify outliers using custom method",
      "Identify outliers using IQR method (1.5x multiplier)"
    )
    
    workflow_steps[[step_num]] <- list(
      step = step_num,
      description = outlier_desc,
      operation = "outliers"
    )
    step_num <- step_num + 1
  }
  
  if (cleaning_options$columnNames) {
    # Get column naming method for description
    naming_method <- if (!is.null(method_options) && !is.null(method_options$columnNames)) method_options$columnNames else "snake_case"
    naming_desc <- switch(naming_method,
      "camelCase" = "Standardize column names to camelCase format",
      "PascalCase" = "Standardize column names to PascalCase format", 
      "lowercase" = "Standardize column names to lowercase format",
      "UPPERCASE" = "Standardize column names to UPPERCASE format",
      "other" = "Standardize column names using custom convention",
      "Standardize column names to snake_case format"
    )
    
    workflow_steps[[step_num]] <- list(
      step = step_num,
      description = naming_desc,
      operation = "column_names"
    )
    step_num <- step_num + 1
  }
  
  # Add custom operations if specified
  if (!is.null(other_operations) && nchar(trimws(other_operations)) > 0) {
    workflow_steps[[step_num]] <- list(
      step = step_num,
      description = paste("Custom operations:", other_operations),
      operation = "custom",
      custom_request = other_operations
    )
    step_num <- step_num + 1
  }
  
  if (length(workflow_steps) == 0) {
    return(list(
      success = FALSE,
      message = "No cleaning operations selected",
      error = "Please select at least one cleaning operation"
    ))
  }
  
  # Generate R code for the first step
  first_step_code <- generate_step_code(workflow_steps[[1]], dataframe, na_handling, method_options, custom_inputs, selected_variables)
  cat("R: Generated first step code:", first_step_code, "\n")
  
  return(list(
    success = TRUE,
    message = "Data Cleaning Agent started successfully",
    workflow_id = paste0("cleaning_", Sys.time()),
    dataframe = dataframe,
    na_handling = na_handling,
    cleaning_options = cleaning_options,
    method_options = method_options,
    custom_inputs = custom_inputs,
    workflow_steps = workflow_steps,
    current_step = c(workflow_steps[[1]], list(code = first_step_code)),
    total_steps = length(workflow_steps)
  ))
}

#' Get Comprehensive Object Summary for Explorer (handles all object types)
#' @param obj_name Name of the object
#' @return List with full explorer data
get_object_explorer_data <- function(obj_name) {
  if (!exists(obj_name, envir = globalenv())) {
    return(list(exists = FALSE, error = "Object not found"))
  }
  
  tryCatch({
    obj <- get(obj_name, envir = globalenv())
    obj_type <- class(obj)[1]
    
    # Handle different object types
    if (is.data.frame(obj)) {
      return(get_dataframe_explorer_data(obj_name))
    } else if (is.list(obj) && !is.data.frame(obj)) {
      # List exploration
      list_info <- list(
        exists = TRUE,
        name = obj_name,
        type = "list",
        length = length(obj),
        names = if (!is.null(names(obj))) names(obj) else NULL,
        elements = list()
      )
      
      # Get info for each element (limit to first 50 for performance)
      max_elements <- min(50, length(obj))
      for (i in 1:max_elements) {
        tryCatch({
          elem <- obj[[i]]
          elem_info <- list(
            index = i,
            name = if (!is.null(names(obj))) names(obj)[i] else NULL,
            type = class(elem)[1],
            length = if (is.vector(elem) || is.list(elem)) length(elem) else NULL,
            preview = capture.output(str(elem, max.level = 1))[1]
          )
          list_info$elements[[i]] <- elem_info
        }, error = function(e) {
          list_info$elements[[i]] <- list(index = i, error = e$message)
        })
      }
      
      return(list_info)
    } else if (is.vector(obj) && !is.list(obj)) {
      # Vector exploration
      vec_info <- list(
        exists = TRUE,
        name = obj_name,
        type = obj_type,
        length = length(obj),
        values = if (length(obj) <= 100) as.list(obj) else as.list(obj[1:100]),
        preview_count = min(100, length(obj))
      )
      
      # Add summary stats for numeric vectors
      if (is.numeric(obj)) {
        non_na <- obj[!is.na(obj)]
        if (length(non_na) > 0) {
          vec_info$summary_stats <- list(
            min = as.numeric(min(non_na)),
            max = as.numeric(max(non_na)),
            mean = as.numeric(mean(non_na)),
            median = as.numeric(median(non_na)),
            sd = as.numeric(sd(non_na)),
            se = as.numeric(sd(non_na) / sqrt(length(non_na)))
          )
          
          # Histogram for numeric vectors - use safe breaks that span full range
          # Convert to numeric to handle integer64 and other special numeric types
          non_na_numeric <- as.numeric(non_na)
          
          tryCatch({
            data_range <- range(non_na_numeric, na.rm = TRUE)
            range_diff <- diff(data_range)
            if (range_diff > 0) {
              # Create breaks that explicitly span the full range
              breaks_seq <- seq(data_range[1], data_range[2], length.out = 21)
              hist_result <- hist(non_na_numeric, breaks = breaks_seq, plot = FALSE, include.lowest = TRUE)
              vec_info$histogram <- list(
                breaks = as.numeric(hist_result$breaks),
                counts = as.numeric(hist_result$counts)
              )
            } else {
              # All values are the same - create a simple histogram with proper breaks
              single_val <- as.numeric(data_range[1])
              vec_info$histogram <- list(
                breaks = c(single_val - 0.5, single_val + 0.5),
                counts = as.numeric(length(non_na))
              )
            }
          }, error = function(e) {
            # If histogram fails, skip it
            vec_info$histogram <- NULL
          })
        }
      } else if (is.character(obj) || is.factor(obj)) {
        # Frequency table for character/factor vectors
        freq_table <- table(obj)
        freq_sorted <- sort(freq_table, decreasing = TRUE)
        top_n <- min(10, length(freq_sorted))
        
        # Ensure values is always a vector (array in JSON), even for single values
        freq_values <- names(freq_sorted)[1:top_n]
        if (length(freq_values) == 1) {
          freq_values <- c(freq_values)  # Ensure it's a vector, not a scalar
        }
        
        vec_info$frequency <- list(
          values = as.character(freq_values),
          counts = as.numeric(freq_sorted[1:top_n]),
          total_categories = length(freq_sorted)
        )
      }
      
      return(vec_info)
    } else if (is.function(obj)) {
      # Function exploration - show full body
      func_info <- list(
        exists = TRUE,
        name = obj_name,
        type = "function",
        formals = names(formals(obj)),
        formals_count = length(formals(obj)),
        body_full = capture.output(body(obj)),
        environment = environmentName(environment(obj))
      )
      return(func_info)
    } else if (is.matrix(obj)) {
      # Matrix exploration (treat similar to dataframe but simpler)
      mat_info <- list(
        exists = TRUE,
        name = obj_name,
        type = "matrix",
        nrow = nrow(obj),
        ncol = ncol(obj),
        dim = dim(obj),
        mode = mode(obj)
      )
      
      # Get column info if small enough
      if (ncol(obj) <= 50) {
        col_info <- lapply(1:ncol(obj), function(i) {
          col_data <- obj[, i]
          info <- list(
            name = if (!is.null(colnames(obj))) colnames(obj)[i] else paste0("V", i),
            type = class(col_data)[1],
            missing = sum(is.na(col_data))
          )
          
          if (is.numeric(col_data)) {
            non_na <- col_data[!is.na(col_data)]
            if (length(non_na) > 0) {
              info$summary_stats <- list(
                min = as.numeric(min(non_na)),
                max = as.numeric(max(non_na)),
                mean = as.numeric(mean(non_na)),
                median = as.numeric(median(non_na))
              )
            }
          }
          info
        })
        mat_info$column_info <- col_info
      }
      
      return(mat_info)
    } else if (is.array(obj) && !is.matrix(obj)) {
      # Array exploration
      # Ensure dim is always a vector (array) for JSON serialization
      dim_vec <- dim(obj)
      if (is.null(dim_vec)) {
        dim_vec <- length(obj)
      }
      # Ensure it's a vector, not a single number
      if (length(dim_vec) == 1) {
        dim_vec <- c(dim_vec)
      }
      
      array_info <- list(
        exists = TRUE,
        name = obj_name,
        type = "array",
        dim = dim_vec,
        dimnames = dimnames(obj),
        mode = mode(obj)
      )
      
      # Provide array data preview
      # For small arrays, show all data; for large arrays, show a sample
      total_elements <- prod(dim(obj))
      
      if (total_elements <= 1000) {
        # Small array - show all values
        # Convert to vector to ensure JSON serializes as array, not object
        array_info$values <- as.vector(obj)
        array_info$preview_type <- "full"
      } else {
        # Large array - show a sample
        # For 2D arrays, show first few rows/cols
        # For 3D+ arrays, show first slice
        if (length(dim(obj)) == 2) {
          # 2D array - show first 10 rows and 10 columns
          max_rows <- min(10, dim(obj)[1])
          max_cols <- min(10, dim(obj)[2])
          sample_data <- obj[1:max_rows, 1:max_cols, drop = FALSE]
          # Convert to vector to ensure JSON serializes as array
          array_info$values <- as.vector(sample_data)
          array_info$preview_type <- "sample"
          array_info$preview_dims <- c(max_rows, max_cols)
          array_info$total_dims <- dim(obj)
        } else if (length(dim(obj)) == 3) {
          # 3D array - show first slice
          max_rows <- min(10, dim(obj)[1])
          max_cols <- min(10, dim(obj)[2])
          sample_data <- obj[1:max_rows, 1:max_cols, 1, drop = FALSE]
          # Convert to vector to ensure JSON serializes as array
          array_info$values <- as.vector(sample_data)
          array_info$preview_type <- "sample"
          array_info$preview_dims <- c(max_rows, max_cols, 1)
          array_info$total_dims <- dim(obj)
        } else {
          # Higher dimensional array - flatten first 100 elements
          # Convert to vector to ensure JSON serializes as array
          array_info$values <- as.vector(obj[1:min(100, total_elements)])
          array_info$preview_type <- "flattened"
          array_info$preview_count <- min(100, total_elements)
        }
      }
      
      return(array_info)
    } else if (inherits(obj, "ts")) {
      # Time series exploration
      ts_info <- list(
        exists = TRUE,
        name = obj_name,
        type = "ts",
        start = start(obj),
        end = end(obj),
        frequency = frequency(obj),
        length = length(obj),
        deltat = deltat(obj),
        tsp = tsp(obj)
      )
      
      # Get time series values
      if (length(obj) <= 1000) {
        # Small time series - show all values
        ts_info$values <- as.list(obj)
        ts_info$preview_type <- "full"
      } else {
        # Large time series - show first 100 values
        ts_info$values <- as.list(obj[1:100])
        ts_info$preview_type <- "sample"
        ts_info$preview_count <- 100
      }
      
      # Add summary stats for numeric time series
      if (is.numeric(obj)) {
        non_na <- obj[!is.na(obj)]
        if (length(non_na) > 0) {
          ts_info$summary_stats <- list(
            min = as.numeric(min(non_na)),
            max = as.numeric(max(non_na)),
            mean = as.numeric(mean(non_na)),
            median = as.numeric(median(non_na)),
            sd = as.numeric(sd(non_na)),
            se = as.numeric(sd(non_na) / sqrt(length(non_na)))
          )
          
          # Histogram for numeric time series
          non_na_numeric <- as.numeric(non_na)
          
          tryCatch({
            data_range <- range(non_na_numeric, na.rm = TRUE)
            range_diff <- diff(data_range)
            if (range_diff > 0) {
              breaks_seq <- seq(data_range[1], data_range[2], length.out = 21)
              hist_result <- hist(non_na_numeric, breaks = breaks_seq, plot = FALSE, include.lowest = TRUE)
              ts_info$histogram <- list(
                breaks = as.numeric(hist_result$breaks),
                counts = as.numeric(hist_result$counts)
              )
            } else {
              single_val <- as.numeric(data_range[1])
              ts_info$histogram <- list(
                breaks = c(single_val - 0.5, single_val + 0.5),
                counts = as.numeric(length(non_na))
              )
            }
          }, error = function(e) {
            ts_info$histogram <- NULL
          })
        }
      }
      
      return(ts_info)
    } else {
      # Other object types - basic info
      other_info <- list(
        exists = TRUE,
        name = obj_name,
        type = obj_type,
        class = class(obj),
        str_preview = capture.output(str(obj, max.level = 2))[1:20]
      )
      return(other_info)
    }
  }, error = function(e) {
    return(list(exists = FALSE, error = paste("Error exploring object:", e$message)))
  })
}

#' Get Comprehensive DataFrame Summary for Explorer
#' @param df_name Name of the DataFrame
#' @return List with full explorer data
get_dataframe_explorer_data <- function(df_name) {
  if (!exists(df_name, envir = globalenv())) {
    return(list(exists = FALSE, error = "DataFrame not found"))
  }
  
  tryCatch({
    df <- get(df_name, envir = globalenv())
    
    if (!is.data.frame(df)) {
      return(list(exists = FALSE, error = "Not a data.frame"))
    }
    
    # Get comprehensive column information
    col_info <- lapply(names(df), function(col_name) {
      col_data <- df[[col_name]]
      
      # Determine type - prioritize date/time types
      if (inherits(col_data, "POSIXct") || inherits(col_data, "POSIXlt")) {
        col_class <- "POSIXct"
      } else if (inherits(col_data, "Date")) {
        col_class <- "Date"
      } else {
        col_class <- class(col_data)[1]
      }
      
      info <- list(
        name = col_name,
        type = col_class,
        missing = sum(is.na(col_data)),
        missing_pct = round(100 * sum(is.na(col_data)) / nrow(df), 2),
        unique_values = length(unique(col_data))
      )
      
      # Numeric columns: histogram + summary stats
      if (is.numeric(col_data)) {
        non_na <- col_data[!is.na(col_data)]
        if (length(non_na) > 0) {
          # Convert to numeric to handle integer64 and other special numeric types
          non_na_numeric <- as.numeric(non_na)
          
          # Generate histogram with safe breaks that span full range
          tryCatch({
            data_range <- range(non_na_numeric, na.rm = TRUE)
            range_diff <- diff(data_range)
            if (range_diff > 0) {
              # Create breaks that explicitly span the full range
              breaks_seq <- seq(data_range[1], data_range[2], length.out = 21)
              hist_result <- hist(non_na_numeric, breaks = breaks_seq, plot = FALSE, include.lowest = TRUE)
              info$histogram <- list(
                breaks = as.numeric(hist_result$breaks),
                counts = as.numeric(hist_result$counts)
              )
            } else {
              # All values are the same - create a simple histogram with proper breaks
              single_val <- as.numeric(data_range[1])
              info$histogram <- list(
                breaks = c(single_val - 0.5, single_val + 0.5),
                counts = as.numeric(length(non_na))
              )
            }
          }, error = function(e) {
            # If histogram fails, skip it
            info$histogram <- NULL
          })
          
          info$summary_stats <- list(
            min = as.numeric(min(non_na)),
            max = as.numeric(max(non_na)),
            mean = as.numeric(mean(non_na)),
            median = as.numeric(median(non_na)),
            sd = as.numeric(sd(non_na)),
            se = as.numeric(sd(non_na) / sqrt(length(non_na)))
          )
        }
      }
      # Logical columns: show TRUE/FALSE counts and percentages
      else if (is.logical(col_data)) {
        non_na <- col_data[!is.na(col_data)]
        total_obs <- length(col_data)
        na_count <- sum(is.na(col_data))
        
        if (length(non_na) > 0) {
          true_count <- sum(non_na == TRUE)
          false_count <- sum(non_na == FALSE)
          
          # Frequency table for mini chart
          freq_table <- table(non_na)
          freq_sorted <- sort(freq_table, decreasing = TRUE)
          
          info$frequency <- list(
            values = c("TRUE", "FALSE"),
            counts = as.numeric(c(true_count, false_count)),
            total_categories = 2
          )
          
          # Summary stats for logical
          info$summary_stats <- list(
            top_values = c("TRUE", "FALSE"),
            top_counts = c(true_count, false_count),
            top_percentages = c(
              round(100 * true_count / length(non_na), 2),
              round(100 * false_count / length(non_na), 2)
            ),
            total_categories = 2,
            total_observations = length(non_na),
            na_count = na_count,
            na_percentage = round(100 * na_count / total_obs, 2)
          )
        }
      }
      # Categorical columns: frequency table + bar chart + enhanced stats
      else if (is.factor(col_data) || is.character(col_data)) {
        non_na <- col_data[!is.na(col_data)]
        if (length(non_na) > 0) {
          freq_table <- table(non_na)
          freq_sorted <- sort(freq_table, decreasing = TRUE)
          
          # Limit to top 20 categories for mini chart
          top_n <- min(20, length(freq_sorted))
          top_counts <- as.numeric(freq_sorted[1:top_n])
          top_labels <- names(freq_sorted[1:top_n])
          
          info$frequency <- list(
            values = as.character(top_labels),
            counts = top_counts,
            total_categories = length(freq_sorted)
          )
          
          # Enhanced summary stats for categorical - show top 5 values
          top_n_display <- min(5, length(freq_sorted))
          top_values <- names(freq_sorted)[1:top_n_display]
          top_counts_display <- as.numeric(freq_sorted[1:top_n_display])
          top_pcts <- round(100 * top_counts_display / length(non_na), 2)
          
          info$summary_stats <- list(
            top_values = as.character(top_values),
            top_counts = top_counts_display,
            top_percentages = top_pcts,
            total_categories = length(freq_sorted),
            total_observations = length(non_na)
          )
          
          # For character columns, add string length stats
          if (is.character(col_data)) {
            char_lengths <- nchar(as.character(non_na))
            info$summary_stats$avg_length = round(mean(char_lengths), 2)
            info$summary_stats$min_length = min(char_lengths)
            info$summary_stats$max_length = max(char_lengths)
            info$summary_stats$median_length = median(char_lengths)
          }
        }
      }
      # Date/Time columns: POSIXct and Date
      else if (inherits(col_data, "POSIXct") || inherits(col_data, "POSIXlt") || inherits(col_data, "Date")) {
        non_na <- col_data[!is.na(col_data)]
        if (length(non_na) > 0) {
          # Convert to numeric for histogram (days since epoch or seconds)
          if (inherits(col_data, "POSIXct") || inherits(col_data, "POSIXlt")) {
            num_data <- as.numeric(non_na)
            # Generate histogram with safe breaks that span full range
            tryCatch({
              data_range <- range(num_data, na.rm = TRUE)
              if (diff(data_range) > 0) {
                breaks_seq <- seq(data_range[1], data_range[2], length.out = 21)
                hist_result <- hist(num_data, breaks = breaks_seq, plot = FALSE, include.lowest = TRUE)
                info$histogram <- list(
                  breaks = as.numeric(hist_result$breaks),
                  counts = as.numeric(hist_result$counts)
                )
              } else {
                info$histogram <- list(
                  breaks = c(data_range[1] - 0.5, data_range[1] + 0.5),
                  counts = length(num_data)
                )
              }
            }, error = function(e) {
              info$histogram <- NULL
            })
          } else {
            # For Date class, convert to numeric (days)
            num_data <- as.numeric(non_na)
            # Generate histogram with safe breaks that span full range
            tryCatch({
              data_range <- range(num_data, na.rm = TRUE)
              if (diff(data_range) > 0) {
                breaks_seq <- seq(data_range[1], data_range[2], length.out = 21)
                hist_result <- hist(num_data, breaks = breaks_seq, plot = FALSE, include.lowest = TRUE)
                info$histogram <- list(
                  breaks = as.numeric(hist_result$breaks),
                  counts = as.numeric(hist_result$counts)
                )
              } else {
                info$histogram <- list(
                  breaks = c(data_range[1] - 0.5, data_range[1] + 0.5),
                  counts = length(num_data)
                )
              }
            }, error = function(e) {
              info$histogram <- NULL
            })
          }
          
          # Date summary stats
          info$summary_stats <- list(
            min = as.character(min(non_na)),
            max = as.character(max(non_na)),
            range_days = as.numeric(difftime(max(non_na), min(non_na), units = "days")),
            median = as.character(median(non_na))
          )
          
          # For POSIXct, also show time range
          if (inherits(col_data, "POSIXct") || inherits(col_data, "POSIXlt")) {
            info$summary_stats$range_hours = as.numeric(difftime(max(non_na), min(non_na), units = "hours"))
          }
        }
      }
      
      return(info)
    })
    
    names(col_info) <- names(df)
    
    return(list(
      exists = TRUE,
      name = df_name,
      nrow = nrow(df),
      ncol = ncol(df),
      columns = names(df),
      column_info = col_info,
      size_mb = round(as.numeric(object.size(df)) / 1024^2, 2)
    ))
  }, error = function(e) {
    return(list(exists = FALSE, error = e$message))
  })
}

#' Get Filtered and Sorted DataFrame Chunk
#' @param df_name Name of the DataFrame
#' @param page Page number
#' @param page_size Rows per page
#' @param filters List of filter conditions
#' @param sorts List of sort specifications (column, direction)
#' @param pinned_cols Vector of pinned column names
#' @return Filtered and sorted chunk
get_dataframe_chunk_filtered <- function(df_name, page = 1, page_size = 100, 
                                         filters = NULL, sorts = NULL, 
                                         pinned_cols = NULL) {
  if (!exists(df_name, envir = globalenv())) {
    return(list(exists = FALSE, error = "DataFrame or Matrix not found"))
  }
  
  tryCatch({
    df <- get(df_name, envir = globalenv())
    
    # Handle both dataframes and matrices
    if (!is.data.frame(df) && !is.matrix(df)) {
      return(list(exists = FALSE, error = paste(df_name, "is not a data.frame or matrix")))
    }
    
    # Convert matrix to dataframe for consistent handling
    if (is.matrix(df)) {
      original_matrix <- df
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      # Preserve column names if they exist and match the number of columns
      original_colnames <- colnames(original_matrix)
      if (!is.null(original_colnames) && length(original_colnames) == ncol(df)) {
        colnames(df) <- original_colnames
      } else if (is.null(original_colnames) || length(original_colnames) != ncol(df)) {
        # If column names don't match, use default names or fix them
        if (is.null(original_colnames)) {
          colnames(df) <- paste0("V", 1:ncol(df))
        } else {
          # Pad or truncate column names to match number of columns
          if (length(original_colnames) < ncol(df)) {
            colnames(df) <- c(original_colnames, paste0("V", (length(original_colnames) + 1):ncol(df)))
          } else {
            colnames(df) <- original_colnames[1:ncol(df)]
          }
        }
      }
    }
    
    # Apply filters
    if (!is.null(filters) && length(filters) > 0) {
      for (i in seq_along(filters)) {
        filter <- filters[[i]]
        
        # Handle both list and data.frame formats from JSON
        if (is.data.frame(filter)) {
          col_name <- filter$column[1]
          filter_type <- filter$type[1]
          # Handle value extraction safely
          if (is.list(filter$value)) {
            filter_value <- filter$value[[1]]
          } else {
            filter_value <- filter$value[1]
          }
        } else if (is.list(filter)) {
          col_name <- filter$column
          filter_type <- filter$type
          filter_value <- filter$value
        } else {
          # Skip if filter is atomic (shouldn't happen, but handle gracefully)
          cat("R: Skipping filter - unexpected format\n")
          next
        }
        
        if (!is.null(col_name) && col_name %in% names(df)) {
          col_data <- df[[col_name]]
          
          if (filter_type == "equals") {
            if (is.numeric(col_data)) {
              df <- df[!is.na(col_data) & col_data == as.numeric(filter_value), , drop = FALSE]
            } else if (is.logical(col_data)) {
              # Handle logical: convert string "TRUE"/"FALSE" to boolean
              bool_value <- if (tolower(as.character(filter_value)) == "true") TRUE else if (tolower(as.character(filter_value)) == "false") FALSE else NULL
              if (!is.null(bool_value)) {
                df <- df[!is.na(col_data) & col_data == bool_value, , drop = FALSE]
              }
            } else {
              df <- df[!is.na(col_data) & col_data == filter_value, , drop = FALSE]
            }
          } else if (filter_type == "not_equals") {
            if (is.numeric(col_data)) {
              df <- df[is.na(col_data) | col_data != as.numeric(filter_value), , drop = FALSE]
            } else if (is.logical(col_data)) {
              # Handle logical: convert string "TRUE"/"FALSE" to boolean
              bool_value <- if (tolower(as.character(filter_value)) == "true") TRUE else if (tolower(as.character(filter_value)) == "false") FALSE else NULL
              if (!is.null(bool_value)) {
                df <- df[is.na(col_data) | col_data != bool_value, , drop = FALSE]
              }
            } else {
              df <- df[is.na(col_data) | col_data != filter_value, , drop = FALSE]
            }
          } else if (filter_type == "contains" && (is.character(col_data) || is.factor(col_data))) {
            df <- df[!is.na(col_data) & grepl(filter_value, as.character(col_data), fixed = TRUE), , drop = FALSE]
          } else if (filter_type == "starts_with" && (is.character(col_data) || is.factor(col_data))) {
            df <- df[!is.na(col_data) & startsWith(as.character(col_data), filter_value), , drop = FALSE]
          } else if (filter_type == "ends_with" && (is.character(col_data) || is.factor(col_data))) {
            df <- df[!is.na(col_data) & endsWith(as.character(col_data), filter_value), , drop = FALSE]
          } else if (filter_type == "is_empty" && (is.character(col_data) || is.factor(col_data))) {
            df <- df[!is.na(col_data) & (as.character(col_data) == "" | trimws(as.character(col_data)) == ""), , drop = FALSE]
          } else if (filter_type == "is_not_empty" && (is.character(col_data) || is.factor(col_data))) {
            df <- df[!is.na(col_data) & as.character(col_data) != "" & trimws(as.character(col_data)) != "", , drop = FALSE]
          } else if (filter_type == "greater_than" && (is.numeric(col_data) || inherits(col_data, "Date") || inherits(col_data, "POSIXct") || inherits(col_data, "POSIXlt"))) {
            if (is.numeric(col_data)) {
              df <- df[!is.na(col_data) & col_data > as.numeric(filter_value), , drop = FALSE]
            } else {
              # Date/POSIXct comparison
              filter_date <- tryCatch({
                if (inherits(col_data, "Date")) {
                  as.Date(filter_value)
                } else {
                  as.POSIXct(filter_value)
                }
              }, error = function(e) NULL)
              if (!is.null(filter_date)) {
                df <- df[!is.na(col_data) & col_data > filter_date, , drop = FALSE]
              }
            }
          } else if (filter_type == "less_than" && (is.numeric(col_data) || inherits(col_data, "Date") || inherits(col_data, "POSIXct") || inherits(col_data, "POSIXlt"))) {
            if (is.numeric(col_data)) {
              df <- df[!is.na(col_data) & col_data < as.numeric(filter_value), , drop = FALSE]
            } else {
              # Date/POSIXct comparison
              filter_date <- tryCatch({
                if (inherits(col_data, "Date")) {
                  as.Date(filter_value)
                } else {
                  as.POSIXct(filter_value)
                }
              }, error = function(e) NULL)
              if (!is.null(filter_date)) {
                df <- df[!is.na(col_data) & col_data < filter_date, , drop = FALSE]
              }
            }
          } else if (filter_type == "between" && (is.numeric(col_data) || inherits(col_data, "Date") || inherits(col_data, "POSIXct") || inherits(col_data, "POSIXlt"))) {
            # Handle between filter - filter_value should be a vector/array
            if (is.list(filter_value) && length(filter_value) >= 2) {
              if (is.numeric(col_data)) {
                val1 <- as.numeric(filter_value[[1]])
                val2 <- as.numeric(filter_value[[2]])
                df <- df[!is.na(col_data) & col_data >= val1 & col_data <= val2, , drop = FALSE]
              } else {
                # Date/POSIXct between
                val1 <- tryCatch({
                  if (inherits(col_data, "Date")) as.Date(filter_value[[1]]) else as.POSIXct(filter_value[[1]])
                }, error = function(e) NULL)
                val2 <- tryCatch({
                  if (inherits(col_data, "Date")) as.Date(filter_value[[2]]) else as.POSIXct(filter_value[[2]])
                }, error = function(e) NULL)
                if (!is.null(val1) && !is.null(val2)) {
                  df <- df[!is.na(col_data) & col_data >= val1 & col_data <= val2, , drop = FALSE]
                }
              }
            } else if (is.vector(filter_value) && length(filter_value) >= 2) {
              if (is.numeric(col_data)) {
                val1 <- as.numeric(filter_value[1])
                val2 <- as.numeric(filter_value[2])
                df <- df[!is.na(col_data) & col_data >= val1 & col_data <= val2, , drop = FALSE]
              } else {
                # Date/POSIXct between
                val1 <- tryCatch({
                  if (inherits(col_data, "Date")) as.Date(filter_value[1]) else as.POSIXct(filter_value[1])
                }, error = function(e) NULL)
                val2 <- tryCatch({
                  if (inherits(col_data, "Date")) as.Date(filter_value[2]) else as.POSIXct(filter_value[2])
                }, error = function(e) NULL)
                if (!is.null(val1) && !is.null(val2)) {
                  df <- df[!is.na(col_data) & col_data >= val1 & col_data <= val2, , drop = FALSE]
                }
              }
            } else {
              cat("R: Invalid 'between' filter value format\n")
              next
            }
          } else if (filter_type == "is_null") {
            df <- df[is.na(col_data), , drop = FALSE]
          } else if (filter_type == "is_not_null") {
            df <- df[!is.na(col_data), , drop = FALSE]
          }
        }
      }
    }
    
    # Apply sorting
    if (!is.null(sorts) && length(sorts) > 0) {
      cat("R: Processing sorts, length:", length(sorts), "class:", class(sorts), "\n")
      tryCatch({
        # Handle both list and data.frame formats from JSON
        sort_cols <- character(0)
        sort_dirs <- logical(0)
        
        for (i in seq_along(sorts)) {
          s <- sorts[[i]]
          cat("R: Sort", i, "- class:", paste(class(s), collapse = ", "), "is.list:", is.list(s), "is.atomic:", is.atomic(s), "\n")
          
          # Handle different formats
          if (is.data.frame(s)) {
            col_name <- s$column[1]
            dir_val <- s$direction[1]
            sort_cols <- c(sort_cols, col_name)
            sort_dirs <- c(sort_dirs, dir_val == "asc")
            cat("R:   Column:", col_name, "Direction:", dir_val, "Ascending:", dir_val == "asc", "\n")
          } else if (is.list(s)) {
            # Check if it has the expected structure
            if (!is.null(s$column) || "column" %in% names(s)) {
              col_name <- ifelse(is.null(s$column), s[["column"]], s$column)
              dir_val <- ifelse(is.null(s$direction), ifelse(is.null(s[["direction"]]), "asc", s[["direction"]]), s$direction)
              sort_cols <- c(sort_cols, col_name)
              sort_dirs <- c(sort_dirs, dir_val == "asc")
              cat("R:   Column:", col_name, "Direction:", dir_val, "Ascending:", dir_val == "asc", "\n")
            } else {
              cat("R: List format but no column field. Names:", paste(names(s), collapse = ", "), "\n")
              cat("R:   Full structure:", capture.output(str(s)), "\n")
              next
            }
          } else if (is.atomic(s)) {
            cat("R: Sort", i, "is atomic vector, cannot extract column/direction. Values:", paste(s, collapse = ", "), "\n")
            cat("R:   This suggests JSON parsing issue - sorts should be lists\n")
            next
          } else {
            cat("R: Skipping sort", i, "- unexpected format, class:", paste(class(s), collapse = ", "), "\n")
            next
          }
        }
        
        if (length(sort_cols) > 0 && all(sort_cols %in% names(df))) {
          # Create order vector
          order_vec <- do.call(order, lapply(1:length(sort_cols), function(i) {
            col <- df[[sort_cols[i]]]
            if (sort_dirs[i]) {
              # Ascending sort
              col
            } else {
              # Descending sort - handle both numeric and character/factor
              if (is.numeric(col)) {
                -col
              } else {
                # For character/factor, reverse the order
                -rank(col, ties.method = "first")
              }
            }
          }))
          
          df <- df[order_vec, , drop = FALSE]
          cat("R: Applied sorting to FULL dataset -", nrow(df), "rows - columns:", paste(sort_cols, collapse = ", "), "directions:", paste(ifelse(sort_dirs, "asc", "desc"), collapse = ", "), "\n")
          # Show first few values to confirm sorting
          if (length(sort_cols) > 0 && sort_cols[1] %in% names(df)) {
            first_vals <- head(df[[sort_cols[1]]], 5)
            cat("R: First 5 values of", sort_cols[1], "after sorting:", paste(first_vals, collapse = ", "), "\n")
          }
        }
      }, error = function(e) {
        cat("Error in sorting:", e$message, "\n")
      })
    }
    
    # Reorder columns (pinned first)
    if (!is.null(pinned_cols) && length(pinned_cols) > 0) {
      # Only keep pinned_cols that actually exist in the dataframe
      valid_pinned <- intersect(pinned_cols, names(df))
      if (length(valid_pinned) > 0) {
        other_cols <- setdiff(names(df), valid_pinned)
        df <- df[, c(valid_pinned, other_cols), drop = FALSE]
        cat("R: Reordered columns - pinned:", length(valid_pinned), "other:", length(other_cols), "\n")
      }
    }
    
    total_rows <- nrow(df)
    
    # Handle empty dataframe
    if (total_rows == 0) {
      return(list(
        exists = TRUE,
        data = list(),
        page = page,
        page_size = page_size,
        total_rows = 0,
        total_pages = 0,
        filtered_rows = 0,
        start_row = 0,
        end_row = 0,
        column_order = names(df)
      ))
    }
    
    # Ensure page_size is valid
    page_size <- max(1, as.integer(page_size))
    page <- max(1, as.integer(page))
    
    total_pages <- max(1, ceiling(total_rows / page_size))
    
    # Ensure page is within bounds
    page <- min(page, total_pages)
    
    # Calculate row indices (1-based indexing for R)
    start_row <- (page - 1) * page_size + 1
    end_row <- min(page * page_size, total_rows)
    
    # Double-check bounds
    if (start_row < 1) start_row <- 1
    if (end_row > total_rows) end_row <- total_rows
    if (start_row > total_rows) {
      # This shouldn't happen, but handle it gracefully
      start_row <- max(1, total_rows)
      end_row <- total_rows
    }
    
    # Extract chunk with proper bounds checking
    if (start_row <= end_row && start_row >= 1 && end_row <= total_rows && start_row <= nrow(df)) {
      tryCatch({
        chunk <- df[start_row:end_row, , drop = FALSE]
      }, error = function(e) {
        chunk <- df[integer(0), , drop = FALSE]
        start_row <- 0
        end_row <- 0
      })
    } else {
      # Fallback: return empty chunk
      chunk <- df[integer(0), , drop = FALSE]
      start_row <- 0
      end_row <- 0
    }
    
    # Convert to list format
    chunk_data <- lapply(names(chunk), function(col_name) {
      as.character(chunk[[col_name]])
    })
    names(chunk_data) <- names(chunk)
    
    return(list(
      exists = TRUE,
      data = chunk_data,
      page = page,
      page_size = page_size,
      total_rows = total_rows,
      total_pages = total_pages,
      filtered_rows = total_rows,
      start_row = start_row,
      end_row = end_row,
      column_order = names(chunk)
    ))
  }, error = function(e) {
    return(list(exists = FALSE, error = e$message))
  })
}

#' Convert Filters and Sorts to R Code
#' @param df_name Name of the DataFrame
#' @param filters List of filters
#' @param sorts List of sorts
#' @return R code string
convert_explorer_view_to_code <- function(df_name, filters = NULL, sorts = NULL) {
  code_parts <- c()
  
  if (!is.null(filters) && length(filters) > 0) {
    filter_codes <- sapply(filters, function(filter) {
      col_name <- filter$column
      filter_type <- filter$type
      filter_value <- filter$value
      
      if (filter_type == "equals") {
        paste0(col_name, " == ", deparse(filter_value))
      } else if (filter_type == "not_equals") {
        paste0(col_name, " != ", deparse(filter_value))
      } else if (filter_type == "contains") {
        paste0("grepl(", deparse(filter_value), ", ", col_name, ", fixed = TRUE)")
      } else if (filter_type == "greater_than") {
        paste0(col_name, " > ", filter_value)
      } else if (filter_type == "less_than") {
        paste0(col_name, " < ", filter_value)
      } else if (filter_type == "between") {
        paste0(col_name, " >= ", filter_value[1], " & ", col_name, " <= ", filter_value[2])
      } else if (filter_type == "is_null") {
        paste0("is.na(", col_name, ")")
      } else if (filter_type == "is_not_null") {
        paste0("!is.na(", col_name, ")")
      } else {
        ""
      }
    })
    
    filter_codes <- filter_codes[filter_codes != ""]
    if (length(filter_codes) > 0) {
      code_parts <- c(code_parts, paste0("dplyr::filter(", df_name, ", ", paste(filter_codes, collapse = " & "), ")"))
    }
  }
  
  if (!is.null(sorts) && length(sorts) > 0) {
    sort_codes <- sapply(sorts, function(sort) {
      if (sort$direction == "asc") {
        paste0("dplyr::arrange(", if(length(code_parts) > 0) "result" else df_name, ", ", sort$column, ")")
      } else {
        paste0("dplyr::arrange(", if(length(code_parts) > 0) "result" else df_name, ", dplyr::desc(", sort$column, "))")
      }
    })
    code_parts <- c(code_parts, sort_codes)
  }
  
  if (length(code_parts) > 0) {
    # Chain with pipes
    result <- code_parts[1]
    for (i in 2:length(code_parts)) {
      result <- paste0(result, " %>% ", code_parts[i])
    }
    return(result)
  } else {
    return(df_name)
  }
}
