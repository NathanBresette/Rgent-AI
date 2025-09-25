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
  cat("🔍 R: Generated first step code:", first_step_code, "\n")
  
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
