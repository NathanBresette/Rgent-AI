# Workspace lookups used by the chat page (greeting, completions, object list).
# They used to live in the agent files, which were removed in 2.4.4.

# Get available dataframes for transformation
get_available_dataframes <- function() {
  objects <- explorer_user_objects(ls(envir = .GlobalEnv))
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
        non_na_data <- na.omit(col_data)
        
        # Check if there are any non-NA values
        if (length(non_na_data) > 0) {
          # Check for skewness and distribution
          if (length(non_na_data) > 3) {
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
        } else {
          # All values are NA
          column_info[[col]] <- list(
            type = "numeric",
            missing = sum(is.na(col_data)),
            unique = 0,
            min = NA,
            max = NA,
            mean = NA,
            median = NA,
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
        non_na_data <- na.omit(col_data)
        
        if (length(non_na_data) > 0) {
          column_info[[col]] <- list(
            type = "date",
            missing = sum(is.na(col_data)),
            unique = length(unique(col_data)),
            min_date = as.character(min(col_data, na.rm = TRUE)),
            max_date = as.character(max(col_data, na.rm = TRUE))
          )
        } else {
          # All values are NA
          column_info[[col]] <- list(
            type = "date",
            missing = sum(is.na(col_data)),
            unique = 0,
            min_date = NA,
            max_date = NA
          )
        }
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

#' Get All Available Objects in Global Environment with Types
#' @return List with objects categorized by type, dataframes first
get_available_objects <- function() {
  tryCatch({
    env_objects <- explorer_user_objects(ls(globalenv()))
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
    if (is.data.frame(obj) || is.matrix(obj)) {
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

