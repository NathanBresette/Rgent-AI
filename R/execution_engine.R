#' R Code Execution Engine
#' 
#' This module handles all R code execution using proven, reliable methods
#' adapted from ClaudeR's execution approach.

#' Execute R code in the active RStudio session
#'
#' This function executes the provided R code in the global environment
#' and captures both the result and any output using proven methods.
#'
#' @param code The R code to execute
#' @param settings The current settings for execution
#' @return A list containing the execution result and metadata
execute_code_in_session <- function(code, settings = NULL) {
  # Default settings if not provided
  if (is.null(settings)) {
    settings <- list(
      print_to_console = TRUE,
      log_to_file = FALSE,
      log_file_path = file.path(path.expand("~"), "rstudioai_logs.R")
    )
  }

  # Print code to console if enabled
  if (settings$print_to_console) {
    cat("\n### AI executing the following code ###\n")
    cat(code, "\n")
    cat("### End of AI code ###\n\n")
  }

  # Log code to file if enabled
  if (settings$log_to_file && !is.null(settings$log_file_path) && settings$log_file_path != "") {
    log_code_to_file(code, settings$log_file_path)
  }

  # Create a temporary environment for evaluation
  env <- .GlobalEnv

  # Set up plot capture if needed
  plot_file <- NULL
  has_plot <- FALSE

  tryCatch({
    # Create a temporary file for plot capture
    plot_file <- tempfile(fileext = ".png")

    # Check if this is a plot command and store it for analysis
    is_plot_command <- any(grepl("(ggplot|plot|hist|boxplot|barplot|scatterplot|qplot|plotly|leaflet|density|pairs|ggpairs|qqnorm|qqplot|corrplot|ggcorrplot|heatmap|geom_|stat_|coord_|facet_|theme_)", code, ignore.case = TRUE))
    
    if (is_plot_command) {
      # Store the plot command for analysis
      tryCatch({
        assign("last_plot_command", code, envir = .GlobalEnv)
        assign("last_plot_timestamp", Sys.time(), envir = .GlobalEnv)
      }, error = function(e) {
        # Silently fail if storage doesn't work
      })
    }

    # Execute code and capture plots
    # For multiple plots, we'll check the environment after execution
    output <- utils::capture.output({
      # Open the graphics device
      grDevices::png(filename = plot_file, width = 800, height = 600)

      # Execute the code with the graphics device open
      result <- withVisible(eval(parse(text = code), envir = env))

      # If result is a ggplot object, print it while device is open
      if (result$visible && !is.null(result$value) && inherits(result$value, "ggplot")) {
        print(result$value)
      }

      # Close the graphics device
      grDevices::dev.off()

      # Check if a plot was created
      has_plot <- tryCatch({
        if (file.exists(plot_file)) {
          file_size <- file.info(plot_file)$size
          !is.null(file_size) && length(file_size) == 1 && file_size > 100
        } else {
          FALSE
        }
      }, error = function(e) {
        FALSE
      })

      # Print the result if it's visible (for non-plot objects)
      if (result$visible && (is.null(result$value) || !inherits(result$value, "ggplot"))) {
        print(result$value)
      }
    })
    
    # After execution, check for multiple ggplot objects that were just printed
    # Only look for plots that were explicitly printed in this code execution
    plot_files <- c()
    if (is_plot_command && has_plot) {
      # Parse code to find plot variable names that were printed (e.g., p1, p2, p3)
      code_lines <- strsplit(code, "\n")[[1]]
      # Look for lines that are just variable names (likely plot prints)
      # Pattern: lines that are just a variable name, possibly with print()
      plot_prints <- character(0)
      for (line in code_lines) {
        line_trimmed <- trimws(line)
        # Match: just a variable name, or print(variable_name)
        if (grepl("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*$", line_trimmed, perl = TRUE)) {
          var_match <- regmatches(line_trimmed, regexec("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*$", line_trimmed, perl = TRUE))[[1]]
          if (length(var_match) > 1) {
            plot_prints <- c(plot_prints, var_match[2])
          }
        } else if (grepl("print\\s*\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\)", line_trimmed, perl = TRUE)) {
          var_match <- regmatches(line_trimmed, regexec("print\\s*\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\)", line_trimmed, perl = TRUE))[[1]]
          if (length(var_match) > 1) {
            plot_prints <- c(plot_prints, var_match[2])
          }
        }
      }
      
      # Remove duplicates and check if variables exist and are ggplot objects
      plot_prints <- unique(plot_prints)
      valid_plot_vars <- character(0)
      for (var_name in plot_prints) {
        tryCatch({
          if (exists(var_name, envir = env)) {
            obj <- get(var_name, envir = env)
            if (inherits(obj, "ggplot")) {
              valid_plot_vars <- c(valid_plot_vars, var_name)
            }
          }
        }, error = function(e) {
          # Skip if we can't access the variable
        })
      }
      
      # If we found multiple plot objects that were printed, capture each one
      if (length(valid_plot_vars) > 1) {
        for (plot_var in valid_plot_vars) {
          tryCatch({
            plot_obj <- get(plot_var, envir = env)
            if (inherits(plot_obj, "ggplot")) {
              # Create separate file for this plot
              plot_file_single <- tempfile(fileext = ".png")
              grDevices::png(filename = plot_file_single, width = 800, height = 600)
              print(plot_obj)
              grDevices::dev.off()
              
              if (file.exists(plot_file_single)) {
                file_size <- tryCatch(file.info(plot_file_single)$size, error = function(e) 0)
                if (length(file_size) == 1 && file_size > 100) {
                  plot_files <- c(plot_files, plot_file_single)
                } else {
                  unlink(plot_file_single)
                }
              } else {
                unlink(plot_file_single)
              }
            }
          }, error = function(e) {
          })
        }
        
        # If we captured multiple plots, use those instead of the single plot_file
        if (length(plot_files) > 0) {
          plot_file <- plot_files
          has_plot <- as.logical(length(plot_files) > 0)  # Ensure single logical value
        }
      }
    }
    
    # Ensure has_plot is always a single logical value
    if (is.logical(has_plot)) {
      has_plot <- has_plot[1]
    } else {
      has_plot <- as.logical(has_plot)[1]
    }

    # Prepare the response
    response <- list(
      success = TRUE,
      output = paste(output, collapse = "\n")
    )

    # Include the result value if available
    if (exists("result") && !is.null(result$value)) {
      response$result <- if (is.data.frame(result$value)) {
        # For dataframes, convert to a readable format
        list(
          is_dataframe = TRUE,
          dimensions = dim(result$value),
          head = utils::head(result$value, 10)
        )
      } else if (inherits(result$value, "ggplot")) {
        # For ggplot objects, don't serialize - just return a simple message
        "ggplot object created (not serialized)"
      } else {
        # For other objects, check if they can be serialized to JSON
        # Complex objects like models, environments, etc. cannot be serialized
        obj_class <- class(result$value)
        
        # Check if object is serializable by attempting to convert to JSON
        is_serializable <- tryCatch({
          # Try to serialize a simple version
          jsonlite::toJSON(list(test = result$value), auto_unbox = TRUE)
          TRUE
        }, error = function(e) {
          FALSE
        })
        
        if (is_serializable && (is.atomic(result$value) || is.list(result$value))) {
          # Object is simple enough to serialize
          result$value
        } else {
          # Complex object - return a summary instead
          list(
            class = obj_class,
            type = typeof(result$value),
            summary = tryCatch({
              # Try to get a summary
              summary_output <- capture.output(print(result$value))
              if (length(summary_output) > 20) {
                # Truncate if too long
                c(summary_output[1:20], "... (truncated)")
              } else {
                summary_output
              }
            }, error = function(e) {
              paste("Complex object of class:", paste(obj_class, collapse = ", "))
            }),
            message = "Object created successfully but cannot be displayed in JSON format"
          )
        }
      }
    }

    # Include plot(s) if available
    if (is.logical(has_plot) && length(has_plot) == 1 && has_plot) {
      # Handle multiple plots or single plot
      plot_files_to_encode <- if (is.null(plot_file)) {
        NULL
      } else if (is.character(plot_file) && length(plot_file) > 1) {
        # Multiple plot files (vector)
        plot_file
      } else if (is.character(plot_file) && length(plot_file) == 1) {
        # Single plot file
        plot_file
      } else {
        NULL
      }
      
      if (!is.null(plot_files_to_encode)) {
        # Convert plots to base64 and add to output (same format as visualization agent)
        tryCatch({
          if (requireNamespace("base64enc", quietly = TRUE)) {
            # Encode each plot separately
            for (pf in plot_files_to_encode) {
              if (file.exists(pf)) {
                plot_base64 <- base64enc::base64encode(pf)
                # Add PLOT_DATA to output so frontend can detect and display it
                output <- c(output, paste0("PLOT_DATA:", plot_base64))
              }
            }
            # Update response output with plot data
            response$output <- paste(output, collapse = "\n")
          } else {
          }
        }, error = function(e) {
          # If base64enc is not available, just continue without base64 encoding
          cat("Warning: Error encoding plot -", e$message, "\n")
        })
      }
      
      response$plot <- list(
        created = TRUE,
        message = "Plot created and displayed in RStudio",
        mime_type = "image/png"
      )
    }
    
    return(response)
  }, error = function(e) {
    # Log the error if logging is enabled
    if (settings$log_to_file && !is.null(settings$log_file_path) && settings$log_file_path != "") {
      log_error_to_file(code, e$message, settings$log_file_path)
    }

    return(list(
      success = FALSE,
      error = e$message
    ))
  }, finally = {
    # Clean up temporary files
    # Use unlink() instead of file.remove() as it's more robust on Windows
    # Suppress warnings about file removal failures (file may be locked or already deleted)
    if (!is.null(plot_file)) {
      # Handle both single file and vector of files
      files_to_cleanup <- if (length(plot_file) == 1) {
        if (file.exists(plot_file)) plot_file else NULL
      } else {
        plot_file[sapply(plot_file, file.exists)]
      }
      
      if (!is.null(files_to_cleanup) && length(files_to_cleanup) > 0) {
        tryCatch({
          # Ensure graphics device is fully closed
          while (grDevices::dev.cur() != 1) {
            grDevices::dev.off()
          }
          # Use unlink with force=TRUE to handle locked files on Windows
          suppressWarnings(unlink(files_to_cleanup, force = TRUE))
        }, error = function(e) {
          # Silently ignore cleanup errors
        })
      }
    }
  })
}

#' Log code to file
#'
#' @param code The R code to log
#' @param log_path The path to the log file
#' @return Invisible NULL
log_code_to_file <- function(code, log_path) {
  # Create timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Format the log entry
  log_entry <- sprintf("# --- [%s] ---\n# Code executed by AI:\n%s\n\n", timestamp, code)

  # Create directory if it doesn't exist
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Append to the log file
  cat(log_entry, file = log_path, append = TRUE)

  invisible(NULL)
}

#' Log error to file
#'
#' @param code The R code that caused the error
#' @param error_message The error message
#' @param log_path The path to the log file
#' @return Invisible NULL
log_error_to_file <- function(code, error_message, log_path) {
  # Create timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Format the log entry
  log_entry <- sprintf("# --- [%s] ---\n# Code executed by AI (ERROR):\n%s\n# Error: %s\n\n",
                      timestamp, code, error_message)

  # Create directory if it doesn't exist
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Append to the log file
  cat(log_entry, file = log_path, append = TRUE)

  invisible(NULL)
}

#' Load execution settings
#'
#' @return A list containing execution settings
load_execution_settings <- function() {
  # Default settings
  default_settings <- list(
    print_to_console = TRUE,
    log_to_file = FALSE,
    log_file_path = file.path(path.expand("~"), "rstudioai_logs.R")
  )

  # Try to load settings from a settings file
  settings_file <- file.path(path.expand("~"), ".rstudioai_settings.rds")

  if (file.exists(settings_file)) {
    tryCatch({
      settings <- readRDS(settings_file)
      # Merge with defaults to ensure all fields exist
      settings <- modifyList(default_settings, settings)
      return(settings)
    }, error = function(e) {
      return(default_settings)
    })
  } else {
    return(default_settings)
  }
}

#' Save execution settings
#'
#' @param settings A list containing execution settings
#' @return Invisible NULL
save_execution_settings <- function(settings) {
  # Save settings to a settings file
  settings_file <- file.path(path.expand("~"), ".rstudioai_settings.rds")
  saveRDS(settings, settings_file)
  invisible(NULL)
}
