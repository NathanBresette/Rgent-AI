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

    # Capture all output
    output <- utils::capture.output({
      # Open the graphics device
      grDevices::png(filename = plot_file, width = 800, height = 600)

      # Execute the code with the graphics device open
      result <- withVisible(eval(parse(text = code), envir = env))

      # Close the graphics device
      grDevices::dev.off()

      # Check if a plot was created
      has_plot <- file.exists(plot_file) && file.info(plot_file)$size > 100

      # Print the result if it's visible
      if (result$visible) {
        print(result$value)
      }
    })

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

    # Include plot if available
    if (has_plot) {
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
    if (!is.null(plot_file) && file.exists(plot_file)) {
      tryCatch({
        # Ensure graphics device is fully closed
        while (grDevices::dev.cur() != 1) {
          grDevices::dev.off()
        }
        # Use unlink with force=TRUE to handle locked files on Windows
        suppressWarnings(unlink(plot_file, force = TRUE))
      }, error = function(e) {
        # Silently ignore cleanup errors
      })
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
