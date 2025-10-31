# Required libraries for execution engine
if (!requireNamespace("base64enc", quietly = TRUE)) {
  install.packages("base64enc")
}
library(base64enc)

# Transformation agent functions are loaded via package namespace
# Agent functions are loaded via package namespace when the package is loaded

# Debug wrapper functions to track down invalid 'envir' argument error
safe_ls <- function(envir) {
  
  ls(envir = envir)
}

safe_get <- function(name, envir) {
  # First check if object exists and is accessible
  if (!exists(name, envir = envir, inherits = FALSE)) {
    cat('DEBUG: Object', name, 'does not exist\n')
    return(NULL)
  }
  
  # Try to get the object safely
  tryCatch({
    
    obj <- get(name, envir = envir, inherits = FALSE)
    return(obj)
  }, error = function(e) {
    cat('DEBUG: Error getting object', name, ':', e$message, '\n')
    return(NULL)
  })
}

# Helper to safely check if an object is an environment before using as envir
safe_env <- function(e) {
  if (is.environment(e)) return(e)
  warning(paste0("Invalid envir argument passed (class: ", class(e), ", typeof: ", typeof(e), "); returning globalenv() as fallback."))
  cat("SAFE_ENV FALLBACK: class:", class(e), "type:", typeof(e), "\n")
  globalenv()
}

# Initialize global_env and nested structures as proper environments
if (!exists("global_env") || !is.environment(global_env)) {
  global_env <- new.env(parent = emptyenv())
  global_env$session_index <- new.env(parent = emptyenv())
  global_env$session_index$data_frames <- new.env(parent = emptyenv())
  global_env$session_index$file_chunks <- new.env(parent = emptyenv())
  global_env$session_index$file_hashes <- new.env(parent = emptyenv())
  global_env$session_index$functions <- new.env(parent = emptyenv())
  global_env$session_index$variables <- new.env(parent = emptyenv())
  
}

# Initialize conversation history
if (!exists("conversation_history") || !is.list(conversation_history)) {
  .GlobalEnv$conversation_history <- list()
  
}

# Helper function to get the path where access code should be stored
get_access_code_path <- function() {
  # Try to use R's standard user config directory (R 4.0+)
  # Use tryCatch to handle cases where tools::R_user_dir might not be available
  config_dir <- tryCatch({
    if (exists("R_user_dir", envir = asNamespace("tools"), mode = "function")) {
      tools::R_user_dir("rstudioai", "config")
    } else {
      stop("R_user_dir not available")
    }
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(config_dir)) {
    # Ensure directory exists
    if (!dir.exists(config_dir)) {
      dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(file.path(config_dir, "access_code.txt"))
  } else {
    # Fallback for older R versions
    fallback_dir <- path.expand("~/.rstudioai")
    if (!dir.exists(fallback_dir)) {
      dir.create(fallback_dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(file.path(fallback_dir, "access_code.txt"))
  }
}

# Save access code to disk for persistence across sessions
save_access_code_to_disk <- function(code) {
  tryCatch({
    if (is.null(code) || length(code) == 0 || nchar(trimws(code)) == 0) {
      return(invisible(FALSE))
    }
    
    code_path <- get_access_code_path()
    
    # Create directory if it doesn't exist (in case fallback is used)
    code_dir <- dirname(code_path)
    if (!dir.exists(code_dir)) {
      dir.create(code_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Write access code to file (writeLines handles file connection internally)
    writeLines(code, code_path)
    return(invisible(TRUE))
  }, error = function(e) {
    return(invisible(FALSE))
  })
}

# Load access code from disk if it exists
load_access_code_from_disk <- function() {
  tryCatch({
    code_path <- get_access_code_path()
    
    if (!file.exists(code_path)) {
      return(NULL)
    }
    
    # Read the access code from file
    code <- readLines(code_path, warn = FALSE)
    
    # Return NULL if file is empty or code is invalid
    if (length(code) == 0 || nchar(trimws(code[1])) == 0) {
      return(NULL)
    }
    
    return(trimws(code[1]))
  }, error = function(e) {
    return(NULL)
  })
}

# Helper function to get current access code
get_current_access_code <- function() {
  if(exists("current_access_code", envir = .GlobalEnv)) {
    .GlobalEnv$current_access_code
  } else {
    stop("No access code set. Please validate your access code in the interface first.")
  }
}

#' Launch Rgent
#' @export
run_rgent <- function() {
  # Check if RStudio API is available
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("RStudio API is not available. Please run this in RStudio.")
  }
  
  # Check for required packages
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    stop("httpuv package is required. Install with: install.packages('httpuv')")
  }
  
  # Load access code from disk if available (persistent across sessions)
  saved_access_code <- load_access_code_from_disk()
  if (!is.null(saved_access_code)) {
    .GlobalEnv$current_access_code <- saved_access_code
  }
  
  # Initialize auto-capture system
  .GlobalEnv$auto_capture_active <- TRUE
  .GlobalEnv$last_error_message <- ""
  
  # Set up task callback to detect errors
  error_callback <- function(expr, value, ok, visible) {
    if (!ok) {
      # Error occurred - get the error message
      current_error <- geterrmessage()
      
      # Check if this is a new error
      if (current_error != "" && current_error != .GlobalEnv$last_error_message) {
        .GlobalEnv$last_error_message <- current_error
        auto_capture_error()
      }
    }
    return(TRUE)  # Keep the callback active
  }
  
  # Add the task callback
  addTaskCallback(error_callback, name = "error_monitor")
  
  # Start WebSocket server
  start_websocket_server()
  
  # Create and launch HTML interface
  launch_html_interface()
}

#' Backwards compatibility alias
#' @export
launch_websocket_chat <- run_rgent

#' Auto-start Rgent if conditions are met
#' 
#' Checks if RStudio is available and WebSocket server isn't already running,
#' then starts Rgent automatically.
#' 
#' @return Invisibly returns TRUE if started, FALSE otherwise
#' @export
auto_start_rgent <- function() {
  cat("DEBUG auto_start_rgent: Function called\n")
  tryCatch({
    # Check if RStudio API is available
    cat("DEBUG auto_start_rgent: Checking RStudio API...\n")
    if (!requireNamespace("rstudioapi", quietly = TRUE)) {
      cat("DEBUG auto_start_rgent: rstudioapi package not available\n")
      return(invisible(FALSE))
    }
    
    if (!rstudioapi::isAvailable()) {
      cat("DEBUG auto_start_rgent: RStudio API not available\n")
      return(invisible(FALSE))
    }
    
    cat("DEBUG auto_start_rgent: RStudio API is available\n")
    
    # Note: We don't check if websocket_server variable exists because:
    # 1. The variable might exist in .RData but server isn't actually running
    # 2. run_rgent() -> start_websocket_server() already handles stopping any existing server
    # 3. It's safe to call run_rgent() even if server appears to exist
    
    cat("DEBUG auto_start_rgent: All conditions met, starting Rgent...\n")
    cat("DEBUG auto_start_rgent: (start_websocket_server() will stop any existing stale server)\n")
    # All conditions met, start Rgent
    # Note: start_websocket_server() stops any existing server before starting new one
    run_rgent()
    cat("DEBUG auto_start_rgent: Rgent started successfully\n")
    return(invisible(TRUE))
  }, error = function(e) {
    cat("DEBUG auto_start_rgent: Error occurred:", e$message, "\n")
    return(invisible(FALSE))
  })
}

#' Set up .Rprofile for automatic Rgent startup
#' 
#' Adds library(rstudioai) to the user's .Rprofile so Rgent auto-starts
#' when RStudio opens. Only adds if not already present.
#' 
#' @return Invisibly returns TRUE if setup was successful or already exists, FALSE otherwise
#' @export
setup_rprofile_auto_start <- function() {
  cat("DEBUG setup_rprofile_auto_start: Function called\n")
  tryCatch({
    # Get path to user's .Rprofile
    rprofile_path <- path.expand("~/.Rprofile")
    cat("DEBUG setup_rprofile_auto_start: Checking .Rprofile at:", rprofile_path, "\n")
    
    # Check if auto-start code is already present
    if (file.exists(rprofile_path)) {
      cat("DEBUG setup_rprofile_auto_start: .Rprofile exists\n")
      rprofile_content <- readLines(rprofile_path, warn = FALSE)
      cat("DEBUG setup_rprofile_auto_start: .Rprofile has", length(rprofile_content), "lines\n")
      # Check if enhanced version is already present
      has_rstudioai <- any(grepl("library\\(rstudioai\\)|require\\(rstudioai\\)", rprofile_content, ignore.case = TRUE))
      has_comment <- any(grepl("Auto-start Rgent when RStudio opens", rprofile_content, fixed = TRUE))
      has_first <- any(grepl("\\.First", rprofile_content, ignore.case = TRUE))
      has_callbacks <- any(grepl("addTaskCallback.*rgent|rgent_startup", rprofile_content))
      cat("DEBUG setup_rprofile_auto_start: has_rstudioai =", has_rstudioai, ", has_comment =", has_comment, ", has_first =", has_first, ", has_callbacks =", has_callbacks, "\n")
      # If it has our comment AND has .First() or callbacks, consider it the new version
      if (has_comment && has_rstudioai && (has_first || has_callbacks)) {
        cat("DEBUG setup_rprofile_auto_start: Already set up with enhanced version, skipping\n")
        return(invisible(TRUE))  # Already set up with enhanced version
      }
      # If old version exists, we'll replace/add the new version below
    } else {
      cat("DEBUG setup_rprofile_auto_start: .Rprofile does not exist, will create\n")
    }
    
    # Create or append to .Rprofile
    # Use .First() to run after R initialization, plus set up a callback that fires on any command
    auto_start_lines <- c(
      "",
      "# Auto-start Rgent when RStudio opens",
      "if (interactive() && requireNamespace(\"rstudioai\", quietly = TRUE)) {",
      "  library(rstudioai)",
      "  # Set up persistent callback that will fire on next command execution",
      "  .rgent_startup_callback <- function(expr, value, ok, visible) {",
      "    cat(\"DEBUG .rgent_startup_callback: Fired\\n\")",
      "    if (requireNamespace(\"rstudioapi\", quietly = TRUE) && rstudioapi::isAvailable()) {",
      "      cat(\"DEBUG .rgent_startup_callback: RStudio available, starting Rgent...\\n\")",
      "      tryCatch(rstudioai::auto_start_rgent(), error = function(e) NULL)",
      "      tryCatch(removeTaskCallback(\".rgent_startup\"), error = function(e) NULL)",
      "      return(FALSE)",
      "    } else {",
      "      cat(\"DEBUG .rgent_startup_callback: RStudio not ready yet\\n\")",
      "      return(TRUE)",
      "    }",
      "  }",
      "  addTaskCallback(.rgent_startup_callback, name = \".rgent_startup\")",
      "  cat(\"DEBUG .Rprofile: Callback set up\\n\")",
      "  # Set up .First() to run after R initialization (preserves any existing .First)",
      "  if (exists(\".First\", envir = .GlobalEnv, inherits = FALSE) && is.function(.First)) {",
      "    .First_original <- .First",
      "    .First <- function() {",
      "      cat(\"DEBUG .First(): Running (preserved original .First)\\n\")",
      "      .First_original()",
      "      cat(\"DEBUG .First(): Checking RStudio availability...\\n\")",
      "      # .First() runs after R initialization - check if RStudio is ready now",
      "      if (requireNamespace(\"rstudioapi\", quietly = TRUE) && rstudioapi::isAvailable()) {",
      "        cat(\"DEBUG .First(): RStudio available, starting Rgent...\\n\")",
      "        tryCatch(rstudioai::auto_start_rgent(), error = function(e) {",
      "          cat(\"DEBUG .First(): Error starting Rgent:\", e$message, \"\\n\")",
      "        })",
      "      } else {",
      "        cat(\"DEBUG .First(): RStudio not ready, will be handled by polling/callbacks...\\n\")",
      "      }",
      "    }",
      "  } else {",
      "    .First <- function() {",
      "      cat(\"DEBUG .First(): Running (new .First)\\n\")",
      "      cat(\"DEBUG .First(): Checking RStudio availability...\\n\")",
      "      if (requireNamespace(\"rstudioapi\", quietly = TRUE) && rstudioapi::isAvailable()) {",
      "        cat(\"DEBUG .First(): RStudio available, starting Rgent...\\n\")",
      "        tryCatch(rstudioai::auto_start_rgent(), error = function(e) {",
      "          cat(\"DEBUG .First(): Error starting Rgent:\", e$message, \"\\n\")",
      "        })",
      "      } else {",
      "        cat(\"DEBUG .First(): RStudio not ready, will be handled by polling/callbacks...\\n\")",
      "      }",
      "    }",
      "  }",
      "  # Set up RStudio session hook for reliable auto-start",
      "  cat(\"DEBUG .Rprofile: Setting up RStudio session hook...\\n\")",
      "  tryCatch({",
      "    if (exists(\"setHook\", envir = baseenv())) {",
      "      setHook(\"rstudio.sessionInit\", function(newSession) {",
      "        cat(\"DEBUG rstudio.sessionInit hook: Fired, newSession =\", newSession, \"\\n\")",
      "        if (newSession) {",
      "          cat(\"DEBUG rstudio.sessionInit hook: New session, scheduling Rgent start...\\n\")",
      "          # Use later package if available, otherwise use a simple delay via task callback",
      "          if (requireNamespace(\"later\", quietly = TRUE)) {",
      "            later::later(function() {",
      "              cat(\"DEBUG rstudio.sessionInit hook: Delayed start executing...\\n\")",
      "              tryCatch({",
      "                if (requireNamespace(\"rstudioapi\", quietly = TRUE) && rstudioapi::isAvailable()) {",
      "                  cat(\"DEBUG rstudio.sessionInit hook: RStudio available, starting Rgent...\\n\")",
      "                  tryCatch(rstudioai::auto_start_rgent(), error = function(e) {",
      "                    cat(\"DEBUG rstudio.sessionInit hook: Error starting Rgent:\", e$message, \"\\n\")",
      "                  })",
      "                } else {",
      "                  cat(\"DEBUG rstudio.sessionInit hook: RStudio not available yet\\n\")",
      "                }",
      "              }, error = function(e) {",
      "                cat(\"DEBUG rstudio.sessionInit hook: Error in delayed function:\", e$message, \"\\n\")",
      "              })",
      "            }, delay = 0.5)",
      "          } else {",
      "            # Fallback: use task callback to delay",
      "            .rgent_hook_callback <- function(expr, value, ok, visible) {",
      "              cat(\"DEBUG rstudio.sessionInit hook: Fallback callback fired\\n\")",
      "              tryCatch({",
      "                if (requireNamespace(\"rstudioapi\", quietly = TRUE) && rstudioapi::isAvailable()) {",
      "                  cat(\"DEBUG rstudio.sessionInit hook: RStudio available, starting Rgent...\\n\")",
      "                  tryCatch(rstudioai::auto_start_rgent(), error = function(e) NULL)",
      "                  tryCatch(removeTaskCallback(\".rgent_hook\"), error = function(e) NULL)",
      "                  return(FALSE)",
      "                }",
      "              }, error = function(e) NULL)",
      "              return(TRUE)",
      "            }",
      "            addTaskCallback(.rgent_hook_callback, name = \".rgent_hook\")",
      "          }",
      "        }",
      "      }, action = \"append\")",
      "      cat(\"DEBUG .Rprofile: RStudio session hook set up successfully\\n\")",
      "    } else {",
      "      cat(\"DEBUG .Rprofile: setHook not available, using fallback approach...\\n\")",
      "    }",
      "  }, error = function(e) {",
      "    cat(\"DEBUG .Rprofile: Error setting up RStudio hook:\", e$message, \"\\n\")",
      "  })",
      "  # Also try immediately if RStudio is already ready",
      "  cat(\"DEBUG .Rprofile: Checking if RStudio is ready immediately...\\n\")",
      "  if (requireNamespace(\"rstudioapi\", quietly = TRUE) && rstudioapi::isAvailable()) {",
      "    cat(\"DEBUG .Rprofile: RStudio ready immediately, starting Rgent...\\n\")",
      "    tryCatch(rstudioai::auto_start_rgent(), error = function(e) NULL)",
      "  } else {",
      "    cat(\"DEBUG .Rprofile: RStudio not ready immediately, will use hook or callbacks\\n\")",
      "    # Set up fallback polling callback if hook fails",
      "    .rgent_fallback_callback <- function(expr, value, ok, visible) {",
      "      cat(\"DEBUG .rgent_fallback_callback: Fired\\n\")",
      "      if (requireNamespace(\"rstudioapi\", quietly = TRUE) && rstudioapi::isAvailable()) {",
      "        cat(\"DEBUG .rgent_fallback_callback: RStudio available, starting Rgent...\\n\")",
      "        tryCatch(rstudioai::auto_start_rgent(), error = function(e) NULL)",
      "        tryCatch(removeTaskCallback(\".rgent_fallback\"), error = function(e) NULL)",
      "        return(FALSE)",
      "      }",
      "      return(TRUE)",
      "    }",
      "    addTaskCallback(.rgent_fallback_callback, name = \".rgent_fallback\")",
      "  }",
      "}"
    )
    
    if (file.exists(rprofile_path)) {
      cat("DEBUG setup_rprofile_auto_start: Appending to existing .Rprofile\n")
      # Read existing content and append
      existing_lines <- readLines(rprofile_path, warn = FALSE)
      all_lines <- c(existing_lines, auto_start_lines)
      writeLines(all_lines, con = rprofile_path)
    } else {
      cat("DEBUG setup_rprofile_auto_start: Creating new .Rprofile\n")
      # Create new file
      writeLines(auto_start_lines, con = rprofile_path)
    }
    
    cat("DEBUG setup_rprofile_auto_start: Successfully set up .Rprofile\n")
    return(invisible(TRUE))
  }, error = function(e) {
    cat("DEBUG setup_rprofile_auto_start: Error occurred:", e$message, "\n")
    return(invisible(FALSE))
  })
}

#' Get RStudio theme information
#' @return List containing theme information
#' @export
get_rstudio_theme <- function() {
  tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      # Get detailed RStudio theme information
      theme_info <- list(
        is_dark = FALSE,
        theme_name = "default",
        editor_theme = "default",
        global_theme = "default",
        colors = list()
      )
      
      # Try to get detailed theme information from RStudio
      tryCatch({
        theme_data <- rstudioapi::getThemeInfo()
        if (!is.null(theme_data)) {
          # Extract theme details
          theme_info$is_dark <- theme_data$dark
          theme_info$theme_name <- theme_data$editor
          theme_info$global_theme <- theme_data$global
          theme_info$editor_theme <- theme_data$editor
          
          # Extract color information
          theme_info$colors <- list(
            foreground = theme_data$foreground,
            background = theme_data$background,
            console_background = theme_data$console_background,
            console_foreground = theme_data$console_foreground
          )
        }
      }, error = function(e) {
        cat("Could not get detailed theme info:", e$message, "\n")
        # Fallback to basic detection
        theme_info$is_dark <- TRUE  # Default to dark for modern RStudio
      })
      
      return(theme_info)
    } else {
      return(list(is_dark = TRUE, theme_name = "default", colors = list()))
    }
  }, error = function(e) {
    cat("Error getting RStudio theme:", e$message, "\n")
    return(list(is_dark = TRUE, theme_name = "default", colors = list()))
  })
}

#' Start WebSocket server for R-JavaScript communication
#' @export
start_websocket_server <- function() {
  # Stop any existing server
  if (!is.null(.GlobalEnv$websocket_server)) {
    tryCatch({
      httpuv::stopServer(.GlobalEnv$websocket_server)
    }, error = function(e) {
      # no-op
    })
  }
  
  # Clear any existing server reference
  .GlobalEnv$websocket_server <- NULL
  
  # Define message handler function
    message_handler <- function(ws, isBinary, data) {
    # Global error handler to catch any unhandled errors
    tryCatch({
      # Parse JSON message
      request <- jsonlite::fromJSON(data)
    
      # Handle different actions
      response <- switch(request$action,
        "set_access_code" = {
          # Check if access code is different from what's already stored
          current_code <- if(exists("current_access_code", envir = .GlobalEnv)) .GlobalEnv$current_access_code else NULL
          if (is.null(current_code) || current_code != request$access_code) {
            # Store access code from frontend in globalenv (for current session)
            .GlobalEnv$current_access_code <- request$access_code
            # Also save to disk for persistence across sessions
            save_access_code_to_disk(request$access_code)
          }
          list(action = "access_code_set", status = "success")
        },
        "get_context" = {
          # Get current R context
          context <- tryCatch({
            capture_context()
          }, error = function(e) {
            cat("ERROR in get_context:", e$message, "\n")
            list(
              workspace_objects = list(),
              active_file = NULL,
              file_contents = NULL
            )
          })
          list(action = "context", data = context)
        },
        "get_dataframes" = {
          # Get available DataFrames for agent configuration
          dataframes <- get_available_dataframes()
          list(action = "dataframes", data = dataframes)
        },
        "get_dataframe_variables" = {
          # Get variables from a specific DataFrame
          dataframe_name <- request$dataframe
          exclude_target <- request$exclude_target_variable
          
          if (is.null(dataframe_name) || !exists(dataframe_name, envir = globalenv())) {
            list(action = "dataframe_variables", data = list())
          } else {
            df <- get(dataframe_name, envir = globalenv())
            if (is.data.frame(df)) {
              # Get all column names
              all_cols <- names(df)
              
              # Exclude target variable if specified
              if (!is.null(exclude_target) && exclude_target != "") {
                all_cols <- all_cols[all_cols != exclude_target]
              }
              
              variables <- lapply(all_cols, function(col_name) {
                col_data <- df[[col_name]]
                list(
                  name = col_name,
                  type = if (is.numeric(col_data)) "numeric" else "categorical"
                )
              })
              list(action = "dataframe_variables", data = variables)
            } else {
              list(action = "dataframe_variables", data = list())
            }
          }
        },
        "get_dataframe_info" = {
          # Get detailed information about a specific DataFrame
          df_info <- get_dataframe_info(request$dataframe)
          list(action = "dataframe_info", data = df_info)
        },
        "start_cleaning_agent" = {
          # Start the cleaning agent workflow
          result <- start_cleaning_agent(request$dataframe, request$na_handling, request$cleaning_options, request$method_options, request$custom_inputs, request$other_operations, request$selected_variables)
          list(action = "agent_started", data = result)
        },
        "next_agent_step" = {
          # Execute next step in cleaning agent workflow
          result <- execute_cleaning_agent(request$dataframe, request$na_handling, request$step)
          list(action = "agent_step", data = result)
        },
        "get_next_step_code" = {
          # Determine if this is a transformation agent or cleaning agent based on request parameters
          if (!is.null(request$transformation_options)) {
            # This is a transformation agent request
            step_code <- get_next_transformation_step_code(request$step_info, request$dataframe, request$method_options, request$custom_inputs)
          } else {
            # This is a cleaning agent request
            step_code <- generate_step_code(request$step_info, request$dataframe, request$na_handling, request$method_options, request$custom_inputs, request$selected_variables)
          }
          
          # Send back the code to execute
          list(action = "execute_code_response", code = step_code)
        },
        "start_transformation_agent" = {
          # Start the transformation agent workflow
          result <- start_transformation_agent(request$dataframe, request$transformation_options, request$method_options, request$custom_inputs, request$selected_variables)
          list(action = "transformation_agent_started", data = result)
        },
        "get_next_transformation_step_code" = {
          # Generate code for the next transformation step
          step_code <- get_next_transformation_step_code(request$step_info, request$dataframe, request$method_options, request$custom_inputs, request$selected_variables)
          
          # Send back the code to execute
          list(action = "execute_code_response", code = step_code)
        },
        "start_statistical_agent" = {
          # Start the statistical agent workflow
          result <- start_statistical_analysis(request$dataframe, request$analysis_options, request$variables, request$method_options, request$custom_inputs)
          list(action = "statistical_agent_started", data = result)
        },
        "get_next_statistical_step_code" = {
          # Execute the statistical step directly (no code generation)
          df <- get(request$dataframe, envir = .GlobalEnv)
          step_result <- execute_statistical_step(df, request$step_info, request$variables, request$method_options, request$custom_inputs)
          
          # Send back the step results
          list(action = "statistical_step_result", data = step_result)
        },
        "start_modeling_agent" = {
          # Start the modeling agent workflow
          result <- start_modeling_agent(request$dataframe, request$target_variable, request$algorithms, request$options, custom_inputs = NULL, selected_variables = request$selected_variables)
          list(action = "modeling_agent_started", data = result)
        },
        "get_next_modeling_step_code" = {
          # Generate code for the next modeling step
          step_result <- get_next_modeling_step(request$dataframe, request$step_info, request$target_variable, request$algorithms, request$selected_variables)
          
          # Send back the code to execute
          list(action = "execute_code_response", code = step_result$code)
        },
        "insert_code" = {
          # Insert code directly using rstudioapi
          tryCatch({
            # Ensure we have an active document
            if (rstudioapi::isAvailable()) {
              rstudioapi::insertText(request$code)
              list(action = "inserted", status = "ok")
            } else {
              list(action = "inserted", status = "error", message = "RStudio API not available")
            }
          }, error = function(e) {
            list(action = "inserted", status = "error", message = e$message)
          })
        },
        "execute_code" = {
          # Store the code for error handling
          .GlobalEnv$currentStepCode <- request$code
          
          # Execute code using proven execution engine approach
          tryCatch({
            # Load execution settings
            settings <- list(
              print_to_console = TRUE,
              log_to_file = FALSE,
              log_file_path = file.path(path.expand("~"), "rstudioai_logs.R")
            )
            
            # Execute the code using proven methods
            result <- execute_code_in_session(request$code, settings)
            
            # Return structured result
            list(
              action = "executed", 
              status = if(result$success) "success" else "error",
              result = result$result,
              output = result$output,
              error = if(!result$success) result$error else NULL,
              plot = result$plot
            )
          }, error = function(e) {
            list(
              action = "executed", 
              status = "error",
              error = e$message,
              output = NULL,
              result = NULL
            )
          })
        },
        
        "execute_and_fix" = {
          # Execute code and if it fails, ask AI to fix it
          tryCatch({
            # Debug: Print the code being executed
            cat("Executing code:", request$code, "\n")
            
            # Get current document context
            doc <- rstudioapi::getActiveDocumentContext()
            if (is.null(doc)) {
              list(action = "execute_and_fix", status = "error", error = "No active document")
            } else {
              # Get current cursor position
              cursor_pos <- doc$selection[[1]]$range$start
              start_line <- cursor_pos[["row"]]
              start_col <- cursor_pos[["column"]]
              
              # Insert the code into the script
              rstudioapi::insertText(request$code)
              
              # Calculate the range of lines we just inserted
              code_lines <- strsplit(request$code, "\n")[[1]]
              num_lines <- length(code_lines)
              end_line <- start_line + num_lines - 1
              
              # Create range for the inserted code
              start_range <- rstudioapi::document_position(start_line, 1)
              end_range <- rstudioapi::document_position(end_line, nchar(code_lines[num_lines]) + 1)
              code_range <- rstudioapi::document_range(start_range, end_range)
              
              # Select the inserted code
              rstudioapi::setSelectionRanges(list(code_range))
              
              # Execute the selected code (which is our inserted code)
              output <- capture.output({
                rstudioapi::executeCommand("executeSelection")
              })
              
              # Get any errors
              last_error <- geterrmessage()
            
            if(last_error != "") {
              # Code failed, ask AI to fix it
              error_context <- tryCatch({
                capture_context()
              }, error = function(e) {
                cat("ERROR in error context capture:", e$message, "\n")
                list(
                  workspace_objects = list(),
                  active_file = NULL,
                  file_contents = NULL
                )
              })
              error_context$last_error <- last_error
              error_context$failed_code <- request$code
              
              # Call AI to fix the code
              fix_response <- httr::POST(
                "https://rgent.onrender.com/chat",
                body = list(
                  access_code = get_current_access_code(),
                  prompt = paste("The following R code failed with error:", last_error, 
                               "\n\nFailed code:\n", request$code,
                               "\n\nPlease provide a corrected version."),
                  context_data = error_context
                ),
                encode = "json",
                httr::timeout(30)
              )
              
              if (httr::status_code(fix_response) == 200) {
                fix_result <- httr::content(fix_response)
                list(
                  action = "execute_and_fix",
                  status = "error_fixed",
                  original_error = last_error,
                  fixed_code = fix_result$response,
                  output = output
                )
              } else {
                list(
                  action = "execute_and_fix",
                  status = "error",
                  error = last_error,
                  output = output
                )
              }
            } else {
              # Code succeeded
              list(
                action = "execute_and_fix",
                status = "success",
                result = NULL,
                output = output
              )
            }
          }
          }, error = function(e) {
            # Code failed with exception
            error_context <- tryCatch({
              capture_context()
            }, error = function(e2) {
              cat("ERROR in error context capture:", e2$message, "\n")
              list(
                workspace_objects = list(),
                active_file = NULL,
                file_contents = NULL
              )
            })
            error_context$last_error <- e$message
            error_context$failed_code <- request$code
            
            # Call AI to fix the code
            fix_response <- httr::POST(
              "https://rgent.onrender.com/chat",
              body = list(
                access_code = get_current_access_code(),
                prompt = paste("The following R code failed with error:", e$message, 
                             "\n\nFailed code:\n", request$code,
                             "\n\nPlease provide a corrected version."),
                context_data = error_context
              ),
              encode = "json",
              httr::timeout(30)
            )
            
            if (httr::status_code(fix_response) == 200) {
              fix_result <- httr::content(fix_response)
              list(
                action = "execute_and_fix",
                status = "error_fixed",
                original_error = e$message,
                fixed_code = fix_result$response,
                output = NULL
              )
            } else {
              list(
                action = "execute_and_fix",
                status = "error",
                error = e$message,
                output = NULL
              )
            }
          })
        },
        "get_cursor" = {
          # Get current cursor position
          doc <- rstudioapi::getActiveDocumentContext()
          if (!is.null(doc)) {
            list(action = "cursor", 
                 line = doc$selection[[1]]$range$start[["row"]],
                 column = doc$selection[[1]]$range$start[["column"]])
          } else {
            list(action = "cursor", line = 1, column = 1)
          }
        },
        "chat_with_ai" = {
          # Use intelligent indexing system
          
          
          # Add user message to conversation history
          add_to_conversation_history("user", request$message)
          
          # Direct workspace scanning (no session_index dependency)
          
          # Get intelligent context based on query
          intelligent_context <- assemble_intelligent_context_safe(request$message)
          
          # Get current context with smart filtering (use simple approach)
          current_context <- capture_context_smart(request$message)
          
          # Check if we have a session, if not initialize one
          if (is.null(.GlobalEnv$current_session_id)) {
            cat("No session found, initializing...\n")
            session_id <- paste0("session_", as.numeric(Sys.time()))
            .GlobalEnv$current_session_id <- session_id
            .GlobalEnv$last_context_state <- current_context
            
            # Initialize session with backend
            tryCatch({
              init_response <- httr::POST(
                "https://rgent.onrender.com/api/initialize-session",
                body = list(
                  access_code = current_access_code,
                  session_id = session_id,
                  initial_context = current_context
                ),
                encode = "json",
                httr::timeout(10)
              )
              cat("Session initialized\n")
            }, error = function(e) {
              cat("Session initialization failed:", e$message, "\n")
            })
          } else {
            # Detect changes since last context
            last_context <- .GlobalEnv$last_context_state
            if (!is.null(last_context)) {
              changes <- detect_context_changes(last_context, current_context)
              
              
              # Store changes in backend
              tryCatch({
                store_response <- httr::POST(
                  "https://rgent.onrender.com/api/store-changes",
                  body = list(
                    access_code = current_access_code,
                    session_id = .GlobalEnv$current_session_id,
                    changes = changes
                  ),
                  encode = "json",
                  httr::timeout(10)
                )
                
              }, error = function(e) {
                cat("Failed to store changes:", e$message, "\n")
              })
            }
            
            # Update stored context
            .GlobalEnv$last_context_state <- current_context
          }
          
          # Use streaming RAG chat endpoint
          tryCatch({
            # Prepare request body for simple chat
            
            # Check if context has meaningful data
            if (!is.null(current_context$workspace_objects)) {
              object_names <- sapply(current_context$workspace_objects, function(obj) obj$name)
            }
            
            if (!is.null(current_context$file_info)) {
              if (!is.null(current_context$file_info$file_contents)) {
              }
            }
            
            # Debug workspace_objects
            if ("workspace_objects" %in% names(current_context)) {
              if (length(current_context$workspace_objects) > 0) {
              }
            }
            
            # Optionally inspect context components (silent)
            
            # Get access code from global environment or use default
            current_access_code <- if (!is.null(.GlobalEnv$current_access_code)) .GlobalEnv$current_access_code else "DEMO123"
            
            # Ensure workspace_objects is always an array, not a named list
            workspace_objects_array <- if (is.list(current_context$workspace_objects) && !is.null(names(current_context$workspace_objects))) {
              # Convert named list to array
              unname(current_context$workspace_objects)
            } else {
              # Already an array or NULL
              current_context$workspace_objects
            }
            
            request_body <- list(
              access_code = current_access_code,
              prompt = request$message,
              context_data = list(
                workspace_objects = workspace_objects_array,
                file_info = current_context$file_info,
                environment_info = current_context$environment_info,
                timestamp = current_context$timestamp
              )
            )
            
            # Debug the final request structure
            
                          
            
                        
            # Debug the actual JSON being sent
            request_json <- jsonlite::toJSON(request_body, auto_unbox = TRUE)
            
            
            response <- httr::POST(
              "https://rgent.onrender.com/chat/stream",
              body = request_body,
              encode = "json",
              httr::timeout(30),  # 30 second timeout
              httr::add_headers("Accept" = "text/event-stream")
            )
            

            
            if (httr::status_code(response) == 200) {
              # Handle streaming response
                             response_text <- httr::content(response, "text")
              
              # Parse Server-Sent Events (SSE) format
                             lines <- strsplit(response_text, "\n")[[1]]
               chunks <- c()
               full_response <- ""
              
              for (line in lines) {
                if (grepl("^data: ", line)) {
                  # Extract JSON data from SSE format
                  json_data <- substring(line, 7)  # Remove "data: " prefix
                  if (json_data != "[DONE]") {
                    tryCatch({
                      chunk_data <- jsonlite::fromJSON(json_data)
                      
                      if (!is.null(chunk_data$chunk)) {
                        chunks <- c(chunks, chunk_data$chunk)
                        full_response <- paste0(full_response, chunk_data$chunk)
                        
                        # Accumulate chunks and send in larger batches
                        if (!exists("current_chunk_buffer")) {
                          current_chunk_buffer <- ""
                        }
                        current_chunk_buffer <- paste0(current_chunk_buffer, chunk_data$chunk)
                        
                        # Check if we're in the middle of a code block
                        code_block_open <- grepl("```[^`]*$", current_chunk_buffer)
                        code_block_complete <- grepl("```.*```", current_chunk_buffer)
                        
                        # Also check if we have an incomplete function call (starts with function name but no closing)
                        incomplete_function <- grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\([^)]*$", current_chunk_buffer)
                        
                        # Send chunk to frontend when we have a complete word or sentence
                        # Also send when we have a complete code block
                        # Don't send if we're in the middle of a code block or incomplete function
                        if (!code_block_open && !incomplete_function && (grepl("[.!?]\\s*$", current_chunk_buffer) || 
                            grepl("\\n\\n", current_chunk_buffer) ||
                            grepl("```$", current_chunk_buffer) ||
                            nchar(current_chunk_buffer) > 100)) {
                          
                        chunk_response <- list(
                          action = "ai_response",
                          streaming = TRUE,
                            chunk = current_chunk_buffer
                        )
                        ws$send(jsonlite::toJSON(chunk_response, auto_unbox = TRUE))
                          
                          # Reset buffer
                          current_chunk_buffer <- ""
                        }
                      }
                      
                      # Check if this is the final chunk
                                               if (!is.null(chunk_data$done) && chunk_data$done) {
                           
                           # Send any remaining buffer
                        if (exists("current_chunk_buffer") && nchar(current_chunk_buffer) > 0) {
                          chunk_response <- list(
                            action = "ai_response",
                            streaming = TRUE,
                            chunk = current_chunk_buffer
                          )
                          ws$send(jsonlite::toJSON(chunk_response, auto_unbox = TRUE))
                        }
                        
                        break
                      }
                    }, error = function(e) {
                      cat("Error parsing chunk:", e$message, "\n")
                    })
                  }
                }
              }
              
              # Add AI response to conversation history
              if (nchar(full_response) > 0) {
                add_to_conversation_history("ai", full_response)
              }
              
              # Send finish signal
              finish_response <- list(
                action = "ai_response",
                streaming = TRUE,
                finished = TRUE
              )
              ws$send(jsonlite::toJSON(finish_response, auto_unbox = TRUE))
              
              list(action = "streaming_complete")
            } else {
              cat("Backend returned error status:", httr::status_code(response), "\n")
              error_content <- httr::content(response)
              cat("Error content:", toString(error_content), "\n")
              # Silenced verbose debug: Full error response
              list(action = "ai_response", message = paste("Error connecting to AI service. Status:", httr::status_code(response)))
            }
          }, error = function(e) {
            cat("Exception during backend call:", e$message, "\n")
            # Silenced debug: Exception details
            # Silenced debug: Exception class
            
            # Check if it's a timeout error
            if (grepl("timeout", tolower(e$message)) || grepl("timed out", tolower(e$message))) {
              # First retry with increased timeout
              # Silenced debug: Timeout detected, attempting retry
              
              # Send "One moment..." message to user
              retry_message <- list(
                action = "ai_response", 
                message = "⏳ One moment, processing your request...",
                streaming = FALSE
              )
              ws$send(jsonlite::toJSON(retry_message, auto_unbox = TRUE))
              
              # Wait a moment
              Sys.sleep(2)
              
              # Retry with increased timeout
              tryCatch({
                response <- httr::POST(
                  "https://rgent.onrender.com/chat/stream",
                  body = request_body,
                  encode = "json",
                  httr::timeout(60),  # Increased to 60 seconds
                  httr::add_headers("Accept" = "text/event-stream")
                )
                
                if (httr::status_code(response) == 200) {
                  # Process the successful response
                  response_text <- httr::content(response, "text")
                  # ... (same processing as before)
                  list(action = "ai_response", message = "✅ Request completed successfully!")
                } else {
                  # Still failed, send environment reduction tips
                  tips_message <- list(
                    action = "ai_response", 
                    message = paste(
                      "❌ Still experiencing delays. Try reducing your environment size:\n\n",
                      "💡 **Tips to reduce environment size:**\n",
                      "• Remove unused variables: `rm(unused_var)`\n", 
                      "• Clear large data frames: `rm(large_df)`\n",
                      "• Remove functions: `rm(function_name)`\n",
                      "• Clear workspace: `rm(list = ls())`\n",
                      "• Restart R session: `Session > Restart R`\n\n",
                      "After cleaning up, try your request again!"
                    ),
                    streaming = FALSE
                  )
                  ws$send(jsonlite::toJSON(tips_message, auto_unbox = TRUE))
                  list(action = "ai_response", message = "Environment size reduction tips sent")
                }
              }, error = function(retry_e) {
                # Second retry also failed, send tips
                # Silenced debug: Retry also failed
                tips_message <- list(
                  action = "ai_response", 
                  message = paste(
                    "❌ Request timed out. Your environment has too much data.\n\n",
                    "💡 **Quick fixes:**\n",
                    "• `rm(list = ls())` - Clear all variables\n",
                    "• `Session > Restart R` - Fresh start\n",
                    "• Remove large objects: `rm(large_dataframe)`\n\n",
                    "Try again after cleaning up your environment!"
                  ),
                  streaming = FALSE
                )
                ws$send(jsonlite::toJSON(tips_message, auto_unbox = TRUE))
                list(action = "ai_response", message = "Environment cleanup tips sent")
              })
            } else {
              # Non-timeout error
              list(action = "ai_response", message = paste("Error connecting to AI service:", e$message))
            }
          })
        },
        "initialize_session" = {
          # Initialize RAG session with initial context
          cat("Initializing RAG session...\n")
          
          # Generate session ID if not provided
          session_id <- if (!is.null(request$session_id)) request$session_id else paste0("session_", as.numeric(Sys.time()))
          
          # Capture initial context
          initial_context <- tryCatch({
            capture_context()
          }, error = function(e) {
            cat("ERROR in initial context capture:", e$message, "\n")
            list(
              workspace_objects = list(),
              active_file = NULL,
              file_contents = NULL
            )
          })
          
          tryCatch({
            response <- httr::POST(
              "https://rgent.onrender.com/api/initialize-session",
              body = list(
                access_code = get_current_access_code(),
                session_id = session_id,
                initial_context = initial_context
              ),
              encode = "json",
              httr::timeout(30)
            )
            
            if (httr::status_code(response) == 200) {
              result <- httr::content(response)
              # Store session ID for future use
              .GlobalEnv$current_session_id <- session_id
              .GlobalEnv$last_context_state <- initial_context
              
              list(action = "session_initialized", 
                   session_id = session_id,
                   message = "Session initialized successfully")
            } else {
              list(action = "session_error", 
                   message = paste("Failed to initialize session. Status:", httr::status_code(response)))
            }
          }, error = function(e) {
            list(action = "session_error", message = paste("Error initializing session:", e$message))
          })
        },
        "detect_changes" = {
          # Detect changes since last context capture
          cat("Detecting context changes...\n")
          
          # Use tryCatch to prevent context capture failures during code execution
          current_context <- tryCatch({
            capture_context()
          }, error = function(e) {
            cat("ERROR in context capture during detect_changes:", e$message, "\n")
            # Return a minimal context to prevent complete failure
            list(
              workspace_objects = list(),
              active_file = NULL,
              file_contents = NULL
            )
          })
          
          last_context <- .GlobalEnv$last_context_state
          
          if (is.null(last_context)) {
            # No previous context, treat as initial
            changes <- list(
              type = "initial",
              file_changes = list(),
              workspace_changes = list()
            )
          } else {
            # Detect changes
            changes <- detect_context_changes(last_context, current_context)
          }
          
          # Update stored context
          .GlobalEnv$last_context_state <- current_context
          
          list(action = "changes_detected", changes = changes)
        },
        "store_changes" = {
          # Store detected changes in backend
          cat("Storing context changes...\n")
          
          session_id <- .GlobalEnv$current_session_id
          if (is.null(session_id)) {
            list(action = "changes_error", message = "No active session")
          } else {
            tryCatch({
              response <- httr::POST(
                "https://rgent.onrender.com/api/store-changes",
                body = list(
                  access_code = get_current_access_code(),
                  session_id = session_id,
                  changes = request$changes
                ),
                encode = "json",
                httr::timeout(30)
              )
              
              if (httr::status_code(response) == 200) {
                list(action = "changes_stored", message = "Changes stored successfully")
              } else {
                list(action = "changes_error", 
                     message = paste("Failed to store changes. Status:", httr::status_code(response)))
              }
            }, error = function(e) {
              list(action = "changes_error", message = paste("Error storing changes:", e$message))
            })
          }
        },
        "semantic_search" = {
          # Perform semantic search for relevant context
          cat("Performing semantic search...\n")
          
          session_id <- .GlobalEnv$current_session_id
          if (is.null(session_id)) {
            list(action = "search_error", message = "No active session")
          } else {
            tryCatch({
              response <- httr::POST(
                "https://rgent.onrender.com/api/semantic-search",
                body = list(
                  access_code = "DEMO123",
                  session_id = session_id,
                  query = request$query,
                  limit = if (!is.null(request$limit)) request$limit else 10
                ),
                encode = "json",
                httr::timeout(30)
              )
              
              if (httr::status_code(response) == 200) {
                result <- httr::content(response)
                list(action = "search_results", 
                     results = result$results,
                     count = result$count)
              } else {
                list(action = "search_error", 
                     message = paste("Search failed. Status:", httr::status_code(response)))
              }
            }, error = function(e) {
              list(action = "search_error", message = paste("Error performing search:", e$message))
            })
          }
        },
        "chat_with_rag" = {
          # Chat with AI using RAG for context retrieval
          cat("Chatting with RAG...\n")
          
          session_id <- .GlobalEnv$current_session_id
          if (is.null(session_id)) {
            list(action = "rag_error", message = "No active session")
          } else {
            # Detect changes first with error handling
            current_context <- tryCatch({
              capture_context()
            }, error = function(e) {
              cat("ERROR in context capture during RAG:", e$message, "\n")
              # Return a minimal context to prevent complete failure
              list(
                workspace_objects = list(),
                active_file = NULL,
                file_contents = NULL
              )
            })
            
            last_context <- .GlobalEnv$last_context_state
            changes <- if (!is.null(last_context)) {
              tryCatch({
                detect_context_changes(last_context, current_context)
              }, error = function(e) {
                cat("ERROR in detect_context_changes:", e$message, "\n")
                list()  # Return empty changes list
              })
            } else {
              list()
            }
            
            # Update stored context
            .GlobalEnv$last_context_state <- current_context
            
            tryCatch({
              response <- httr::POST(
                "https://rgent.onrender.com/chat",
                body = list(
                  access_code = get_current_access_code(),
                  prompt = request$message,
                  context_data = current_context
                ),
                encode = "json",
                httr::timeout(30)
              )
              
              if (httr::status_code(response) == 200) {
                result <- httr::content(response)
                list(action = "ai_response", 
                     message = result$response)
              } else {
                list(action = "ai_error", 
                     message = paste("Chat failed. Status:", httr::status_code(response)))
              }
            }, error = function(e) {
              list(action = "ai_error", message = paste("Error in chat:", e$message))
            })
          }
        },
        "get_last_error" = {
          # Check if access code is set
          if (!exists("current_access_code", envir = .GlobalEnv)) {
            list(action = "error", message = "Please validate your access code first using the 'Validate Access' button.")
          } else {
            # Capture last error with context and code
            error_details <- capture_last_error()
          
          # Safely check if there's an error
          has_error <- FALSE
          if (!is.null(error_details) && !is.null(error_details$has_error)) {
            has_error <- as.logical(error_details$has_error)
          }
          
          if (!has_error) {
            # No error found
            list(action = "get_last_error", message = "No errors found in the console. Everything looks good!")
          } else {
            # Send a message to the AI chat that debugging is starting
            debug_start_message <- list(
              action = "chat_with_ai",
              message = "🔍 Debugging error..."
            )
            ws$send(jsonlite::toJSON(debug_start_message, auto_unbox = TRUE))
            
            # Send debug info first
             debug_info <- list(
               action = "debug_info",
               error_message = error_details$error_message,
               error_code = error_details$error_code,  # Add the code that caused the error
               console_context = error_details$console_context,
               relevant_context = error_details$relevant_context,
               timestamp = error_details$timestamp,
               working_directory = error_details$working_directory,
               r_version = error_details$r_version
             )
             
            ws$send(jsonlite::toJSON(debug_info, auto_unbox = TRUE))
            
            # Now stream the AI analysis
            tryCatch({
              
              # Prepare the AI prompt
              prompt <- paste(
                "Debug this R error:\n\n",
                "ERROR: ", error_details$error_message, "\n\n",
                "CODE THAT CAUSED IT:\n", error_details$error_code, "\n\n",
                "RELEVANT CONTEXT:\n", 
                "Available objects: ", paste(names(error_details$relevant_context$objects), collapse = ", "), "\n",
                "Available functions: ", paste(names(error_details$relevant_context$functions), collapse = ", "), "\n",
                "Available data frames: ", paste(names(error_details$relevant_context$data_frames), collapse = ", "), "\n\n",
                "Please provide:\n",
                "1. What went wrong\n",
                "2. How to fix it\n",
                "3. Corrected code"
              )
              
              # Prepare request body
              request_body <- list(
                access_code = get_current_access_code(),
                prompt = prompt,
                context_data = error_details$relevant_context
              )
              
              # Make streaming request to backend
              response <- httr::POST(
                "https://rgent.onrender.com/chat/stream",
                body = request_body,
                encode = "json",
                httr::timeout(30)
              )
              
              if (httr::status_code(response) == 200) {
                # Process streaming response
                response_text <- httr::content(response, "text", encoding = "UTF-8")
                lines <- strsplit(response_text, "\n")[[1]]
                
                current_chunk_buffer <- ""
                
                                 for (line in lines) {
                   if (nchar(line) > 0 && grepl("^data: ", line)) {
                     json_data <- substr(line, 7, nchar(line))
                     
                     # Validate JSON data before parsing
                     if (nchar(json_data) == 0 || json_data == "[DONE]") {
                       next
                     }
                     
                     tryCatch({
                       chunk_data <- jsonlite::fromJSON(json_data)
                       
                       # Handle different response formats
                       if (!is.null(chunk_data$action) && chunk_data$action == "ai_response" && chunk_data$streaming) {
                         # Standard format
                         chunk_text <- chunk_data$chunk
                       } else if (!is.null(chunk_data$chunk) && !is.null(chunk_data$done)) {
                         # Alternative format: {"chunk": "text", "done": false}
                         chunk_text <- chunk_data$chunk
                         is_done <- chunk_data$done
                       } else {
                         # Unknown format, skip
                         cat("Unknown chunk format:", json_data, "\n")
                         next
                       }
                       
                                                if (!is.null(chunk_text) && nchar(chunk_text) > 0) {
                           # Add to buffer
                           current_chunk_buffer <- paste0(current_chunk_buffer, chunk_text)
                           
                           # Check if we're in the middle of a code block
                           code_block_open <- grepl("```[^`]*$", current_chunk_buffer)
                           code_block_complete <- grepl("```.*```", current_chunk_buffer)
                           
                           # Also check if we have an incomplete function call
                           incomplete_function <- grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\([^)]*$", current_chunk_buffer)
                           
                           # Send chunk to frontend when we have a complete word or sentence
                           if (!code_block_open && !incomplete_function && (grepl("[.!?]\\s*$", current_chunk_buffer) || 
                               grepl("\\n\\n", current_chunk_buffer) ||
                               grepl("```$", current_chunk_buffer) ||
                               nchar(current_chunk_buffer) > 100)) {
                             
                             chunk_response <- list(
                               action = "debug_ai_response",
                               streaming = TRUE,
                               chunk = current_chunk_buffer
                             )
                             ws$send(jsonlite::toJSON(chunk_response, auto_unbox = TRUE))
                             
                             # Reset buffer
                             current_chunk_buffer <- ""
                             
                             # Small delay for streaming effect
                             Sys.sleep(0.1)
                           }
                         }
                         
                         # Check if this is the final chunk
                         if ((!is.null(chunk_data$action) && chunk_data$action == "ai_response" && !chunk_data$streaming) ||
                             (!is.null(is_done) && is_done == TRUE)) {
                         # Final chunk
                         if (nchar(current_chunk_buffer) > 0) {
                           final_chunk_response <- list(
                             action = "debug_ai_response",
                             streaming = TRUE,
                             chunk = current_chunk_buffer
                           )
                           ws$send(jsonlite::toJSON(final_chunk_response, auto_unbox = TRUE))
                         }
                         
                         # Send completion signal
                         completion_response <- list(
                           action = "debug_ai_response",
                           streaming = FALSE
                         )
                         ws$send(jsonlite::toJSON(completion_response, auto_unbox = TRUE))
                       }
                     }, error = function(e) {
                       cat("Error parsing chunk:", e$message, "\n")
                       cat("Problematic JSON data:", json_data, "\n")
                       
                       # Skip this chunk and continue with the next one
                       # Don't use 'next' as it's not in a loop context
                     })
                  }
                }
                             } else {
                 # Error response
                 error_content <- httr::content(response, "text", encoding = "UTF-8")
                 # Silenced verbose debug: Full error response
                 
                 error_response <- list(
                   action = "debug_ai_response",
                   streaming = FALSE,
                   error = paste("AI analysis failed:", httr::status_code(response))
                 )
                 ws$send(jsonlite::toJSON(error_response, auto_unbox = TRUE))
               }
                          }, error = function(e) {
               # Silenced debug: Exception details
               # Silenced debug: Exception class
               
               # Send a simple error message if streaming fails
               error_response <- list(
                 action = "debug_ai_response",
                 streaming = FALSE,
                 error = paste("Error in AI analysis:", e$message)
               )
               ws$send(jsonlite::toJSON(error_response, auto_unbox = TRUE))
             })
            
            # Return success status
            list(action = "get_last_error", status = "streaming_started")
          }
        }
        },
        "new_conversation" = {
          # Clear conversation history
          
          # Clear conversation history
          .GlobalEnv$conversation_history <- list()
          
          list(action = "new_conversation", status = "success", message = "Conversation history cleared. Workspace context refreshed.")
        },
        "apply_debug_fix" = {
          # Apply the proposed fix to the code
          tryCatch({
            if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
              # Get the current document
              doc_context <- rstudioapi::getActiveDocumentContext()
              if (!is.null(doc_context)) {
                # Get the line number and new code
                line_number <- request$line_number
                new_code <- request$new_code
                
                # Create the range for the line to replace
                start_pos <- rstudioapi::document_position(line_number, 1)
                end_pos <- rstudioapi::document_position(line_number, nchar(doc_context$contents[line_number]) + 1)
                range <- rstudioapi::document_range(start_pos, end_pos)
                
                # Replace the line with the new code
                rstudioapi::modifyRange(range, new_code)
                
                list(action = "apply_debug_fix", status = "success", message = "Fix applied successfully")
              } else {
                list(action = "apply_debug_fix", status = "error", message = "No active document")
              }
            } else {
              list(action = "apply_debug_fix", status = "error", message = "RStudio API not available")
            }
          }, error = function(e) {
            list(action = "apply_debug_fix", status = "error", message = paste("Error applying fix:", e$message))
          })
        },
        "toggle_auto_fix" = {
          # Toggle auto-fix mode
          tryCatch({
            list(action = "toggle_auto_fix", status = "error", message = "auto_fix_mode is deprecated")
          }, error = function(e) {
            list(action = "toggle_auto_fix", status = "error", message = e$message)
          })
        },
        "analyze_last_plot" = {
          # Analyze the last plot command and provide insights
          
          
          tryCatch({
            # Step 1: Find and analyze the last plot
            analysis_result <- analyze_last_plot()
            
            if (!analysis_result$success) {
              # No plot found or analysis failed
              list(action = "plot_analysis", 
                   success = FALSE, 
                   message = analysis_result$message)
            } else {
              # Send step 1 progress
              progress_msg <- list(
                action = "chat_with_ai",
                message = "Step 2: Analyzing plot structure and data..."
              )
              ws$send(jsonlite::toJSON(progress_msg, auto_unbox = TRUE))
              
              # Step 2: Send step 2 progress
              progress_msg <- list(
                action = "chat_with_ai",
                message = "Step 3: Running statistical analysis..."
              )
              ws$send(jsonlite::toJSON(progress_msg, auto_unbox = TRUE))
              
              # Step 3: Send step 3 progress
              progress_msg <- list(
                action = "chat_with_ai",
                message = "Step 4: Generating insights and recommendations..."
              )
              ws$send(jsonlite::toJSON(progress_msg, auto_unbox = TRUE))
              
              # Get analysis data
              analysis <- analysis_result$analysis
              
              # Create AI prompt for plot analysis
              prompt <- paste(
                "Analyze this plot and provide insights:\n\n",
                "PLOT COMMAND: ", analysis$plot_command, "\n",
                "PLOT TYPE: ", analysis$plot_type, "\n",
                "DATA VARIABLES: ", if(is.list(analysis$data_variables)) {
                  paste(names(analysis$data_variables), "=", unlist(analysis$data_variables), collapse = ", ")
                } else {
                  analysis$data_variables
                }, "\n\n",
                "STATISTICAL ANALYSIS RESULTS:\n",
                paste(names(analysis$analysis_results), ":", 
                      sapply(analysis$analysis_results, function(x) paste(x, collapse = "\n")), 
                      collapse = "\n\n"),
                "\n\nPlease provide:\n",
                "1. What the plot shows (distribution, trends, patterns)\n",
                "2. Key statistical insights\n",
                "3. Any outliers or unusual patterns\n",
                "4. Suggestions for improvement\n",
                "5. Code examples for better visualizations"
              )
              
              # Send to AI for interpretation using streaming endpoint
              tryCatch({
                # Prepare request body for streaming
                request_body <- list(
                  access_code = get_current_access_code(),
                  prompt = prompt,
                  context_data = list(
                    plot_analysis = analysis,
                    workspace_objects = tryCatch({
                      capture_context()$workspace_objects
                    }, error = function(e) {
                      cat("ERROR in plot analysis context capture:", e$message, "\n")
                      list()
                    })
                  )
                )
                
                
                response <- httr::POST(
                  "https://rgent.onrender.com/chat/stream",
                  body = request_body,
                  encode = "json",
                  httr::timeout(60),  # 60 second timeout for plot analysis
                  httr::add_headers("Accept" = "text/event-stream")
                )
                
                
                if (httr::status_code(response) == 200) {
                  # Handle streaming response
                  response_text <- httr::content(response, "text")
                  
                  # Parse Server-Sent Events (SSE) format
                  lines <- strsplit(response_text, "\n")[[1]]
                  full_response <- ""
                  
                  for (line in lines) {
                    if (grepl("^data: ", line)) {
                      # Extract JSON data from SSE format
                      json_data <- substring(line, 7)  # Remove "data: " prefix
                      if (json_data != "[DONE]") {
                        tryCatch({
                          chunk_data <- jsonlite::fromJSON(json_data)
                          
                          if (!is.null(chunk_data$chunk)) {
                            full_response <- paste0(full_response, chunk_data$chunk)
                            
                            # Accumulate chunks and send in larger batches
                            if (!exists("current_chunk_buffer")) {
                              current_chunk_buffer <- ""
                            }
                            current_chunk_buffer <- paste0(current_chunk_buffer, chunk_data$chunk)
                            
                            # Check if we're in the middle of a code block
                            code_block_open <- grepl("```[^`]*$", current_chunk_buffer)
                            code_block_complete <- grepl("```.*```", current_chunk_buffer)
                            
                            # Also check if we have an incomplete function call (starts with function name but no closing)
                            incomplete_function <- grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\([^)]*$", current_chunk_buffer)
                            
                            # Send chunk to frontend when we have a complete word or sentence
                            # Also send when we have a complete code block
                            # Don't send if we're in the middle of a code block or incomplete function
                            if (!code_block_open && !incomplete_function && (grepl("[.!?]\\s*$", current_chunk_buffer) || 
                                grepl("\\n\\n", current_chunk_buffer) ||
                                grepl("```$", current_chunk_buffer) ||
                                nchar(current_chunk_buffer) > 100)) {
                              
                              chunk_response <- list(
                                action = "ai_response",
                                streaming = TRUE,
                                chunk = current_chunk_buffer
                              )
                              ws$send(jsonlite::toJSON(chunk_response, auto_unbox = TRUE))
                              
                              # Reset buffer
                              current_chunk_buffer <- ""
                              
                            }
                          }
                          
                          # Check if this is the final chunk
                                                      if (!is.null(chunk_data$done) && chunk_data$done) {
                              
                              # Send any remaining buffer
                            if (exists("current_chunk_buffer") && nchar(current_chunk_buffer) > 0) {
                              chunk_response <- list(
                                action = "ai_response",
                                streaming = TRUE,
                                chunk = current_chunk_buffer
                              )
                              ws$send(jsonlite::toJSON(chunk_response, auto_unbox = TRUE))
                            }
                            
                            break
                          }
                        }, error = function(e) {
                          cat("Error parsing plot analysis chunk:", e$message, "\n")
                        })
                      }
                    }
                  }
                  
                  # Send finish signal
                  finish_response <- list(
                    action = "ai_response",
                    streaming = TRUE,
                    finished = TRUE
                  )
                  ws$send(jsonlite::toJSON(finish_response, auto_unbox = TRUE))
                  
                  list(action = "plot_analysis_streaming_complete")
                  
                } else {
                  cat("Backend returned error status:", httr::status_code(response), "\n")
                  error_content <- httr::content(response)
                  cat("Error content:", toString(error_content), "\n")
                  list(action = "plot_analysis", 
                       success = FALSE,
                       message = paste("AI analysis failed. Status:", httr::status_code(response)))
                }
              }, error = function(e) {
                cat("Exception during plot analysis backend call:", e$message, "\n")
                # Silenced debug: Exception details
                list(action = "plot_analysis", 
                     success = FALSE,
                     message = paste("Plot analysis failed:", e$message))
              })
            }
          }, error = function(e) {
            list(action = "plot_analysis", 
                 success = FALSE,
                 message = paste("Plot analysis failed:", e$message))
          })
        },
        "start_visualization_agent" = {
          # Start the visualization agent workflow
          result <- start_visualization_agent(request$dataframe, request$options)
          list(action = "visualization_agent_started", data = result)
        },
        "get_next_visualization_step" = {
          # Get the next visualization step
          step_result <- get_next_visualization_step(request$step, request$dataframe, request$variables, request$options)
          
          # Send back the step results
          list(action = "visualization_step_result", data = step_result)
        },
        "execute_visualization_step" = {
          # Generate code for the visualization step (like other agents)
          step_code <- generate_visualization_step_code(request$step, request$dataframe, request$variables, request$options)
          
          # Send back the code to execute through execution engine
          list(action = "execute_code_response", code = step_code)
        },
        "analyze_visualization_plot" = {
          # Convert data_variables to the format expected by generate_analysis_commands
          data_var <- if (is.list(request$data_variables)) {
            # Extract dataframe and variable names
            dataframe <- request$data_variables$dataframe
            x_var <- request$data_variables$x
            y_var <- request$data_variables$y
            
            # Create appropriate data variable string based on plot type
            if (request$plot_type %in% c("histogram", "density", "bar")) {
              result <- paste0(dataframe, "$", x_var)
            } else if (request$plot_type %in% c("scatter", "boxplot", "line", "line_plot")) {
              result <- list(x = paste0(dataframe, "$", x_var), y = paste0(dataframe, "$", y_var))
            } else {
              result <- paste0(dataframe, "$", x_var)
            }
            
            result
          } else {
            request$data_variables
          }
          
          # Analyze the visualization plot
          analysis_result <- analyze_visualization_plot(
            request$plot_command, 
            request$plot_type, 
            data_var
          )
          
          if (!analysis_result$success) {
            list(action = "visualization_plot_analysis", 
                 success = FALSE, 
                 message = analysis_result$message)
          } else {
            # Send progress messages
            progress_msg <- list(
              action = "chat_with_ai",
              message = "Step 2: Analyzing plot structure and data..."
            )
            ws$send(jsonlite::toJSON(progress_msg, auto_unbox = TRUE))
            
            progress_msg <- list(
              action = "chat_with_ai",
              message = "Step 3: Running statistical analysis..."
            )
            ws$send(jsonlite::toJSON(progress_msg, auto_unbox = TRUE))
            
            progress_msg <- list(
              action = "chat_with_ai",
              message = "Step 4: Generating insights and recommendations..."
            )
            ws$send(jsonlite::toJSON(progress_msg, auto_unbox = TRUE))
            
            # Get analysis data
            analysis <- analysis_result$analysis
            
            # Create AI prompt for plot analysis
            prompt <- paste(
              "Analyze this visualization plot and provide insights:\n\n",
              "PLOT COMMAND: ", analysis$plot_command, "\n",
              "PLOT TYPE: ", analysis$plot_type, "\n",
              "DATA VARIABLES: ", if(is.list(analysis$data_variables)) {
                paste(names(analysis$data_variables), "=", unlist(analysis$data_variables), collapse = ", ")
              } else {
                analysis$data_variables
              }, "\n\n",
              "STATISTICAL ANALYSIS RESULTS:\n",
              paste(names(analysis$analysis_results), ":", 
                    sapply(analysis$analysis_results, function(x) paste(x, collapse = "\n")), 
                    collapse = "\n\n"),
              "\n\nPlease provide:\n",
              "1. What the plot shows (distribution, trends, patterns)\n",
              "2. Key statistical insights\n",
              "3. Any outliers or unusual patterns\n",
              "4. Suggestions for improvement\n",
              "5. Code examples for better visualizations"
            )
            
            # Send to AI for interpretation using streaming endpoint
            tryCatch({
              # Prepare request body for streaming
              request_body <- list(
                access_code = get_current_access_code(),
                prompt = prompt,
                context_data = list(
                  plot_analysis = analysis,
                  workspace_objects = tryCatch({
                    capture_context()$workspace_objects
                  }, error = function(e) {
                    cat("ERROR in plot analysis context capture:", e$message, "\n")
                    list()
                  })
                )
              )
              
              response <- httr::POST(
                "https://rgent.onrender.com/chat/stream",
                body = request_body,
                encode = "json",
                httr::timeout(60),  # 60 second timeout for plot analysis
                httr::add_headers("Accept" = "text/event-stream")
              )
              
              if (httr::status_code(response) == 200) {
                # Handle streaming response
                response_text <- httr::content(response, "text")
                
                # Parse Server-Sent Events (SSE) format
                lines <- strsplit(response_text, "\n")[[1]]
                full_response <- ""
                
                for (line in lines) {
                  if (grepl("^data: ", line)) {
                    # Extract JSON data from SSE format
                    json_data <- substring(line, 7)  # Remove "data: " prefix
                    if (json_data != "[DONE]") {
                      tryCatch({
                        chunk_data <- jsonlite::fromJSON(json_data)
                        if (!is.null(chunk_data$chunk)) {
                          # Send chunk to frontend
                          chunk_msg <- list(
                            action = "ai_response",
                            streaming = TRUE,
                            chunk = chunk_data$chunk
                          )
                          ws$send(jsonlite::toJSON(chunk_msg, auto_unbox = TRUE))
                          full_response <- paste0(full_response, chunk_data$chunk)
                        }
                      }, error = function(e) {
                        cat("Error parsing chunk:", e$message, "\n")
                      })
                    }
                  }
                }
                
                # Send completion message
                completion_msg <- list(
                  action = "plot_analysis_streaming_complete"
                )
                ws$send(jsonlite::toJSON(completion_msg, auto_unbox = TRUE))
                
                list(action = "visualization_plot_analysis", 
                     success = TRUE, 
                     message = "Plot analysis completed")
              } else {
                list(action = "visualization_plot_analysis", 
                     success = FALSE, 
                     message = paste("AI analysis failed with status:", httr::status_code(response)))
              }
            }, error = function(e) {
              list(action = "visualization_plot_analysis", 
                   success = FALSE, 
                   message = paste("AI analysis error:", e$message))
            })
          }
        },
        "stop_agent" = {
          list(action = "stop_response", status = "success", message = "Agent workflow stopped")
        },
        list(action = "error", message = "Unknown action")
      )
      
      # Send response back to JavaScript
      tryCatch({
        # Ensure code content is properly escaped for JSON
        if (response$action == "agent_step" && !is.null(response$data$code)) {
          # Don't let jsonlite HTML-encode the R code
          response$data$code <- response$data$code
        }
        
        # Special handling for dataframes action to ensure data serializes as JSON array
        if (response$action == "dataframes") {
          # Ensure data is always a proper array structure
          if (is.null(response$data)) {
            response$data <- list()
          } else {
            # Convert character vector to list to ensure it serializes as array (not string)
            # This prevents auto_unbox from converting single-element vectors to strings
            # Lists are never unboxed by auto_unbox, only atomic vectors with length 1 are
            if (is.character(response$data)) {
              response$data <- as.list(response$data)
            } else if (!is.list(response$data)) {
              response$data <- as.list(as.character(response$data))
            }
          }
        }
        
        # Use auto_unbox = TRUE for all responses (dataframes data is now a list, so it won't be unboxed)
        response_json <- jsonlite::toJSON(response, auto_unbox = TRUE, escape_double = FALSE)
        # Check if response is too large (WebSocket has size limits)
        if (nchar(response_json) > 100000) {  # 100KB limit
          cat("WARNING: Response too large (", nchar(response_json), " chars), truncating...\n")
          # For large responses, send a simplified version
          if (response$action == "agent_step" && !is.null(response$data$code)) {
            response$data$code <- paste0("# Code generated successfully (", nchar(response$data$code), " characters)\n",
                                       "# Use the 'Insert at Cursor' button to get the full code\n",
                                       "# Summary: ", substr(response$data$code, 1, 500), "...")
            response_json <- jsonlite::toJSON(response, auto_unbox = TRUE, escape_double = FALSE)
          }
        }
        ws$send(response_json)
      }, error = function(e) {
        cat("ERROR sending response to frontend:", e$message, "\n")
        cat("Error details:", tryCatch(toString(e), error = function(e2) "Error converting error to string"), "\n")
        # Try to send a simple error response
        tryCatch({
          error_response <- list(action = "error", message = "Error processing request")
          ws$send(jsonlite::toJSON(error_response, auto_unbox = TRUE))
        }, error = function(e2) {
          cat("ERROR sending error response:", e2$message, "\n")
        })
      })
      
    }, error = function(e) {
      # Global error handler - print traceback for any unhandled error
      cat("GLOBAL ERROR HANDLER: Error in message handler:", e$message, "\n")
      cat("GLOBAL ERROR HANDLER: Error class:", class(e), "\n")
      cat("GLOBAL ERROR HANDLER: Error type:", typeof(e), "\n")
      cat("GLOBAL ERROR HANDLER: Full error object:", toString(e), "\n")
      cat("GLOBAL ERROR HANDLER: Traceback:\n")
      traceback(3)
      
      # Send error response
      cat("Error in message handler:", e$message, "\n")
      cat("Error details:", tryCatch(toString(e), error = function(e2) "Error converting error to string"), "\n")
      error_response <- list(action = "error", message = e$message)
      tryCatch({
        error_json <- jsonlite::toJSON(error_response, auto_unbox = TRUE)
        ws$send(error_json)
      }, error = function(e2) {
        cat("Failed to send error response:", e2$message, "\n")
        cat("Error details:", tryCatch(toString(e2), error = function(e3) "Error converting error to string"), "\n")
      })
    })
  }
  
  # Try to start server on port 8888
  tryCatch({
    .GlobalEnv$websocket_server <- httpuv::startServer(
      "127.0.0.1", 
      8888,
      list(
        onWSOpen = function(ws) {
          # Send theme information immediately upon connection
          tryCatch({
            theme_info <- get_rstudio_theme()
            theme_message <- list(
              action = "theme_info",
              is_dark = theme_info$is_dark,
              theme_name = theme_info$theme_name,
              editor_theme = theme_info$editor_theme,
              global_theme = theme_info$global_theme,
              colors = theme_info$colors
            )
            ws$send(jsonlite::toJSON(theme_message, auto_unbox = TRUE))
          }, error = function(e) {
            # ignore theme send errors
          })
          
          # Check if access code exists and notify frontend
          tryCatch({
            if (exists("current_access_code", envir = .GlobalEnv) && 
                !is.null(.GlobalEnv$current_access_code) && 
                nchar(trimws(.GlobalEnv$current_access_code)) > 0) {
              # Send access code to frontend so it can auto-validate and hide the access section
              access_code_message <- list(
                action = "access_code_loaded",
                access_code = .GlobalEnv$current_access_code,
                auto_validate = TRUE
              )
              ws$send(jsonlite::toJSON(access_code_message, auto_unbox = TRUE))
            }
          }, error = function(e) {
            # Silently handle errors
          })
          
          # Set up message handler
          ws$onMessage(function(isBinary, data) {
            message_handler(ws, isBinary, data)
          })
        },
        onWSClose = function(ws) {
          # no-op
        }
      )
    )
    
      }, error = function(e) {
      # Try alternative port silently
      tryCatch({
        .GlobalEnv$websocket_server <- httpuv::startServer(
          "127.0.0.1", 
          8889,
          list(
            onWSOpen = function(ws) {
              tryCatch({
                theme_info <- get_rstudio_theme()
                theme_message <- list(
                  action = "theme_info",
                  is_dark = theme_info$is_dark,
                  theme_name = theme_info$theme_name
                )
                ws$send(jsonlite::toJSON(theme_message, auto_unbox = TRUE))
                
                # Check if access code exists and notify frontend
                if (exists("current_access_code", envir = .GlobalEnv) && 
                    !is.null(.GlobalEnv$current_access_code) && 
                    nchar(trimws(.GlobalEnv$current_access_code)) > 0) {
                  access_code_message <- list(
                    action = "access_code_loaded",
                    access_code = .GlobalEnv$current_access_code,
                    auto_validate = TRUE
                  )
                  ws$send(jsonlite::toJSON(access_code_message, auto_unbox = TRUE))
                }
              }, error = function(e) {
                # ignore
              })
              
              # Set up message handler
              ws$onMessage(function(isBinary, data) {
                message_handler(ws, isBinary, data)
              })
            },
            onWSClose = function(ws) {
              # no-op
            }
          )
        )
      }, error = function(e2) {
        stop("Failed to start WebSocket server on both ports")
      })
    })
}

#' Launch HTML interface with HTTPS support for clipboard API
launch_html_interface <- function() {
  # Try multiple paths to find the HTML file
  possible_paths <- c(
    system.file("public/websocket_chat.html", package = "rstudioai"),
    file.path(getwd(), "inst/public/websocket_chat.html"),
    file.path(dirname(getwd()), "clean_package/inst/public/websocket_chat.html"),
    file.path(getwd(), "clean_package/inst/public/websocket_chat.html")
  )
  
  html_file <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      html_file <- path
      break
    }
  }
  
  if (is.null(html_file)) {
    cat("HTML file not found. Tried paths:\n")
    for (path in possible_paths) {
      cat("  -", path, "\n")
    }
    cat("Current working directory:", getwd(), "\n")
    return(invisible(FALSE))
  }
  
  # Open directly in the RStudio viewer pane (localhost file)
  temp_file <- tempfile(fileext = ".html")
  file.copy(html_file, temp_file)
  rstudioapi::viewer(temp_file)
}


#' Add message to conversation history
#' @param role The role of the message sender ("user" or "ai")
#' @param message The message content
#' @export
add_to_conversation_history <- function(role, message) {
  # Get current conversation history
  if (!exists("conversation_history")) {
    .GlobalEnv$conversation_history <- list()
  }
  
  # Add new message
  .GlobalEnv$conversation_history <- c(.GlobalEnv$conversation_history, list(
    list(
      role = role,
      message = message,
      timestamp = Sys.time()
    )
  ))
  
  # Keep only last 6 messages to avoid context bloat
  if (length(.GlobalEnv$conversation_history) > 6) {
    .GlobalEnv$conversation_history <- .GlobalEnv$conversation_history[(length(.GlobalEnv$conversation_history) - 5):length(.GlobalEnv$conversation_history)]
  }
  
  
}

#' Get conversation history as formatted string
#' @export
get_conversation_history <- function() {
  if (!exists("conversation_history") || length(.GlobalEnv$conversation_history) == 0) {
    return("")
  }
  
  # Format conversation history
  history_text <- "CONVERSATION HISTORY:\n"
  for (i in seq_along(.GlobalEnv$conversation_history)) {
    msg <- .GlobalEnv$conversation_history[[i]]
    role_label <- ifelse(msg$role == "user", "USER", "AI")
    history_text <- paste0(history_text, role_label, ": ", msg$message, "\n\n")
  }
  
  return(history_text)
}

#' Capture current workspace context
#' @export
capture_context <- function() {
  tryCatch({
    # Get workspace objects with detailed information
    workspace_objects <- tryCatch({
      # Use globalenv() instead of .GlobalEnv for more robust access
      global_env <- globalenv()
      lapply(safe_ls(global_env), function(obj_name) {
      tryCatch({
        obj <- safe_get(obj_name, global_env)
        
        # Get detailed object information
        obj_info <- list(
          name = obj_name,
          class = paste(class(obj), collapse = ", "),
          length = length(obj),
          is_function = is.function(obj),
          is_data_frame = is.data.frame(obj),
          is_vector = is.vector(obj),
          is_list = is.list(obj),
          is_matrix = is.matrix(obj),
          is_array = is.array(obj)
        )
        
        # Add object-specific details with intelligent summarization
        
        if (is.data.frame(obj)) {
          obj_info$dimensions <- dim(obj)
          obj_info$column_names <- names(obj)
          obj_info$column_types <- sapply(obj, function(col) paste(class(col), collapse = ", "))
          
          # Only add basic sample data (first 2 rows, first 3 columns)
          if (nrow(obj) > 0) {
            head_data <- head(obj, 2)
            sample_cols <- min(3, ncol(head_data))
            obj_info$sample_data <- lapply(1:sample_cols, function(i) {
              list(
                column = names(head_data)[i],
                sample_values = as.character(head_data[[i]])[1:2]  # Only first 2 values
              )
            })
          } else {
            obj_info$sample_data <- list()
          }
          
        } else if (is.vector(obj) && !is.list(obj)) {
          if (length(obj) <= 5) {
            tryCatch({
              obj_info$values <- as.character(obj)
            }, error = function(e) {
              cat("  - Error converting vector to character:", e$message, "\n")
              obj_info$values <- paste("Error: Could not convert to character")
            })
          } else {
            obj_info$summary <- list(
              total_length = length(obj),
              unique_count = tryCatch(length(unique(obj)), error = function(e) "unknown"),
              na_count = tryCatch(sum(is.na(obj)), error = function(e) "unknown")
            )
          }
          
        } else if (is.list(obj) && !is.data.frame(obj)) {
          if (length(obj) <= 3) {
            tryCatch({
              obj_info$list_structure <- lapply(names(obj), function(name) {
                item <- obj[[name]]
                list(
                  name = name,
                  class = paste(class(item), collapse = ", "),
                  length = length(item)
                )
              })
            }, error = function(e) {
              cat("  - Error processing list structure:", e$message, "\n")
              obj_info$list_structure <- list()
            })
          } else {
            tryCatch({
              obj_info$list_summary <- list(
                total_items = length(obj),
                item_names = names(obj)[1:5]
              )
            }, error = function(e) {
              obj_info$list_summary <- list(total_items = length(obj), item_names = "unknown")
            })
          }
          
        } else if (is.function(obj)) {
          tryCatch({
            obj_info$function_args <- names(formals(obj))
          }, error = function(e) {
            cat("  - Error getting function args:", e$message, "\n")
            obj_info$function_args <- "unknown"
          })
          obj_info$function_source <- if (is.primitive(obj)) "primitive" else "user-defined"
          
        } else if (is.matrix(obj) || is.array(obj)) {
                    obj_info$dimensions <- dim(obj)
                      
          } else if (is.environment(obj)) {
          tryCatch({
            obj_info$env_contents <- ls(obj)
                      }, error = function(e) {
              obj_info$env_contents <- "unknown"
            })
            
          } else {
            obj_info$is_s4 <- isS4(obj)
          }
        
        obj_info
      }, error = function(e) {
        cat("  - Error processing object:", obj_name, ":", e$message, "\n")
        cat("  - Error details:", tryCatch(toString(e), error = function(e2) "Error converting error to string"), "\n")
        list(
          name = obj_name,
          class = "error",
          length = NULL,
          is_function = FALSE,
          is_data_frame = FALSE,
          error = e$message
        )
      })
    })
    }, error = function(e) {
      cat("Error in workspace objects processing:", e$message, "\n")
      cat("Error details:", tryCatch(toString(e), error = function(e2) "Error converting error to string"), "\n")
      list()  # Return empty list if workspace processing fails
    })
    
    # Get environment info
    environment_info <- tryCatch({
      list(
        r_version = as.character(R.version.string),
        platform = as.character(R.version$platform),
        working_directory = as.character(getwd()),
        packages = tryCatch(names(sessionInfo()$otherPkgs), error = function(e) list()),
        loaded_namespaces = tryCatch(loadedNamespaces(), error = function(e) list())
      )
    }, error = function(e) {
      cat("Error in environment info processing:", e$message, "\n")
      list(
        r_version = "unknown",
        platform = "unknown", 
        working_directory = "unknown",
        packages = list(),
        loaded_namespaces = list()
      )
    })
    
    # Get file information if RStudio API is available
    file_info <- list()
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      tryCatch({
        # Get active document context
        doc_context <- rstudioapi::getActiveDocumentContext()
        if (!is.null(doc_context)) {
          # Handle both saved files and untitled documents
          file_path <- doc_context$path
          file_name <- if (file_path != "") basename(file_path) else "Untitled Document"
          
          # Safely get cursor position
          cursor_line <- 1
          cursor_column <- 1
          tryCatch({
            if (length(doc_context$selection) > 0) {
              # Access selection as a document_selection object
              selection <- doc_context$selection[[1]]  # Get first selection
              # Access document_position object correctly
              start_pos <- selection$range$start
              cursor_line <- start_pos[["row"]]
              cursor_column <- start_pos[["column"]]
            }
          }, error = function(e) {
            cursor_line <- 1
            cursor_column <- 1
          })
          
          # Capture full file content
          file_contents <- doc_context$contents
          
          file_info <- list(
            active_file = file_path,
            active_file_name = file_name,
            cursor_line = cursor_line,
            cursor_column = cursor_column,
            total_lines = length(doc_context$contents),
            file_contents = file_contents,
            file_modified = doc_context$modified
          )
        }
      }, error = function(e) {
        file_info <- list(error = paste("Error getting file info:", e$message))
      })
    }
    
    # Return context data with simplified structure to avoid double encoding
    context_data <- list(
      workspace_objects = workspace_objects,
      environment_info = environment_info,
      file_info = file_info,
      timestamp = as.character(Sys.time())
    )
    
    # Convert to JSON and back to flatten nested structures
    tryCatch({
      json_str <- jsonlite::toJSON(context_data, auto_unbox = TRUE)
      context_data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    }, error = function(e) {
      # Fallback silently to original structure
    })
    
    context_data
  }, error = function(e) {
    cat("ERROR in capture_context:", e$message, "\n")
    traceback(2)
    cat("Error details:", tryCatch(toString(e), error = function(e2) "Error converting error to string"), "\n")
    list(
      error = paste("Error capturing context:", e$message),
      timestamp = as.character(Sys.time())
    )
  })
}

#' Stop WebSocket server
#' @export
stop_websocket_server <- function() {
  if (!is.null(.GlobalEnv$websocket_server)) {
    tryCatch({
      httpuv::stopServer(.GlobalEnv$websocket_server)
      .GlobalEnv$websocket_server <- NULL
    }, error = function(e) {
      # silently ignore
    })
  } else {
    # no-op
  }
}

#' Detect changes between two context states
#' @param old_context Previous context state
#' @param new_context Current context state
#' @return List of detected changes
#' @export
detect_context_changes <- function(old_context, new_context) {
  tryCatch({
    changes <- list(
      file_changes = list(),
      workspace_changes = list()
    )
  
  # Detect file changes
  if (!is.null(old_context$file_info) && !is.null(new_context$file_info)) {
    old_file <- old_context$file_info$file_contents
    new_file <- new_context$file_info$file_contents
    
    if (!is.null(old_file) && !is.null(new_file) && is.character(old_file) && is.character(new_file)) {
      old_lines <- strsplit(old_file, "\n")[[1]]
      new_lines <- strsplit(new_file, "\n")[[1]]
      
      # Find added lines
      added_lines <- which(!(new_lines %in% old_lines))
      if (length(added_lines) > 0) {
        changes$file_changes$added_lines <- added_lines
      }
      
      # Find modified lines (simplified - lines that exist in both but content changed)
      modified_lines <- c()
      for (i in 1:min(length(old_lines), length(new_lines))) {
        # Handle missing values safely with comprehensive error handling
        old_line <- tryCatch({
          line_value <- old_lines[i]
          if (is.na(line_value) || is.null(line_value) || length(line_value) == 0 || identical(line_value, "")) {
            ""
          } else {
            as.character(line_value)
          }
        }, error = function(e) "")
        
        new_line <- tryCatch({
          line_value <- new_lines[i]
          if (is.na(line_value) || is.null(line_value) || length(line_value) == 0 || identical(line_value, "")) {
            ""
          } else {
            as.character(line_value)
          }
        }, error = function(e) "")
        
        if (old_line != new_line) {
          modified_lines <- c(modified_lines, i)
        }
      }
      if (length(modified_lines) > 0) {
        changes$file_changes$modified_lines <- modified_lines
      }
      
      # Find deleted lines
      deleted_lines <- which(!(old_lines %in% new_lines))
      if (length(deleted_lines) > 0) {
        changes$file_changes$deleted_lines <- deleted_lines
      }
    }
  }
  
  # Detect workspace changes
  if (!is.null(old_context$workspace_objects) && !is.null(new_context$workspace_objects)) {
    old_objects <- sapply(old_context$workspace_objects, function(obj) obj$name)
    new_objects <- sapply(new_context$workspace_objects, function(obj) obj$name)
    
    # Find added objects
    added_objects <- setdiff(new_objects, old_objects)
    if (length(added_objects) > 0) {
      changes$workspace_changes$added_objects <- added_objects
    }
    
    # Find removed objects
    removed_objects <- setdiff(old_objects, new_objects)
    if (length(removed_objects) > 0) {
      changes$workspace_changes$removed_objects <- removed_objects
    }
    
    # Find modified objects (simplified - objects that exist in both but properties changed)
    common_objects <- intersect(old_objects, new_objects)
    modified_objects <- c()
    for (obj_name in common_objects) {
      old_obj <- old_context$workspace_objects[[which(sapply(old_context$workspace_objects, function(obj) obj$name) == obj_name)]]
      new_obj <- new_context$workspace_objects[[which(sapply(new_context$workspace_objects, function(obj) obj$name) == obj_name)]]
      
             # Check if objects have the required properties and they're not NA
       old_class <- if (!is.null(old_obj$class) && !is.na(old_obj$class)) old_obj$class else "unknown"
       new_class <- if (!is.null(new_obj$class) && !is.na(new_obj$class)) new_obj$class else "unknown"
       old_length <- if (!is.null(old_obj$length) && !is.na(old_obj$length)) old_obj$length else 0
       new_length <- if (!is.null(new_obj$length) && !is.na(new_obj$length)) new_obj$length else 0
       
       if (old_class != new_class || old_length != new_length) {
        modified_objects <- c(modified_objects, obj_name)
      }
    }
    if (length(modified_objects) > 0) {
      changes$workspace_changes$modified_objects <- modified_objects
    }
  }
  
    return(changes)
  }, error = function(e) {
    cat("ERROR in detect_context_changes:", e$message, "\n")
    cat("Error details:", tryCatch(toString(e), error = function(e2) "Error converting error to string"), "\n")
    # Return empty changes if there's an error
    list(
      file_changes = list(),
      workspace_changes = list()
    )
  })
} 

#' Capture last error with context and code
#' @return List containing error information and context
#' @export
capture_last_error <- function() {
  tryCatch({
    # Get the last error message - try multiple sources
    last_error <- ""
    
    # Try .Last.error first
    if (exists(".Last.error") && !is.null(.Last.error)) {
      last_error <- as.character(.Last.error)
    }
    
    # Fallback to geterrmessage
    if (last_error == "") {
      last_error <- geterrmessage()
    }
    
    # Also check if we have a stored error message
    if (last_error == "" && exists("last_error_message") && !is.null(last_error_message)) {
      last_error <- last_error_message
    }
    
    if (last_error == "") {
      return(list(
        has_error = FALSE,
        message = "No error found in console"
      ))
    }
    
    # Get the console lines above the error
    console_context <- get_console_context()
    
    # Check if we have stored code from the frontend
    stored_code <- ""
    if (exists("currentStepCode", envir = .GlobalEnv) && !is.null(.GlobalEnv$currentStepCode)) {
      stored_code <- .GlobalEnv$currentStepCode
    }
    
    # Use stored code if available, otherwise use console context
    error_code <- if (nchar(stored_code) > 0) stored_code else console_context$code
    
    # Get relevant context from index
    relevant_context <- get_relevant_context_for_error(error_code, last_error)
    
    # Get error details
    error_details <- list(
      has_error = TRUE,
      error_message = last_error,
      error_code = error_code,  # Use the stored code or console context
      console_context = console_context,
      relevant_context = relevant_context,
      timestamp = as.character(Sys.time()),
      r_version = as.character(R.version.string),
      working_directory = getwd()
    )
    
    return(error_details)
    
  }, error = function(e) {
    return(list(
      has_error = TRUE,
      error_message = paste("Error capturing error context:", e$message),
      timestamp = as.character(Sys.time())
    ))
  })
} 

# =============================================================================
# ENHANCED DEBUG HELPER FUNCTIONS
# =============================================================================

#' Get the last executed code from R history
#' @export
get_last_executed_code <- function() {
  tryCatch({
    # Try to get from RStudio history
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      # Get the active document context
      doc_context <- rstudioapi::getActiveDocumentContext()
      if (!is.null(doc_context) && length(doc_context$contents) > 0) {
        # Get the last few lines that might contain the error
        lines <- doc_context$contents
        last_lines <- tail(lines, 10)  # Last 10 lines
        return(paste(last_lines, collapse = "\n"))
      }
    }
    
    # Fallback: try to get from R history
    if (exists(".Last.value")) {
      return("Last executed code (from .Last.value)")
    }
    
    return("Could not determine last executed code")
    
  }, error = function(e) {
    return(paste("Error getting last code:", e$message))
  })
}

#' Auto-capture error context and show notification
#' @export
auto_capture_error <- function() {
  tryCatch({
    # Get the last error message from .Last.error
    last_error <- ""
    if (exists(".Last.error") && !is.null(.Last.error)) {
      last_error <- as.character(.Last.error)
    }
    
    # Fallback to geterrmessage if .Last.error is empty
    if (last_error == "") {
      last_error <- geterrmessage()
    }
    
    if (last_error != "") {
      # Capture console context
      console_context <- get_console_context()
      
      # Update workspace index
      update_workspace_index()
      
      # Store error context for later use
      .GlobalEnv$last_error_context <- list(
        error_message = last_error,
        console_context = console_context,
        timestamp = Sys.time()
      )
      
      # Show notification with exact error
      cat("\n🐛 Error detected:", last_error, "\n")
      cat("Click the debug button to fix it!\n\n")
      
          }
  }, error = function(e) {
    cat("Error in auto_capture_error:", e$message, "\n")
  })
}

#' Execute code with auto-capture
#' @export
execute_with_capture <- function(code) {
  tryCatch({
    eval(parse(text = code))
  }, error = function(e) {
    auto_capture_error()
    stop(e$message)  # Re-throw the error
  })
}

#' Toggle auto-fix mode (removed)
#' @export
toggle_auto_fix <- function() {
  warning("auto_fix_mode is deprecated and has been removed; no action taken.")
  FALSE
}

#' Test auto-capture system
#' @export
test_auto_capture <- function() {
  cat("🧪 Testing auto-capture system...\n")
  cat("Current error handler:", toString(options("error")), "\n")
  cat("Auto-fix mode:", get_auto_fix_status(), "\n")
  
  # Test with a simple error
  cat("Triggering test error...\n")
  tryCatch({
    stop("Test error for auto-capture")
  }, error = function(e) {
    cat("Test error caught:", e$message, "\n")
  })
}

#' Manually trigger auto-capture
#' @export
trigger_auto_capture <- function() {
  auto_capture_error()
}

#' Capture error after it occurs
#' @export
capture_last_error <- function() {
  
  # Get the last error message - try multiple sources
  last_error <- ""
  
  # Try .Last.error first
  if (exists(".Last.error") && !is.null(.Last.error)) {
    last_error <- as.character(.Last.error)
  }
  
  # Fallback to geterrmessage
  if (last_error == "") {
    last_error <- geterrmessage()
  }
  
  # Also check if we have a stored error message
  if (last_error == "" && exists("last_error_message") && !is.null(last_error_message)) {
    last_error <- last_error_message
  }
  
  if (last_error == "") {
    return(list(
      has_error = FALSE,
      message = "No error found in console"
    ))
  }
  
  # Get the console lines above the error
  console_context <- get_console_context()
  
  # Get relevant context from index
  relevant_context <- get_relevant_context_for_error(console_context$code, last_error)
  
  # Get error details
  error_details <- list(
    has_error = TRUE,
    error_message = last_error,
    error_code = console_context$code,  # Add the code that caused the error
    console_context = console_context,
    relevant_context = relevant_context,
    timestamp = as.character(Sys.time()),
    r_version = as.character(R.version.string),
    working_directory = getwd()
  )
  
  return(error_details)
}

#' Remove error monitoring task callback
#' @export
remove_error_monitoring <- function() {
  tryCatch({
    removeTaskCallback("error_monitor")
    cat("✅ Error monitoring removed\n")
  }, error = function(e) {
    cat("No error monitoring to remove\n")
  })
}

#' Get the console lines above the error
#' @export
get_console_context <- function() {
  tryCatch({
    # Try to get from R's console history
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      # Try to get console history using savehistory and readLines
      tryCatch({
        # Save current history to a temporary file
        temp_file <- tempfile("history", fileext = ".R")
        
        # Save the history
        savehistory(temp_file)
        
        # Check if file exists and read it
        if (file.exists(temp_file)) {
          history_lines <- readLines(temp_file)
          
          if (length(history_lines) > 0) {
            # Get the last 10 commands from history (more context)
            last_commands <- tail(history_lines, 10)
            console_text <- paste(last_commands, collapse = "\n")
            
            # Clean up temp file
            unlink(temp_file)
            
            return(list(
              code = console_text,
              start_line = 1,
              end_line = length(last_commands),
              current_line = length(last_commands)
            ))
          } else {
            unlink(temp_file)
          }
        }
      }, error = function(e) {
        # Clean up temp file if it exists
        if (exists("temp_file") && file.exists(temp_file)) {
          unlink(temp_file)
        }
      })
      
      # Fallback: Get from active document context
      tryCatch({
        # Get the active document context
        doc_context <- rstudioapi::getActiveDocumentContext()
        
        if (!is.null(doc_context) && "contents" %in% names(doc_context) && length(doc_context$contents) > 0) {
          # Get the last few lines from the document
          lines <- doc_context$contents
          last_lines <- tail(lines, 10)  # Get more lines to search through
          
          # Look for R commands in the document (lines that look like R code)
          r_commands <- c()
          for (line in last_lines) {
            line <- trimws(line)
            # Check if line looks like an R command
            if (nchar(line) > 0 && 
                !grepl("^#", line) &&  # Not a comment
                !grepl("^$", line) &&  # Not empty
                (grepl("<-", line) || grepl("\\(", line) || grepl("\\$", line) || grepl("plot", line) || grepl("mean", line))) {
              r_commands <- c(r_commands, line)
            }
          }
          
          if (length(r_commands) > 0) {
            # Get the last 5 R commands
            last_r_commands <- tail(r_commands, 5)
            console_text <- paste(last_r_commands, collapse = "\n")
            
            return(list(
              code = console_text,
              start_line = 1,
              end_line = length(last_r_commands),
              current_line = length(last_r_commands)
            ))
          }
        }
      }, error = function(e) {
        # Silently handle errors
      })
    }
    
    # Try to get from R's history
    tryCatch({
      # Save current history to a temporary file
      temp_file <- tempfile("history", fileext = ".R")
      savehistory(temp_file)
      
      # Read the last few lines
      if (file.exists(temp_file)) {
        history_lines <- readLines(temp_file)
        last_lines <- tail(history_lines, 5)
        console_text <- paste(last_lines, collapse = "\n")
        
        # Clean up
        unlink(temp_file)
        
        return(list(
          code = console_text,
          start_line = 1,
          end_line = length(last_lines),
          current_line = length(last_lines)
        ))
      }
    }, error = function(e) {
      # Ignore history errors
    })
    
    # Fallback: try to get from R history
    if (exists(".Last.value")) {
      return(list(
        code = "Last executed code (from .Last.value)",
        start_line = 1,
        end_line = 1,
        current_line = 1
      ))
    }
    
    return(list(
      code = "Could not determine last executed code",
      start_line = 1,
      end_line = 1,
      current_line = 1
    ))
    
  }, error = function(e) {
    return(list(
      code = paste("Error getting console context:", e$message),
      start_line = 1,
      end_line = 1,
      current_line = 1
    ))
  })
}



#' Get relevant context from index based on error type
#' @export
get_relevant_context_for_error <- function(error_code, error_message) {
  tryCatch({
    # Initialize relevant context
    relevant_context <- list(
      objects = list(),
      functions = list(),
      data_frames = list(),
      error_type = "unknown"
    )
    
    # Check if workspace index exists
    if (!exists("workspace_index", envir = .GlobalEnv)) {
      return(relevant_context)
    }
    
    workspace_index <- .GlobalEnv$workspace_index
    
    # Determine error type and get relevant context
    if (grepl("object.*not found", error_message, ignore.case = TRUE)) {
      # Object not found error
      relevant_context$error_type <- "object_not_found"
      relevant_context$objects <- workspace_index$objects
      relevant_context$data_frames <- workspace_index$data_frames
      
    } else if (grepl("could not find function", error_message, ignore.case = TRUE)) {
      # Function not found error
      relevant_context$error_type <- "function_not_found"
      relevant_context$functions <- workspace_index$functions
      
    } else if (grepl("data frame", error_message, ignore.case = TRUE) || 
               grepl("column", error_message, ignore.case = TRUE)) {
      # Data frame related error
      relevant_context$error_type <- "data_frame_error"
      relevant_context$data_frames <- workspace_index$data_frames
      
    } else if (grepl("argument", error_message, ignore.case = TRUE)) {
      # Function argument error
      relevant_context$error_type <- "argument_error"
      relevant_context$functions <- workspace_index$functions
      
    } else {
      # General error - include all context
      relevant_context$error_type <- "general_error"
      relevant_context$objects <- workspace_index$objects
      relevant_context$functions <- workspace_index$functions
      relevant_context$data_frames <- workspace_index$data_frames
    }
    
    # Add conversation history if available
    if (exists("conversation_history", envir = .GlobalEnv)) {
      relevant_context$conversation_history_length <- length(.GlobalEnv$conversation_history)
    }
    
    # Add index timestamp
    if (!is.null(workspace_index$last_updated)) {
      relevant_context$index_last_updated <- workspace_index$last_updated
    }
    
    return(relevant_context)
    
  }, error = function(e) {
    return(list(
      error_type = "error_analyzing_context",
      error = e$message
    ))
  })
}

#' Send debug error to AI for analysis
#' @export
send_error_to_ai <- function(error_details) {
  tryCatch({
    # Prepare the AI prompt
    prompt <- paste(
      "Debug this R error:\n\n",
      "ERROR: ", error_details$error_message, "\n\n",
      "CODE THAT CAUSED IT:\n", error_details$error_code, "\n\n",
      "RELEVANT CONTEXT:\n", 
      "Available objects: ", paste(names(error_details$relevant_context$objects), collapse = ", "), "\n",
      "Available functions: ", paste(names(error_details$relevant_context$functions), collapse = ", "), "\n",
      "Available data frames: ", paste(names(error_details$relevant_context$data_frames), collapse = ", "), "\n\n",
      "Please provide:\n",
      "1. What went wrong\n",
      "2. How to fix it\n",
      "3. Corrected code"
    )
    
    # Send to AI backend
    response <- httr::POST(
      "https://rgent.onrender.com/chat",
      body = list(
        access_code = get_current_access_code(),
        prompt = prompt,
        context_data = error_details$relevant_context
      ),
      encode = "json",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      result <- httr::content(response)
      return(list(
        success = TRUE,
        ai_analysis = result$response
      ))
    } else {
      return(list(
        success = FALSE,
        error = paste("AI request failed with status:", httr::status_code(response))
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error sending to AI:", e$message)
    ))
  })
}

# =============================================================================
# SIMPLE, SAFE INDEXING SYSTEM (NO ENVIRONMENTS)
# =============================================================================

# Simple list-based storage (no environments to avoid envir errors)
workspace_index <- list(
  objects = list(),
  data_frames = list(),
  functions = list(),
  variables = list(),
  last_updated = NULL
)

# Safe workspace scanning functions
safe_get_object_info <- function(obj_name) {
  tryCatch({
    obj <- get(obj_name, envir = globalenv())
    list(
      name = obj_name,
      class = paste(class(obj), collapse = ", "),
      type = typeof(obj),
      length = length(obj),
      is_function = is.function(obj),
      is_data_frame = is.data.frame(obj),
      dimensions = if(is.data.frame(obj)) dim(obj) else NULL,
      columns = if(is.data.frame(obj)) names(obj) else NULL
    )
  }, error = function(e) {
    list(
      name = obj_name,
      class = "unknown",
      type = "unknown", 
      length = 0,
      error = e$message
    )
  })
}

update_workspace_index <- function() {
  # Safely scan workspace and update index
  tryCatch({
    
    
    # Get all workspace objects
    workspace_objects <- ls(envir = globalenv())
    
    # Clear old index using global environment
    .GlobalEnv$workspace_index$objects <- list()
    .GlobalEnv$workspace_index$data_frames <- list()
    .GlobalEnv$workspace_index$functions <- list()
    .GlobalEnv$workspace_index$variables <- list()
    
    # Scan each object safely
    for (obj_name in workspace_objects) {
      obj_info <- safe_get_object_info(obj_name)
      
      # Skip NULL or invalid objects
      if (is.null(obj_info)) next
      
      # Add to appropriate category using global environment
      .GlobalEnv$workspace_index$objects[[obj_name]] <- obj_info
      
      if (obj_info$is_data_frame) {
        .GlobalEnv$workspace_index$data_frames[[obj_name]] <- obj_info
      } else if (obj_info$is_function) {
        .GlobalEnv$workspace_index$functions[[obj_name]] <- obj_info
      } else {
        .GlobalEnv$workspace_index$variables[[obj_name]] <- obj_info
      }
    }
    
    .GlobalEnv$workspace_index$last_updated <- Sys.time()
    
  }, error = function(e) {
    cat("Error updating workspace index:", e$message, "\n")
  })
}

# Get current editor context using rstudioapi
get_editor_context <- function() {
  tryCatch({
    if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
      return("RStudio API not available")
    }
    
    # Get active document
    doc <- rstudioapi::getActiveDocumentContext()
    if (is.null(doc)) {
      return("No active document")
    }
    
    # Get cursor position
    cursor <- doc$selection[[1]]$range$start
    line <- cursor[["row"]]
    column <- cursor[["column"]]
    
    # Get document content
    content <- paste(doc$contents, collapse = "\n")
    
    # Get file path
    file_path <- doc$path
    
    list(
      file_path = file_path,
      file_name = basename(file_path),
      cursor_line = line,
      cursor_column = column,
      content_length = nchar(content),
      has_content = length(doc$contents) > 0
    )
    
  }, error = function(e) {
    cat("ERROR: Failed to get editor context:", e$message, "\n")
    return("Editor context unavailable")
  })
}

# Simple context assembly (no complex environments)
assemble_simple_context <- function(query) {
  tryCatch({
    
    
    # Get workspace_index from global environment
    workspace_index <- .GlobalEnv$workspace_index
    
    # Debug workspace_index
    
    
    context_parts <- list()
    
    # 1. Get editor context
    editor_context <- get_editor_context()
    if (is.list(editor_context)) {
      context_parts <- c(context_parts, list(
        paste("=== EDITOR CONTEXT ==="),
        paste("File:", editor_context$file_name),
        paste("Cursor: Line", editor_context$cursor_line, "Column", editor_context$cursor_column),
        paste("Content length:", editor_context$content_length, "characters")
      ))
    }
    
    # 2. Get workspace objects
    if (length(workspace_index$objects) > 0) {
      object_names <- names(workspace_index$objects)
      
      context_parts <- c(context_parts, list(
        paste("=== WORKSPACE OBJECTS ==="),
        paste("Total objects:", length(object_names)),
        paste("Objects:", paste(object_names, collapse = ", "))
      ))
    } else {
      # Silenced debug: no workspace objects
    }
    
    # 3. Get data frames info
    if (length(workspace_index$data_frames) > 0) {
      df_names <- names(workspace_index$data_frames)
      
      df_info <- sapply(df_names, function(name) {
        info <- workspace_index$data_frames[[name]]
        if (!is.null(info$dimensions)) {
          paste(name, ":", info$dimensions[1], "rows ×", info$dimensions[2], "cols")
        } else {
          paste(name, ": data frame")
        }
      })
      context_parts <- c(context_parts, list(
        paste("=== DATA FRAMES ==="),
        paste(df_info, collapse = "\n")
      ))
    } else {
      # Silenced debug: no data frames
    }
    
    # 4. Get functions info
    if (length(workspace_index$functions) > 0) {
      func_names <- names(workspace_index$functions)
      
      context_parts <- c(context_parts, list(
        paste("=== FUNCTIONS ==="),
        paste("Functions:", paste(func_names, collapse = ", "))
      ))
    } else {
      # Silenced debug: no functions
    }
    
    # 5. Add conversation history
    conversation_history <- get_conversation_history()
    if (nchar(conversation_history) > 0) {
      context_parts <- c(context_parts, list(
        paste("=== CONVERSATION HISTORY ==="),
        conversation_history
      ))
    }
    
    # 6. Add basic info
    context_parts <- c(context_parts, list(
      paste("=== BASIC INFO ==="),
      paste("Working directory:", getwd()),
      paste("R version:", R.version.string),
      paste("Timestamp:", Sys.time())
    ))
    
    # Combine all parts
    final_context <- paste(context_parts, collapse = "\n")
    
    
    
    return(final_context)
    
  }, error = function(e) {
    cat("ERROR: Simple context assembly failed:", e$message, "\n")
    return("Context assembly failed - using minimal context")
  })
}

# =============================================================================
# ROBUST CHUNKING SYSTEM
# =============================================================================

chunk_file_robustly <- function(file_content, file_path) {
  # Robust chunking with multiple strategies and validation
  
  chunks <- list()
  
  tryCatch({
    # Primary strategy: Function-based chunking
    chunks <- chunk_by_functions(file_content, file_path)
    
    # Validate chunk quality
    chunks <- validate_and_fix_chunks(chunks, file_content, file_path)
    
    # Fallback: Line-based chunking if function chunking fails
    if (length(chunks) == 0 || any(sapply(chunks, function(c) nchar(c$content) > 2000))) {
      # Silenced debug: function chunking fallback
      chunks <- chunk_by_lines(file_content, max_lines = 50, file_path = file_path)
    }
    
  }, error = function(e) {
    cat("ERROR: Chunking failed, using simple fallback:", e$message, "\n")
    chunks <- chunk_by_lines(file_content, max_lines = 30, file_path = file_path)
  })
  
  return(chunks)
}

chunk_by_functions <- function(file_content, file_path) {
  # Chunk by function boundaries with comprehensive metadata
  
  chunks <- list()
  lines <- strsplit(file_content, "\n")[[1]]
  
  current_chunk <- list(
    content = "",
    start_line = 1,
    end_line = 1,
    type = "unknown",
    function_name = NULL,
    file_path = file_path,
    file_name = basename(file_path),
    chunk_id = NULL,
    metadata = list()
  )
  
  brace_count <- 0
  in_function <- FALSE
  chunk_counter <- 0
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Detect function start
    if (grepl("^\\s*[a-zA-Z_][a-zA-Z0-9_.]*\\s*<-\\s*function\\s*\\(", line)) {
      # Save previous chunk if not empty
      if (nchar(current_chunk$content) > 0 && current_chunk$type != "unknown") {
        chunk_counter <- chunk_counter + 1
        current_chunk$chunk_id <- paste0(basename(file_path), "_", chunk_counter)
        chunks <- c(chunks, list(current_chunk))
      }
      
      # Start new function chunk
      function_name <- extract_function_name(line)
      chunk_counter <- chunk_counter + 1
      current_chunk <- list(
        content = line,
        start_line = i,
        end_line = i,
        type = "function",
        function_name = function_name,
        file_path = file_path,
        file_name = basename(file_path),
        chunk_id = paste0(basename(file_path), "_", chunk_counter),
        metadata = list(
          function_signature = line,
          arguments = extract_function_arguments(line),
          complexity = "medium",
          dependencies = list(),
          docstring = extract_docstring(lines, i)
        )
      )
      in_function <- TRUE
      brace_count <- 0
    } else {
      # Add to current chunk
      current_chunk$content <- paste(current_chunk$content, line, sep = "\n")
      current_chunk$end_line <- i
      
      # Count braces for function boundaries
      if (in_function) {
        brace_count <- brace_count + sum(charToRaw(line) == charToRaw("{"))
        brace_count <- brace_count - sum(charToRaw(line) == charToRaw("}"))
        
        # End function when braces are balanced
        if (brace_count <= 0 && in_function) {
          current_chunk$type <- "function"
          current_chunk$metadata$complexity <- calculate_function_complexity(current_chunk$content)
          current_chunk$metadata$dependencies <- extract_function_dependencies(current_chunk$content)
          chunks <- c(chunks, list(current_chunk))
          
          # Start new chunk
          chunk_counter <- chunk_counter + 1
          current_chunk <- list(
            content = "",
            start_line = i + 1,
            end_line = i + 1,
            type = "unknown",
            function_name = NULL,
            file_path = file_path,
            file_name = basename(file_path),
            chunk_id = paste0(basename(file_path), "_", chunk_counter),
            metadata = list()
          )
          in_function <- FALSE
        }
      }
    }
  }
  
  # Add final chunk
  if (nchar(current_chunk$content) > 0) {
    chunk_counter <- chunk_counter + 1
    current_chunk$chunk_id <- paste0(basename(file_path), "_", chunk_counter)
    chunks <- c(chunks, list(current_chunk))
  }
  
  return(chunks)
}

chunk_by_lines <- function(file_content, max_lines = 50, file_path = NULL) {
  # Fallback: Line-based chunking with metadata
  
  lines <- strsplit(file_content, "\n")[[1]]
  chunks <- list()
  
  for (i in seq(1, length(lines), by = max_lines)) {
    end_line <- min(i + max_lines - 1, length(lines))
    chunk_lines <- lines[i:end_line]
    
    # Detect chunk type based on content
    chunk_type <- "line_chunk"
    function_name <- NULL
    
    # Check if this chunk contains function definitions
    for (line in chunk_lines) {
      if (grepl("^\\s*[a-zA-Z_][a-zA-Z0-9_.]*\\s*<-\\s*function\\s*\\(", line)) {
        chunk_type <- "function_definition"
        function_name <- extract_function_name(line)
        break
      }
    }
    
    chunk <- list(
      content = paste(chunk_lines, collapse = "\n"),
      start_line = i,
      end_line = end_line,
      type = chunk_type,
      function_name = function_name,
      file_path = file_path,
      file_name = if (!is.null(file_path)) basename(file_path) else "unknown",
      chunk_id = paste0(if (!is.null(file_path)) basename(file_path) else "unknown", "_line_", i),
      metadata = list(
        line_count = length(chunk_lines),
        has_comments = any(grepl("^\\s*#", chunk_lines)),
        has_functions = any(grepl("function\\s*\\(", chunk_lines)),
        has_data_assignments = any(grepl("<-", chunk_lines))
      )
    )
    
    chunks <- c(chunks, list(chunk))
  }
  
  return(chunks)
}

validate_and_fix_chunks <- function(chunks, file_content, file_path) {
  # Validate chunks and fix issues while preserving metadata
  
  valid_chunks <- list()
  
  for (chunk in chunks) {
    # Check chunk size
    if (nchar(chunk$content) > 2000) {
      # Split large chunks
      sub_chunks <- split_large_chunk(chunk)
      valid_chunks <- c(valid_chunks, sub_chunks)
    } else if (nchar(chunk$content) < 50) {
      # Skip tiny chunks
      next
    } else {
      # Add validation metadata
      chunk$metadata$validated <- TRUE
      chunk$metadata$content_length <- nchar(chunk$content)
      chunk$metadata$line_count <- length(strsplit(chunk$content, "\n")[[1]])
      valid_chunks <- c(valid_chunks, list(chunk))
    }
  }
  
  return(valid_chunks)
}

split_large_chunk <- function(chunk) {
  # Split large chunks into smaller ones while preserving metadata
  
  lines <- strsplit(chunk$content, "\n")[[1]]
  max_lines_per_chunk <- 30
  
  sub_chunks <- list()
  
  for (i in seq(1, length(lines), by = max_lines_per_chunk)) {
    end_line <- min(i + max_lines_per_chunk - 1, length(lines))
    chunk_lines <- lines[i:end_line]
    
    # Preserve original metadata and add split info
    sub_metadata <- chunk$metadata
    sub_metadata$split_from <- chunk$chunk_id
    sub_metadata$split_index <- i
    
    sub_chunk <- list(
      content = paste(chunk_lines, collapse = "\n"),
      start_line = chunk$start_line + i - 1,
      end_line = chunk$start_line + end_line - 1,
      type = paste0(chunk$type, "_sub"),
      function_name = chunk$function_name,
      file_path = chunk$file_path,
      file_name = chunk$file_name,
      chunk_id = paste0(chunk$chunk_id, "_sub_", i),
      metadata = sub_metadata
    )
    
    sub_chunks <- c(sub_chunks, list(sub_chunk))
  }
  
  return(sub_chunks)
}

extract_function_name <- function(line) {
  # Extract function name from function definition
  
  # Match function name pattern
  match <- regexpr("^\\s*([a-zA-Z_][a-zA-Z0-9_.]*)\\s*<-\\s*function", line)
  if (match > 0) {
    return(substr(line, match, match + attr(match, "match.length") - 1))
  }
  
  return("unknown_function")
}

extract_function_arguments <- function(line) {
  # Extract function arguments from function definition
  
  # Extract arguments from parentheses
  args_match <- regexpr("function\\s*\\(([^)]*)\\)", line)
  if (args_match > 0) {
    args_text <- substr(line, args_match + 8, args_match + attr(args_match, "match.length") - 2)
    if (nchar(args_text) > 0) {
      return(strsplit(args_text, "\\s*,\\s*")[[1]])
    }
  }
  
  return(list())
}

extract_docstring <- function(lines, start_line) {
  # Extract docstring from lines around function definition
  
  docstring <- ""
  
  # Look for comments before function definition
  for (i in max(1, start_line - 5):(start_line - 1)) {
    if (i <= length(lines)) {
      line <- lines[i]
      if (grepl("^\\s*#", line)) {
        docstring <- paste(docstring, substr(line, regexpr("#", line)[1] + 1), sep = "\n")
      }
    }
  }
  
  return(trimws(docstring))
}

calculate_function_complexity <- function(content) {
  # Calculate function complexity based on content
  
  lines <- strsplit(content, "\n")[[1]]
  
  # Simple complexity metrics
  if_else_count <- sum(grepl("if\\s*\\(", lines))
  for_count <- sum(grepl("for\\s*\\(", lines))
  while_count <- sum(grepl("while\\s*\\(", lines))
  function_calls <- sum(grepl("[a-zA-Z_][a-zA-Z0-9_.]*\\s*\\(", lines))
  
  total_complexity <- if_else_count + for_count + while_count + function_calls
  
  if (total_complexity > 10) return("high")
  else if (total_complexity > 5) return("medium")
  else return("low")
}

extract_function_dependencies <- function(content) {
  # Extract function dependencies from content
  
  lines <- strsplit(content, "\n")[[1]]
  dependencies <- list()
  
  # Look for library() and require() calls
  for (line in lines) {
    if (grepl("library\\s*\\(", line)) {
      lib_match <- regexpr("library\\s*\\(([^)]+)\\)", line)
      if (lib_match > 0) {
        lib_name <- substr(line, lib_match + 8, lib_match + attr(lib_match, "match.length") - 2)
        dependencies <- c(dependencies, list(list(type = "library", name = trimws(lib_name))))
      }
    }
  }
  
  return(dependencies)
}

# =============================================================================
# ROBUST HASH-BASED CHANGE DETECTION
# =============================================================================

create_robust_hash <- function(content, metadata = NULL) {
  # Create robust hash with collision protection
  
  # Combine content with metadata for uniqueness
  hash_input <- paste(
    content,
    metadata$start_line %||% "",
    metadata$end_line %||% "",
    metadata$function_name %||% "",
    sep = "|||"
  )
  
  # Use SHA-256 for collision resistance
  hash <- digest::digest(hash_input, algo = "sha256")
  
  # Add content length as additional check
  return(list(
    hash = hash,
    length = nchar(content),
    metadata = metadata
  ))
}

detect_changes_robustly <- function(file_path) {
  # Robust change detection with multiple checks
  
  tryCatch({
    file_content <- readLines(file_path, warn = FALSE)
    current_content <- paste(file_content, collapse = "\n")
    
    # Multiple change detection strategies
    current_hash <- create_robust_hash(current_content)
    current_length <- nchar(current_content)
    
    # Get stored hash info
    stored_hash_key <- paste0("hash_", file_path)
    stored_length_key <- paste0("length_", file_path)
    
    if (exists(stored_hash_key, envir = safe_env(session_index$file_hashes))) {
      old_hash <- session_index$file_hashes[[stored_hash_key]]
      old_length <- session_index$file_hashes[[stored_length_key]]
      
      # Check hash and length
      if (old_hash == current_hash$hash && old_length == current_length) {
        return(list(has_changed = FALSE))
      }
    }
    
    # File has changed - update stored info
    session_index$file_hashes[[stored_hash_key]] <- current_hash$hash
    session_index$file_hashes[[stored_length_key]] <- current_length
    
    return(list(
      has_changed = TRUE,
      new_content = current_content
    ))
    
  }, error = function(e) {
    cat("ERROR: Change detection failed for", file_path, ":", e$message, "\n")
    return(list(has_changed = FALSE))
  })
}

# =============================================================================
# DEBOUNCED FILE WATCHING
# =============================================================================

debounced_file_change <- function(file_path, callback) {
  # Debounced file change handler
  
  # Add to pending changes
  file_change_debouncer$pending_changes[[file_path]] <- Sys.time()
  
  # Cancel existing timer
  if (!is.null(file_change_debouncer$timer)) {
    file_change_debouncer$timer <- NULL
  }
  
  # Set new timer (300ms delay for smooth UX)
  file_change_debouncer$timer <- later::later(function() {
    process_pending_changes(callback)
  }, delay = 0.3)
}

process_pending_changes <- function(callback) {
  # Process all pending file changes asynchronously
  
  if (length(file_change_debouncer$pending_changes) == 0 || 
      file_change_debouncer$is_processing) {
    return()
  }
  
  file_change_debouncer$is_processing <- TRUE
  
  # Get unique files that changed
  changed_files <- names(file_change_debouncer$pending_changes)
  
  # Process changes in background (non-blocking)
  tryCatch({
    for (file_path in changed_files) {
      tryCatch({
        # Quick change detection
        change_info <- detect_changes_robustly(file_path)
        
        if (change_info$has_changed) {
          # Update index for this file
          update_file_index(file_path, change_info$new_content)
        }
      }, error = function(e) {
        cat("ERROR: Failed to process changes for", file_path, ":", e$message, "\n")
      })
    }
    
    # Call callback when done
    if (!is.null(callback)) {
      callback()
    }
    
  }, finally = {
    # Clear pending changes
    file_change_debouncer$pending_changes <- list()
    file_change_debouncer$is_processing <- FALSE
  })
}

# =============================================================================
# MEMORY MANAGEMENT
# =============================================================================

cleanup_old_data <- function() {
  # Clean up old data to prevent memory buildup
  
  tryCatch({
    # Ensure session_index is accessible
    global_env <- globalenv()
    if (!exists("session_index", envir = global_env)) {
      # Silenced debug: creating session_index for cleanup
      global_env$session_index <- new.env()
      global_env$session_index$file_chunks <- new.env()
      global_env$session_index$file_hashes <- new.env()
      global_env$session_index$last_cleanup <- Sys.time()
    }
    
    current_time <- Sys.time()
    last_cleanup <- global_env$session_index$last_cleanup %||% current_time
    
    # Cleanup every 3 minutes
    if (as.numeric(difftime(current_time, last_cleanup, units = "mins")) > 3) {
      
      # Remove old file chunks (keep only 50 most recent)
      if (length(ls(safe_env(global_env$session_index$file_chunks))) > 50) {
        file_times <- sapply(ls(safe_env(global_env$session_index$file_chunks)), function(f) {
          safe_env(global_env$session_index$file_chunks[[f]]$last_accessed %||% 0)
        })
        old_files <- names(sort(file_times)[1:(length(file_times) - 50)])
        rm(list = old_files, envir = safe_env(global_env$session_index$file_chunks))
      }
      
      # Remove old hashes (keep only 100 most recent)
      if (length(ls(safe_env(global_env$session_index$file_hashes))) > 100) {
        hash_times <- sapply(ls(safe_env(global_env$session_index$file_hashes)), function(h) {
          safe_env(global_env$session_index$file_hashes[[h]]$timestamp %||% 0)
        })
        old_hashes <- names(sort(hash_times)[1:(length(hash_times) - 100)])
        rm(list = old_hashes, envir = safe_env(global_env$session_index$file_hashes))
      }
      
      # Force garbage collection
      gc()
      
      global_env$session_index$last_cleanup <- current_time
      # Silenced debug: memory cleanup completed
    }
  }, error = function(e) {
    cat("ERROR: Cleanup failed:", e$message, "\n")
  })
}

# =============================================================================
# ACCURATE TOKEN ESTIMATION
# =============================================================================

estimate_tokens_accurately <- function(text) {
  # Accurate token estimation
  
  # Count actual tokens (rough approximation)
  words <- strsplit(text, "\\s+")[[1]]
  
  # Estimate: 1 token ≈ 4 characters for English code
  char_estimate <- nchar(text) / 4
  
  # Word-based estimate
  word_estimate <- length(words) * 1.3
  
  # Use the more conservative estimate
  return(min(char_estimate, word_estimate))
}

manage_token_budget_accurately <- function(context_parts, max_tokens = 2000) {
  # Accurate token budget management
  
  total_tokens <- sum(sapply(context_parts, estimate_tokens_accurately))
  
  if (total_tokens <= max_tokens) {
    return(context_parts)
  }
  
  # Prioritize context parts
  priorities <- c(
    "cursor_context" = 1.0,
    "semantic_matches" = 0.8,
    "object_context" = 0.6,
    "environment_info" = 0.4
  )
  
  # Trim context based on priorities
  trimmed_parts <- list()
  remaining_tokens <- max_tokens
  
  for (part in context_parts) {
    part_tokens <- estimate_tokens_accurately(part)
    
    if (part_tokens <= remaining_tokens) {
      trimmed_parts <- c(trimmed_parts, list(part))
      remaining_tokens <- remaining_tokens - part_tokens
    } else {
      # Truncate this part
      max_chars <- remaining_tokens * 4
      truncated_part <- substr(part, 1, max_chars)
      trimmed_parts <- c(trimmed_parts, list(truncated_part))
      break
    }
  }
  
  return(trimmed_parts)
}

# =============================================================================
# PERFORMANCE MONITORING
# =============================================================================

track_performance <- function(operation, start_time = Sys.time()) {
  # Track performance metrics
  
  if (!exists("metrics", envir = safe_env(performance_monitor))) {
    performance_monitor$metrics <- list()
  }
  
  function() {
    end_time <- Sys.time()
    duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    if (!exists(operation, envir = safe_env(performance_monitor$metrics))) {
      performance_monitor$metrics[[operation]] <- list()
    }
    
    performance_monitor$metrics[[operation]] <- c(
      performance_monitor$metrics[[operation]], 
      list(duration)
    )
    
    # Alert if performance degrades
    recent_times <- tail(performance_monitor$metrics[[operation]], 10)
    if (length(recent_times) >= 5 && mean(recent_times) > 0.5) {  # > 500ms
      cat("WARNING: Slow performance detected for", operation, ":", mean(recent_times), "s\n")
    }
  }
}

# =============================================================================
# SAFE INDEX UPDATE
# =============================================================================

safe_index_update <- function() {
  # Safely update index with error handling
  
  tryCatch({
    # Track performance
    perf_tracker <- track_performance("index_update")
    
    # Direct workspace scanning (no session_index dependency)
    # Silenced debug: using direct workspace scanning
    
    # Cleanup memory
    cleanup_old_data()
    
    # Track performance
    perf_tracker()
    
  }, error = function(e) {
    cat("ERROR: Index update failed:", e$message, "\n")
    # Fallback to simple context capture
    return(tryCatch({
      capture_context()
    }, error = function(e) {
      cat("ERROR in fallback context capture:", e$message, "\n")
      list(
        workspace_objects = list(),
        active_file = NULL,
        file_contents = NULL
      )
    }))
  })
}

update_file_index <- function(file_path, file_content) {
  # Update index for a specific file with comprehensive metadata
  
  tryCatch({
    # Ensure session_index is accessible
    global_env <- globalenv()
    if (!exists("session_index", envir = global_env)) {
      # Silenced debug: creating session_index
      global_env$session_index <- new.env()
      global_env$session_index$file_chunks <- new.env()
    }
    
    # Chunk the file
    chunks <- chunk_file_robustly(file_content, file_path)
    
    # Create comprehensive file metadata
    file_metadata <- list(
      file_path = file_path,
      file_name = basename(file_path),
      file_size = nchar(file_content),
      line_count = length(strsplit(file_content, "\n")[[1]]),
      chunk_count = length(chunks),
      last_accessed = Sys.time(),
      chunk_types = table(sapply(chunks, function(c) c$type)),
      function_count = sum(sapply(chunks, function(c) c$type == "function")),
      has_functions = any(sapply(chunks, function(c) c$type == "function")),
      has_data_assignments = any(sapply(chunks, function(c) 
        !is.null(c$metadata$has_data_assignments) && c$metadata$has_data_assignments))
    )
    
    # Store chunks with comprehensive metadata
    global_env$session_index$file_chunks[[file_path]] <- list(
      chunks = chunks,
      metadata = file_metadata,
      last_accessed = Sys.time(),
      content_length = nchar(file_content)
    )
    
    # Silenced debug: index and file metadata updated
    
  }, error = function(e) {
    cat("ERROR: Failed to update index for", file_path, ":", e$message, "\n")
  })
}

# =============================================================================
# INTELLIGENT CONTEXT ASSEMBLY (UPDATED)
# =============================================================================

assemble_intelligent_context_safe <- function(query) {
  # Use the new simple context assembly
  tryCatch({
    # Update workspace index first
    update_workspace_index()
    
    # Assemble context using the new system
    context <- assemble_simple_context(query)
    
    return(context)
    
  }, error = function(e) {
    cat("ERROR: Context assembly failed:", e$message, "\n")
    return("Context assembly failed - using minimal context")
  })
}

# =============================================================================
# INITIALIZATION
# =============================================================================

# Initialize the simple system when package loads


# Initialize workspace_index in global environment
if (!exists("workspace_index", envir = .GlobalEnv)) {
  .GlobalEnv$workspace_index <- list(
    objects = list(),
    data_frames = list(),
    functions = list(),
    variables = list(),
    last_updated = NULL
  )
  
}

update_workspace_index()

#' Smart filtering functions for workspace objects
#' @export
smart_filter_objects <- function(query, objects) {
  tryCatch({
    # Extract keywords from query
    keywords <- extract_keywords(query)
    
    # Try exact matches first
    exact_matches <- find_exact_matches(keywords, objects)
    
    # If no exact matches, try fuzzy/partial
    matches <- if (length(exact_matches) == 0) {
      find_partial_matches(keywords, objects)
    } else {
      exact_matches
    }
    
    # If still no matches, return everything (fallback)
    if (length(matches) == 0) {
      return(objects)
    }
    
    return(matches)
  }, error = function(e) {
    cat("ERROR in smart_filter_objects:", e$message, "\n")
    # Return all objects as fallback
    return(objects)
  })
}

#' Extract keywords from user query
extract_keywords <- function(query) {
  # Convert to lowercase for matching
  query_lower <- tolower(query)
  
  # Extract potential object names (words that could be variable names)
  # Look for patterns like: sales_data, customerInfo, product_prices, etc.
  keywords <- c()
  
  # Split by common separators and spaces
  words <- unlist(strsplit(query_lower, "[\\s,_]+"))
  
  # Filter out common words that aren't likely object names
  stop_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", 
                  "of", "with", "by", "is", "are", "was", "were", "be", "been", "being",
                  "have", "has", "had", "do", "does", "did", "will", "would", "could",
                  "should", "may", "might", "can", "show", "me", "my", "all", "data",
                  "plot", "graph", "chart", "summary", "info", "information", "about",
                  "create", "make", "generate", "display", "print", "view", "see")
  
  # Add meaningful keywords
  for (word in words) {
    if (nchar(word) > 1 && !(word %in% stop_words)) {
      keywords <- c(keywords, word)
    }
  }
  
  # Add special keywords for data types
  if (grepl("data|frame|df|table", query_lower)) {
    keywords <- c(keywords, "dataframe")
  }
  if (grepl("plot|graph|chart|visual", query_lower)) {
    keywords <- c(keywords, "plot")
  }
  if (grepl("function|func|method", query_lower)) {
    keywords <- c(keywords, "function")
  }
  if (grepl("vector|array|list", query_lower)) {
    keywords <- c(keywords, "vector")
  }
  
  return(unique(keywords))
}

#' Find exact matches for keywords in object names
find_exact_matches <- function(keywords, objects) {
  tryCatch({
    matches <- list()
    
    if (is.null(objects) || length(objects) == 0) {
      return(matches)
    }
    
    for (obj in objects) {
      if (is.null(obj) || is.null(obj$name)) {
        next
      }
      
      obj_name <- obj$name
      obj_name_lower <- tolower(obj_name)
      
      # Check for exact matches
      for (keyword in keywords) {
        if (!is.null(keyword) && obj_name_lower == keyword) {
          matches[[obj_name]] <- obj
          break
        }
      }
    }
    
    return(matches)
  }, error = function(e) {
    cat("ERROR in find_exact_matches:", e$message, "\n")
    return(list())
  })
}

#' Find partial and fuzzy matches for keywords in object names
find_partial_matches <- function(keywords, objects) {
  tryCatch({
    matches <- list()
    
    if (is.null(objects) || length(objects) == 0) {
      return(matches)
    }
    
    for (obj in objects) {
      if (is.null(obj) || is.null(obj$name)) {
        next
      }
    obj_name <- obj$name
    obj_name_lower <- tolower(obj_name)
    
    # Check for partial matches
    for (keyword in keywords) {
      if (is.null(keyword) || nchar(keyword) == 0) {
        next
      }
      
      # Check if keyword is contained in object name
      tryCatch({
        if (grepl(keyword, obj_name_lower, fixed = TRUE)) {
          matches[[obj_name]] <- obj
          break
        }
      }, error = function(e) {
        # Skip this keyword if grepl fails
      })
      
      # Check if object name is contained in keyword (for abbreviations)
      tryCatch({
        if (grepl(obj_name_lower, keyword, fixed = TRUE)) {
          matches[[obj_name]] <- obj
          break
        }
      }, error = function(e) {
        # Skip this keyword if grepl fails
      })
      
      # Handle underscore variations
      # "salesdata" should match "sales_data"
      obj_no_underscore <- gsub("_", "", obj_name_lower)
      keyword_no_underscore <- gsub("_", "", keyword)
      
      if (obj_no_underscore == keyword_no_underscore) {
        matches[[obj_name]] <- obj
        break
      }
      
      # Handle camelCase variations
      # "salesData" should match "sales_data"
      obj_camel <- gsub("_([a-z])", "\\U\\1", obj_name_lower, perl = TRUE)
      if (obj_camel == keyword) {
        matches[[obj_name]] <- obj
        break
      }
    }
    
    # Check for data type matches
    for (keyword in keywords) {
      if (keyword == "dataframe" && !is.null(obj$is_data_frame) && obj$is_data_frame) {
        matches[[obj_name]] <- obj
        break
      }
      if (keyword == "plot") {
        tryCatch({
          if (grepl("plot", obj_name_lower) || grepl("graph", obj_name_lower) || grepl("chart", obj_name_lower)) {
            matches[[obj_name]] <- obj
            break
          }
        }, error = function(e) {
          # Skip if grepl fails
        })
      }
      if (keyword == "function" && !is.null(obj$is_function) && obj$is_function) {
        matches[[obj_name]] <- obj
        break
      }
      if (keyword == "vector" && !is.null(obj$is_vector) && obj$is_vector) {
        matches[[obj_name]] <- obj
        break
      }
    }
  }
  
  return(matches)
  }, error = function(e) {
    cat("ERROR in find_partial_matches:", e$message, "\n")
    return(list())
  })
}

#' Capture current workspace context with smart filtering
#' @export
capture_context_smart <- function(query = NULL) {
  tryCatch({
    # Get environment information FIRST
    environment_info <- tryCatch({
      list(
        r_version = R.version.string,
        platform = R.version$platform,
        working_directory = getwd(),
        loaded_packages = names(sessionInfo()$otherPkgs)
      )
    }, error = function(e) {
      cat("ERROR getting environment info:", e$message, "\n")
      list(error = e$message)
    })
    
    # Get file information if available
    file_info <- tryCatch({
      doc <- rstudioapi::getActiveDocumentContext()
      if (!is.null(doc)) {
        # Handle document_selection properly
        selection_info <- tryCatch({
          if (!is.null(doc$selection) && length(doc$selection) > 0) {
            list(
              file_path = doc$path,
              file_contents = doc$contents,
              selection_text = doc$selection[[1]]$text,
              cursor_position = list(
                line = doc$selection[[1]]$range$start[1],
                column = doc$selection[[1]]$range$start[2]
              )
            )
          } else {
            list(
              file_path = doc$path,
              file_contents = doc$contents,
              selection_text = NULL,
              cursor_position = NULL
            )
          }
        }, error = function(e) {
          # Fallback if selection processing fails
          list(
            file_path = doc$path,
            file_contents = doc$contents,
            selection_text = NULL,
            cursor_position = NULL
          )
        })
      } else {
        list(file_path = NULL, file_contents = NULL)
      }
    }, error = function(e) {
      cat("ERROR getting file info:", e$message, "\n")
      list(error = e$message)
    })
    
    # Get workspace objects with detailed information
    workspace_objects <- tryCatch({
      # Use globalenv() instead of .GlobalEnv for more robust access
      global_env <- globalenv()
      all_objects <- lapply(safe_ls(global_env), function(obj_name) {
        tryCatch({
          obj <- safe_get(obj_name, global_env)
          
          # Get detailed object information with error handling
          obj_info <- list(
            name = obj_name,
            class = tryCatch(paste(class(obj), collapse = ", "), error = function(e) "unknown"),
            length = tryCatch(length(obj), error = function(e) 0),
            is_function = tryCatch(is.function(obj), error = function(e) FALSE),
            is_data_frame = tryCatch(is.data.frame(obj), error = function(e) FALSE),
            is_vector = tryCatch(is.vector(obj), error = function(e) FALSE),
            is_list = tryCatch(is.list(obj), error = function(e) FALSE),
            is_matrix = tryCatch(is.matrix(obj), error = function(e) FALSE),
            is_array = tryCatch(is.array(obj), error = function(e) FALSE)
          )
          
          # Add object-specific details with intelligent summarization
          
          if (is.data.frame(obj)) {
            obj_info$dimensions <- dim(obj)
            obj_info$column_names <- names(obj)
            obj_info$column_types <- sapply(obj, function(col) paste(class(col), collapse = ", "))
            
            # Only add basic sample data (first 2 rows, first 3 columns)
            if (nrow(obj) > 0) {
              head_data <- head(obj, 2)
              sample_cols <- min(3, ncol(head_data))
              obj_info$sample_data <- lapply(1:sample_cols, function(i) {
                list(
                  column = names(head_data)[i],
                  sample_values = as.character(head_data[[i]])[1:2]  # Only first 2 values
                )
              })
            } else {
              obj_info$sample_data <- list()
            }
            
          } else if (is.vector(obj) && !is.list(obj)) {
            if (length(obj) <= 5) {
              tryCatch({
                obj_info$values <- as.character(obj)
              }, error = function(e) {
                cat("  - Error converting vector to character:", e$message, "\n")
                obj_info$values <- paste("Error: Could not convert to character")
              })
            } else {
              obj_info$summary <- list(
                total_length = length(obj),
                unique_count = tryCatch(length(unique(obj)), error = function(e) "unknown"),
                na_count = tryCatch(sum(is.na(obj)), error = function(e) "unknown")
              )
            }
            
          } else if (is.list(obj) && !is.data.frame(obj)) {
            if (length(obj) <= 3) {
              tryCatch({
                obj_info$list_structure <- lapply(names(obj), function(name) {
                  item <- obj[[name]]
                  list(
                    name = name,
                    class = paste(class(item), collapse = ", "),
                    length = length(item)
                  )
                })
              }, error = function(e) {
                cat("  - Error processing list structure:", e$message, "\n")
                obj_info$list_structure <- list()
              })
            } else {
              tryCatch({
                obj_info$list_summary <- list(
                  total_items = length(obj),
                  item_names = names(obj)[1:5]  # Only first 5 names
                )
              }, error = function(e) {
                obj_info$list_summary <- list(total_items = length(obj), item_names = "unknown")
              })
            }
            
          } else if (is.function(obj)) {
            tryCatch({
              # Get function arguments
              args <- formals(obj)
              obj_info$arguments <- names(args)
              
              # Convert argument defaults to JSON-safe format
              safe_defaults <- lapply(args, function(arg) {
                if (is.symbol(arg)) {
                  as.character(arg)
                } else if (is.null(arg)) {
                  NULL
                } else {
                  as.character(arg)
                }
              })
              obj_info$argument_defaults <- safe_defaults
              
              # Get function body (first few lines)
              body_text <- capture.output(body(obj))
              obj_info$body_preview <- paste(body_text[1:min(5, length(body_text))], collapse = "\n")
            }, error = function(e) {
              cat("  - Error processing function:", e$message, "\n")
              obj_info$arguments <- list()
              obj_info$body_preview <- "Error: Could not process function"
            })
            
          } else {
            tryCatch({
              obj_info$summary <- list(
                class = paste(class(obj), collapse = ", "),
                length = length(obj),
                mode = mode(obj)
              )
            }, error = function(e) {
              obj_info$summary <- list(class = "unknown", length = "unknown", mode = "unknown")
            })
          }
          
          return(obj_info)
        }, error = function(e) {
          cat("ERROR processing object", obj_name, ":", e$message, "\n")
          return(list(
            name = obj_name,
            class = "error",
            error = e$message
          ))
        })
      })
      
      # Filter objects if query provided
      if (!is.null(query) && nchar(query) > 0) {
        
        filtered_objects <- smart_filter_objects(query, all_objects)
        # Return structured list with filtered objects
        return(list(
          workspace_objects = filtered_objects,
          environment_info = environment_info,
          file_info = file_info,
          timestamp = Sys.time()
        ))
      } else {
        # Return structured list with all objects
        return(list(
          workspace_objects = all_objects,
          environment_info = environment_info,
          file_info = file_info,
          timestamp = Sys.time()
        ))
      }
      
    }, error = function(e) {
      cat("ERROR getting workspace objects:", e$message, "\n")
      # Return empty structured list if workspace objects fail
      return(list(
        workspace_objects = list(),
        environment_info = environment_info,
        file_info = file_info,
        timestamp = Sys.time()
      ))
    })
    
  }, error = function(e) {
    cat("ERROR in capture_context:", e$message, "\n")
    list(error = e$message)
  })
}

#' Incremental workspace update functions
#' @export
update_workspace_index_incremental <- function() {
  tryCatch({
    # Silenced debug: updating workspace index incrementally
    
    # Ensure workspace_index exists
    if (!exists("workspace_index", envir = .GlobalEnv)) {
      # Silenced debug: initializing workspace_index
      .GlobalEnv$workspace_index <- list(
        objects = list(),
        data_frames = list(),
        functions = list(),
        variables = list(),
        last_updated = NULL
      )
    }
    
    # Get current workspace objects
    current_objects <- ls(envir = globalenv())
    # Silenced debug: found current workspace objects
    
    # Get last scan info
    last_scan <- .GlobalEnv$workspace_index$last_scan_objects
    if (is.null(last_scan)) {
      # Silenced debug: no previous scan, full scan
      return(update_workspace_index_full())
    }
    
    # Find new, removed, and potentially changed objects
    new_objects <- setdiff(current_objects, last_scan)
    removed_objects <- setdiff(last_scan, current_objects)
    
    # Silenced debug: new objects info
    # Silenced debug: removed objects info
    
    # Process only new objects
    if (length(new_objects) > 0) {
      # Silenced debug: processing new objects
      for (obj_name in new_objects) {
        # Silenced debug: processing new object
        obj_info <- safe_get_object_info(obj_name)
        
        # Store in appropriate category
        .GlobalEnv$workspace_index$objects[[obj_name]] <- obj_info
        
        if (obj_info$is_data_frame) {
          .GlobalEnv$workspace_index$data_frames[[obj_name]] <- obj_info
          # Silenced debug: added to data_frames
        } else if (obj_info$is_function) {
          .GlobalEnv$workspace_index$functions[[obj_name]] <- obj_info
          # Silenced debug: added to functions
        } else {
          .GlobalEnv$workspace_index$variables[[obj_name]] <- obj_info
          # Silenced debug: added to variables
        }
      }
    }
    
    # Remove deleted objects
    if (length(removed_objects) > 0) {
      # Silenced debug: removing deleted objects
              for (obj_name in removed_objects) {
          # Silenced debug: removing object
        .GlobalEnv$workspace_index$objects[[obj_name]] <- NULL
        .GlobalEnv$workspace_index$data_frames[[obj_name]] <- NULL
        .GlobalEnv$workspace_index$functions[[obj_name]] <- NULL
        .GlobalEnv$workspace_index$variables[[obj_name]] <- NULL
      }
    }
    
    # Check for modified objects (objects that exist in both but might have changed)
    common_objects <- intersect(current_objects, last_scan)
    modified_objects <- c()
    
    for (obj_name in common_objects) {
      # Quick check for modifications (length and class)
      current_obj <- tryCatch({
        safe_get(obj_name, globalenv())
      }, error = function(e) NULL)
      
      if (!is.null(current_obj)) {
        stored_obj <- .GlobalEnv$workspace_index$objects[[obj_name]]
        if (!is.null(stored_obj)) {
          # Check if object has changed
          current_length <- length(current_obj)
          current_class <- paste(class(current_obj), collapse = ", ")
          
          if (stored_obj$length != current_length || stored_obj$class != current_class) {
            # Silenced debug: object modified
            modified_objects <- c(modified_objects, obj_name)
            
            # Update the object info
            obj_info <- safe_get_object_info(obj_name)
            .GlobalEnv$workspace_index$objects[[obj_name]] <- obj_info
            
            # Update category
            if (obj_info$is_data_frame) {
              .GlobalEnv$workspace_index$data_frames[[obj_name]] <- obj_info
            } else if (obj_info$is_function) {
              .GlobalEnv$workspace_index$functions[[obj_name]] <- obj_info
            } else {
              .GlobalEnv$workspace_index$variables[[obj_name]] <- obj_info
            }
          }
        }
      }
    }
    
    # Silenced debug: modified objects summary
    
    # Update last scan info
    .GlobalEnv$workspace_index$last_scan_objects <- current_objects
    .GlobalEnv$workspace_index$last_updated <- Sys.time()
    
    # Silenced debug: incremental update complete
    # Silenced debug: final counts summary
    
  }, error = function(e) {
    cat("ERROR: Failed to update workspace index incrementally:", e$message, "\n")
    cat("Falling back to full update...\n")
    update_workspace_index_full()
  })
}

#' Full workspace update (fallback)
#' @export
update_workspace_index_full <- function() {
  tryCatch({
    # Silenced debug: performing full workspace index update
    
    # Get all workspace objects
    workspace_objects <- ls(envir = globalenv())
    # Silenced debug: found workspace objects
    
    # Clear old index using global environment
    .GlobalEnv$workspace_index$objects <- list()
    .GlobalEnv$workspace_index$data_frames <- list()
    .GlobalEnv$workspace_index$functions <- list()
    .GlobalEnv$workspace_index$variables <- list()
    
    # Scan each object safely
    for (obj_name in workspace_objects) {
      # Silenced debug: processing object
      obj_info <- safe_get_object_info(obj_name)
      # Silenced debug: object info flags
      
      # Store in appropriate category using global environment
      .GlobalEnv$workspace_index$objects[[obj_name]] <- obj_info
      
      if (obj_info$is_data_frame) {
        .GlobalEnv$workspace_index$data_frames[[obj_name]] <- obj_info
        # Silenced debug: added to data_frames
      } else if (obj_info$is_function) {
        .GlobalEnv$workspace_index$functions[[obj_name]] <- obj_info
        # Silenced debug: added to functions
      } else {
        .GlobalEnv$workspace_index$variables[[obj_name]] <- obj_info
        # Silenced debug: added to variables
      }
    }
    
    # Store last scan info for incremental updates
    .GlobalEnv$workspace_index$last_scan_objects <- workspace_objects
    .GlobalEnv$workspace_index$last_updated <- Sys.time()
    
    # Silenced debug: full workspace index updated and final counts
    
  }, error = function(e) {
    cat("ERROR: Failed to update workspace index:", e$message, "\n")
  })
}

#' Smart context capture with incremental processing
#' @export
capture_context_smart_incremental <- function(query = NULL) {
  tryCatch({
    # Silenced debug: capturing context with incremental processing
    
    # No incremental update needed - process all objects directly
    
    # Simple approach - process all objects each time
    workspace_objects <- tryCatch({
      
      # Get all objects in global environment
      object_names <- ls(envir = globalenv())
      
      # Process each object with basic info
      all_objects <- lapply(object_names, function(obj_name) {
        tryCatch({
          obj <- get(obj_name, envir = globalenv())
          
          # Basic object info
          obj_info <- list(
            name = obj_name,
            class = paste(class(obj), collapse = ", "),
            is_data_frame = is.data.frame(obj),
            is_function = is.function(obj)
          )
          
          obj_info
        }, error = function(e) {
          list(name = obj_name, class = "error")
        })
      })
      
      all_objects
    }, error = function(e) {
      list()
    })
    
    # Build simple context string
    parts <- c(
      "=== WORKSPACE OBJECTS (BASIC) ===",
      paste(sapply(workspace_objects, function(x) paste("-", x$name, "(", x$class, ")")), collapse = "\n")
    )
    
    paste(parts, collapse = "\n")
    
  }, error = function(e) {
    ""
  })
}

#' Update workspace index

# =============================================================================
# PLOT ANALYSIS SYSTEM
# =============================================================================

#' Find the last plot command in R history
find_last_plot_command <- function() {
  tryCatch({
    # Get recent R history
    temp_file <- tempfile("history", fileext = ".R")
    savehistory(temp_file)
    
    if (file.exists(temp_file)) {
      history_lines <- readLines(temp_file)
      unlink(temp_file)
      
      # Look for plot commands in reverse order
      plot_commands <- c(
        "hist(", "plot(", "boxplot(", "barplot(", "scatterplot(",
        "qplot(", "ggplotly(", "plot_ly(",
        "plotly::", "leaflet::", "dygraph(",
        "density(", "pairs(", "ggpairs(", "GGally::",
        "qqnorm(", "qqplot(", "qqline(", "residuals(", "fitted(",
        "corrplot(", "ggcorrplot(", "heatmap(", "geom_tile(",
        "ts(", "autoplot(", "acf(", "pacf(", "decompose(",
        "prcomp(", "biplot(", "kmeans(", "hclust(",
        "stat_ecdf(", "pie(", "coord_polar("
      )
      
      # First pass: Prioritize ggplot and geom_* reconstruction
      for (i in length(history_lines):1) {
        line <- history_lines[i]
        
        # 1) ggplot: reconstruct multi-line chain starting from ggplot or geoms
        if (grepl("ggplot\\(", line)) {
          return(reconstruct_ggplot_command(history_lines, i))
        }
        
        # 2) geom_*: treat as part of ggplot chain when present
        if (grepl("geom_", line)) {
          return(reconstruct_ggplot_command(history_lines, i))
        }
        
        # 2.5) base R QQ: stitch qqnorm + qqline across adjacent lines
        if (grepl("qqnorm\\(", line) || grepl("qqline\\(", line)) {
          return(reconstruct_qq_command(history_lines, i))
        }
        
        # 3) Other plot commands
        for (cmd in plot_commands) {
          # Skip ggplot or geom_* patterns (handled above)
          if (identical(cmd, "ggplot(") || grepl("^geom_", cmd)) next
          if (grepl(cmd, line, fixed = TRUE)) {
            # Check if this is a multi-line plot command that needs reconstruction
            if (cmd %in% c("plot(", "hist(", "boxplot(", "barplot(")) {
              return(reconstruct_base_plot_command(history_lines, i, cmd))
            } else {
              return(list(
                command = line,
                line_number = i,
                found = TRUE
              ))
            }
          }
        }
      }
    }
    
    return(list(found = FALSE, message = "No plot command found in recent history"))
    
  }, error = function(e) {
    return(list(found = FALSE, message = paste("Error reading history:", e$message)))
  })
}

#' Reconstruct multi-line ggplot2 command
reconstruct_ggplot_command <- function(history_lines, start_line) {
  tryCatch({
    # Silenced debug: reconstruct_ggplot_command called
    
    # Check if we're starting from a geom line or ggplot line
    current_line <- start_line
    line <- history_lines[current_line]
    
    if (grepl("ggplot\\(", line)) {
      # We're starting from a ggplot line
      command_lines <- c(line)
      current_line <- current_line + 1
    } else if (grepl("geom_", line)) {
      # Starting from a geom line: find the nearest preceding ggplot line
      ggplot_index <- NA_integer_
      for (i in (current_line - 1):1) {
        if (grepl("ggplot\\(", history_lines[i])) {
          ggplot_index <- i
          break
        }
      }
      if (is.na(ggplot_index)) {
        # If no ggplot found above, also scan forward in case history saved mid-chain
        for (i in (current_line + 1):length(history_lines)) {
          if (grepl("ggplot\\(", history_lines[i])) {
            ggplot_index <- i
            break
          }
        }
      }
      if (is.na(ggplot_index)) {
        return(list(command = line, line_number = start_line, found = TRUE))
      }
      command_lines <- c(history_lines[ggplot_index])
      current_line <- ggplot_index + 1
    } else {
      # Unknown line type - returning as is
      return(list(
        command = line,
        line_number = start_line,
        found = TRUE
      ))
    }
    
    # Look for continuation lines (lines with + or %>%)
    # Do not skip a line before scanning; start from current_line as set above
    while (current_line <= length(history_lines)) {
      line <- history_lines[current_line]
      
      # Continue if this line is part of the ggplot chain
      if (grepl("^\\s*\\+", line) || grepl("^\\s*%>%", line) || 
          grepl("geom_", line) || grepl("labs\\(", line) || 
          grepl("theme\\(", line) || grepl("scale_", line) ||
          grepl("facet_", line) || grepl("coord_", line)) {
        command_lines <- c(command_lines, line)
        current_line <- current_line + 1
        next
      }
      
      # If previous captured line ended with a +, treat the next line as continuation
        if (length(command_lines) > 0 && grepl("\\+\\s*$", command_lines[length(command_lines)])) {
          command_lines <- c(command_lines, line)
          current_line <- current_line + 1
        next
      }
      
          break
    }
    
    # Combine all lines into a single command
    full_command <- paste(command_lines, collapse = " ")
    # Silenced debug: final reconstructed command
    
    return(list(
      command = full_command,
      line_number = start_line,
      found = TRUE
    ))
    
  }, error = function(e) {
    # Fallback to just the ggplot line
    return(list(
      command = history_lines[start_line],
      line_number = start_line,
      found = TRUE
    ))
  })
}

#' Identify plot type from command
identify_plot_type <- function(command) {
  command_lower <- tolower(command)
  
  if (grepl("hist\\(", command_lower)) {
    return("histogram")
  } else if (grepl("prcomp\\(", command_lower) || grepl("biplot\\(", command_lower)) {
    return("pca_plot")
  } else if (grepl("kmeans\\(", command_lower) || grepl("hclust\\(", command_lower)) {
    return("cluster_plot")
  } else if (grepl("corrplot\\(", command_lower) || grepl("ggcorrplot\\(", command_lower)) {
    return("correlation_plot")
  } else if (grepl("heatmap\\(", command_lower) || grepl("geom_tile\\(", command_lower)) {
    return("heatmap")
  } else if (grepl("ts\\(", command_lower) || grepl("autoplot\\(", command_lower)) {
    return("time_series")
  } else if (grepl("acf\\(", command_lower) || grepl("pacf\\(", command_lower)) {
    return("autocorrelation")
  } else if (grepl("decompose\\(", command_lower)) {
    return("seasonal_decomposition")
  } else if (grepl("plot\\(", command_lower)) {
    # Check if it's a line plot
    if (grepl("type\\s*=\\s*[\"']l[\"']", command_lower)) {
      return("line_plot")
    } else {
      return("scatter")
    }
  } else if (grepl("boxplot\\(", command_lower)) {
    return("boxplot")
  } else if (grepl("barplot\\(", command_lower)) {
    return("barplot")
  } else if (grepl("density\\(", command_lower)) {
    return("density")
  } else if (grepl("pairs\\(", command_lower)) {
    return("pairs")
  } else if (grepl("ggpairs\\(", command_lower) || grepl("ggally::", command_lower)) {
    return("pairs")
  } else if (grepl("ggplot\\(", command_lower)) {
    # For ggplot2, we need to look for the geom type to determine plot type
    if (grepl("geom_violin\\(", command_lower)) {
      return("violin")
    } else if (grepl("geom_density\\(", command_lower)) {
      return("density")
    } else if (grepl("geom_line\\(", command_lower)) {
      return("line_plot")
    } else if (grepl("geom_point\\(", command_lower)) {
      return("scatter")
    } else if (grepl("geom_bar\\(", command_lower)) {
      return("barplot")
    } else if (grepl("geom_boxplot\\(", command_lower)) {
      return("boxplot")
      } else if (grepl("geom_histogram\\(", command_lower)) {
    return("histogram")
  } else if (grepl("geom_qq\\(", command_lower)) {
    return("qq_plot")
  } else if (grepl("geom_qq_line\\(", command_lower)) {
    return("qq_plot")
  } else if (grepl("geom_smooth\\(", command_lower)) {
    return("residual_plot")
  } else if (grepl("geom_", command_lower)) {
    # Default to scatter for other geom types
    return("scatter")
  } else {
    # If no geom found, default to ggplot
    return("ggplot")
  }
  } else if (grepl("geom_density\\(", command_lower)) {
    return("density")
  } else if (grepl("geom_line\\(", command_lower)) {
    return("line_plot")
  } else if (grepl("geom_violin\\(", command_lower)) {
    return("violin")
  } else if (grepl("geom_point\\(", command_lower)) {
    return("scatter")
  } else if (grepl("geom_bar\\(", command_lower)) {
    return("barplot")
  } else if (grepl("geom_boxplot\\(", command_lower)) {
    return("boxplot")
  } else if (grepl("geom_histogram\\(", command_lower)) {
    return("histogram")
  } else if (grepl("geom_qq\\(", command_lower)) {
    return("qq_plot")
  } else if (grepl("geom_qq_line\\(", command_lower)) {
    return("qq_plot")
  } else if (grepl("geom_smooth\\(", command_lower)) {
    return("residual_plot")
  } else if (grepl("geom_", command_lower)) {
    return("scatter")
  } else if (grepl("qqnorm\\(", command_lower) || grepl("qqplot\\(", command_lower)) {
    return("qq_plot")
  } else if (grepl("residuals\\(", command_lower) || grepl("fitted\\(", command_lower)) {
    return("residual_plot")
  } else if (grepl("qqnorm\\(", command_lower) || grepl("qqplot\\(", command_lower)) {
    return("qq_plot")
  } else if (grepl("residuals\\(", command_lower) || grepl("fitted\\(", command_lower)) {
    return("residual_plot")
  } else if (grepl("geom_jitter\\(", command_lower)) {
    return("jitter_plot")
  } else if (grepl("geom_step\\(", command_lower) || grepl("stat_ecdf\\(", command_lower)) {
    return("ecdf_plot")
  } else if (grepl("pie\\(", command_lower) || grepl("coord_polar\\(", command_lower)) {
    return("pie_chart")
  } else if (grepl("plot_ly\\(", command_lower)) {
    # Determine plotly plot type based on type parameter
    if (grepl("type\\s*=\\s*[\"']scatter[\"']", command_lower)) {
      return("plotly_scatter")
    } else if (grepl("type\\s*=\\s*[\"']bar[\"']", command_lower)) {
      return("plotly_bar")
    } else if (grepl("type\\s*=\\s*[\"']histogram[\"']", command_lower)) {
      return("plotly_histogram")
    } else if (grepl("type\\s*=\\s*[\"']box[\"']", command_lower)) {
      return("plotly_box")
    } else {
      return("plotly_scatter")  # Default for plot_ly
    }
  } else if (grepl("ggplotly\\(", command_lower)) {
    return("ggplotly")
  } else if (grepl("plotly::", command_lower)) {
    return("interactive")
  } else if (grepl("leaflet", command_lower)) {
    return("map")
  } else if (grepl("plot\\(", command_lower)) {
    # Fallback for any plot() command that wasn't caught above
    return("scatter")
  } else {
    return("unknown")
  }
}

#' Extract data variables from plot command
extract_plot_data <- function(command, plot_type) {
  tryCatch({
    
    
    
    # Simple extraction - look for common patterns
    if (plot_type == "histogram") {
      # Extract data from hist(data) using regexec for better parsing
      pattern <- "hist\\s*\\(\\s*([^,)]+)"
      match_result <- regexec(pattern, command)
      matches <- regmatches(command, match_result)
      
      if (length(matches) > 0 && length(matches[[1]]) >= 2) {
        data_var <- trimws(matches[[1]][2])
        return(data_var)
      }
    } else if (plot_type == "scatter" || plot_type == "line" || plot_type == "line_plot") {
      # Use regexec for better capture group handling
      # Pattern to match plot(x, y, ...) where x and y are the first two arguments
      pattern <- "plot\\s*\\(\\s*([^,]+)\\s*,\\s*([^,)]+)"
      match_result <- regexec(pattern, command)
      matches <- regmatches(command, match_result)
      
      if (length(matches) > 0 && length(matches[[1]]) >= 3) {
        x_var <- trimws(matches[[1]][2])
        y_var <- trimws(matches[[1]][3])
        return(list(x = x_var, y = y_var))
      }
      
      # If that fails, try to extract single data from plot(data)
      pattern_single <- "plot\\s*\\(\\s*([^,)]+)"
      match_result_single <- regexec(pattern_single, command)
      matches_single <- regmatches(command, match_result_single)
      
      if (length(matches_single) > 0 && length(matches_single[[1]]) >= 2) {
        data_var <- trimws(matches_single[[1]][2])
        return(data_var)
      }
    } else if (plot_type == "density") {
      # Extract data from density(data)
      match <- regexpr("density\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 8, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (plot_type == "pairs") {
      # Extract data from pairs(data) or ggpairs(data)
      match <- regexpr("(pairs|ggpairs)\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 7, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (plot_type == "qq_plot") {
      # Extract data from qqnorm(data) or qqplot(data) using capture group
      qq_exec <- regexec("(qqnorm|qqplot)\\(([^,\")]+|[^)]+)\",?", command)
      qq_match <- regmatches(command, qq_exec)
      if (length(qq_match) > 0 && length(qq_match[[1]]) >= 3) {
        data_var <- trimws(qq_match[[1]][3])
        return(data_var)
      } else {
        # Fallback simpler pattern
      match <- regexpr("(qqnorm|qqplot)\\(([^,)]+)[,)]", command)
      if (match > 0) {
          # Use captured group via regexec for robustness
          exec2 <- regexec("(qqnorm|qqplot)\\(([^,)]+)[,)]", command)
          m2 <- regmatches(command, exec2)
          if (length(m2) > 0 && length(m2[[1]]) >= 3) {
            return(trimws(m2[[1]][3]))
          }
        }
      }
    } else if (plot_type == "residual_plot") {
      # Extract data from residuals(model) or fitted(model)
      match <- regexpr("(residuals|fitted)\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 10, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (plot_type == "map") {
      # Extract data from leaflet(data) or similar
      match <- regexpr("leaflet\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 9, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (grepl("plotly", plot_type)) {
      # Extract data from plotly commands: plot_ly(data, x=~var1, y=~var2, ...)
      # Pattern: plot_ly(data, x=~var1, y=~var2) or plot_ly(data, x=var1, y=var2)
      
      # First extract the data frame (first parameter)
      data_frame_pattern <- "plot_ly\\s*\\(\\s*([^,]+)"
      data_frame_match <- regexec(data_frame_pattern, command)
      data_frame_matches <- regmatches(command, data_frame_match)
      
      data_frame <- NULL
      if (length(data_frame_matches) > 0 && length(data_frame_matches[[1]]) >= 2) {
        data_frame <- trimws(data_frame_matches[[1]][2])
      }
      
      # Extract x variable: x = ~var or x = var
      x_var <- NULL
      x_pattern <- "x\\s*=\\s*~?([^,)\\s]+)"
      x_match <- regexec(x_pattern, command)
      x_matches <- regmatches(command, x_match)
      if (length(x_matches) > 0 && length(x_matches[[1]]) >= 2) {
        x_var <- trimws(x_matches[[1]][2])
      }
      
      # Extract y variable: y = ~var or y = var
      y_var <- NULL
      y_pattern <- "y\\s*=\\s*~?([^,)\\s]+)"
      y_match <- regexec(y_pattern, command)
      y_matches <- regmatches(command, y_match)
      if (length(y_matches) > 0 && length(y_matches[[1]]) >= 2) {
        y_var <- trimws(y_matches[[1]][2])
      }
      
      # Extract color variable if present: color = ~var
      color_var <- NULL
      color_pattern <- "color\\s*=\\s*~?([^,)\\s]+)"
      color_match <- regexec(color_pattern, command)
      color_matches <- regmatches(command, color_match)
      if (length(color_matches) > 0 && length(color_matches[[1]]) >= 2) {
        color_var <- trimws(color_matches[[1]][2])
      }
      
      # Return structured data
      if (!is.null(data_frame)) {
        result <- list(
          data_frame = data_frame,
          x = x_var,
          y = y_var,
          color = color_var
        )
        return(result)
      }
      
    } else if (plot_type == "ggplotly") {
      # For ggplotly, extract the ggplot part and parse that
      ggplot_pattern <- "ggplotly\\s*\\(\\s*(.+)\\s*\\)"
      ggplot_match <- regexec(ggplot_pattern, command)
      ggplot_matches <- regmatches(command, ggplot_match)
      
      if (length(ggplot_matches) > 0 && length(ggplot_matches[[1]]) >= 2) {
        ggplot_code <- trimws(ggplot_matches[[1]][2])
        # Recursively extract from the ggplot part
        ggplot_type <- identify_plot_type(ggplot_code)
        return(extract_plot_data(ggplot_code, ggplot_type))
      }
      
    } else if (plot_type == "correlation_plot") {
      # Extract data from corrplot(data) or ggcorrplot(data)
      match <- regexpr("(corrplot|ggcorrplot)\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 10, match + attr(match, "match.length") - 2)
        
        # If the data_var contains nested functions like cor(mtcars), extract the inner data
        if (grepl("cor\\(", data_var)) {
          inner_match <- regexpr("cor\\(([^,)]+)[,)]", data_var)
          if (inner_match > 0) {
            inner_data <- substr(data_var, inner_match + 4, inner_match + attr(inner_match, "match.length") - 2)
            return(inner_data)
          }
        }
        
        return(data_var)
      }
    } else if (plot_type == "heatmap") {
      # Extract data from heatmap(data) or geom_tile(data)
      match <- regexpr("(heatmap|geom_tile)\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 9, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (plot_type == "time_series") {
      # Extract data from ts(data) or autoplot(data)
      match <- regexpr("(ts|autoplot)\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 4, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (plot_type == "autocorrelation") {
      # Extract data from acf(data) or pacf(data)
      match <- regexpr("(acf|pacf)\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 5, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (plot_type == "seasonal_decomposition") {
      # Extract data from decompose(data)
      match <- regexpr("decompose\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 10, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (plot_type == "pca_plot") {
      # Extract data from prcomp(data) or biplot(data)
      match <- regexpr("(prcomp|biplot)\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 8, match + attr(match, "match.length") - 2)
        
        # Handle indexed data like pca_result (from prcomp(mtcars[,1:4]))
        # If the data_var is a result object, try to get the original data
        if (grepl("_result$", data_var) || grepl("_pca$", data_var)) {
          # Try to find the original data by looking for the variable assignment
          # This is a simplified approach - in practice, we'd need more sophisticated parsing
          return(data_var)
        }
        
        return(data_var)
      }
    } else if (plot_type == "cluster_plot") {
      # Extract data from kmeans(data) or hclust(data)
      match <- regexpr("(kmeans|hclust)\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 8, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (plot_type == "jitter_plot" || plot_type == "ecdf_plot" || plot_type == "pie_chart") {
      # These are ggplot2 plots, handled by the general ggplot2 extraction
      # They will be caught by the violin/ggplot section below
      return(NULL)
    } else if (plot_type == "violin" || plot_type == "ggplot" || plot_type == "scatter" || plot_type == "line" || plot_type == "line_plot" || plot_type == "density") {
      # Silenced debug: processing ggplot2 plot type
      # For ggplot2 plots, extract the data frame and variables from aes()
      # First extract the data frame
      data_match <- regexpr("ggplot\\(([^,]+)", command)
      # Silenced debug: data frame regex match position
      if (data_match > 0) {
        data_frame <- substr(command, data_match + 7, data_match + attr(data_match, "match.length") - 1)
        # Silenced debug: extracted data frame
        
        # Then extract variables from aes() - check both in ggplot() and in geom_*()
        aes_match <- regexpr("aes\\(([^)]+)\\)", command)
        # Silenced debug: aes() regex match position
        
        # If no aes() found in main command, look for aes() in geom_*() functions
        if (aes_match <= 0) {
          # Look for aes() inside geom_*() functions
          geom_aes_match <- regexpr("geom_[^(]*\\([^)]*aes\\(([^)]+)\\)[^)]*\\)", command)
          # Silenced debug: geom_aes() regex match position
          if (geom_aes_match > 0) {
            # Extract the aes content from within the geom function
            geom_content <- substr(command, geom_aes_match, geom_aes_match + attr(geom_aes_match, "match.length") - 1)
                          aes_in_geom_match <- regexpr("aes\\(([^)]+)\\)", geom_content)
              if (aes_in_geom_match > 0) {
                aes_content <- substr(geom_content, aes_in_geom_match + 5, aes_in_geom_match + attr(aes_in_geom_match, "match.length") - 2)
                # Silenced debug: aes() found in geom function
              }
          }
        } else {
          aes_content <- substr(command, aes_match + 5, aes_match + attr(aes_match, "match.length") - 2)
          # Silenced debug: extracted aes content from main command
        }
        
        if (exists("aes_content") && !is.null(aes_content)) {
          
          # Extract x and y variables from aes(...) supporting expressions
          x_pattern <- "x\\s*=\\s*([^,\\)]+)"
          y_pattern <- "y\\s*=\\s*([^,\\)]+)"
          
          # Use regexec to capture group 1 (the value after x= / y=)
          x_exec <- regexec(x_pattern, aes_content, perl = TRUE)
          y_exec <- regexec(y_pattern, aes_content, perl = TRUE)
          x_res <- regmatches(aes_content, x_exec)
          y_res <- regmatches(aes_content, y_exec)
          
          x_var <- if (length(x_res) > 0 && length(x_res[[1]]) >= 2) trimws(x_res[[1]][2]) else NULL
          y_var <- if (length(y_res) > 0 && length(y_res[[1]]) >= 2) trimws(y_res[[1]][2]) else NULL
          
          # Fallback: unnamed aesthetics like aes(xvar, yvar)
          if (is.null(x_var) && is.null(y_var) && !grepl("=", aes_content, fixed = TRUE)) {
            parts <- strsplit(aes_content, ",")[[1]]
            parts <- trimws(parts)
            if (length(parts) >= 1) x_var <- parts[1]
            if (length(parts) >= 2) y_var <- parts[2]
          }
          
          if (!is.null(x_var) && !is.null(y_var)) {
            return(list(
              data_frame = data_frame,
              x = x_var,
              y = y_var
            ))
          } else if (!is.null(x_var)) {
            return(list(
              data_frame = data_frame,
              x = x_var
            ))
          } else {
            # If we can't extract variables, just return the data frame
            return(data_frame)
          }
        } else {
          # If no aes() found anywhere, just return the data frame
          return(data_frame)
        }
      }
    }
    
    return(NULL)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Generate analysis commands based on plot type
generate_analysis_commands <- function(plot_type, data_var) {
  commands <- list()
  
  if (plot_type == "histogram") {
    commands$basic_stats <- paste0("summary(", data_var, ")")
    commands$distribution <- paste0("hist(", data_var, ", plot=FALSE)")
    commands$outliers <- paste0("boxplot.stats(", data_var, ")$out")
    commands$normality <- paste0("shapiro.test(", data_var, ")")
    commands$skewness <- paste0("if(require(e1071)) skewness(", data_var, ") else 'e1071 package needed'")
  } else if (plot_type == "scatter") {
    commands$correlation <- paste0("cor(", data_var$x, ", ", data_var$y, ")")
    commands$regression <- paste0("summary(lm(", data_var$y, " ~ ", data_var$x, "))")
    commands$outliers <- paste0("outlierTest(lm(", data_var$y, " ~ ", data_var$x, "))")
  } else if (plot_type == "line" || plot_type == "line_plot") {
    commands$correlation <- paste0("cor(", data_var$x, ", ", data_var$y, ")")
    commands$time_series <- paste0("if(require(ts)) acf(", data_var$y, ") else 'ts package needed'")
  } else if (plot_type == "density") {
    commands$basic_stats <- paste0("summary(", data_var, ")")
    commands$density_estimation <- paste0("density(", data_var, ")")
    commands$normality <- paste0("shapiro.test(", data_var, ")")
    commands$skewness <- paste0("if(require(e1071)) skewness(", data_var, ") else 'e1071 package needed'")
  } else if (plot_type == "pairs") {
    commands$correlation_matrix <- paste0("cor(", data_var, ")")
    commands$summary_stats <- paste0("summary(", data_var, ")")
    commands$multivariate_analysis <- paste0("if(require(corrplot)) corrplot(cor(", data_var, ")) else 'corrplot package needed'")
  } else if (plot_type == "qq_plot") {
    commands$normality_test <- paste0("shapiro.test(", data_var, ")")
    commands$summary_stats <- paste0("summary(", data_var, ")")
    commands$skewness <- paste0("if(require(e1071)) skewness(", data_var, ") else 'e1071 package needed'")
    commands$kurtosis <- paste0("if(require(e1071)) kurtosis(", data_var, ") else 'e1071 package needed'")
  } else if (plot_type == "residual_plot") {
    commands$residual_analysis <- paste0("summary(", data_var, ")")
    commands$normality_test <- paste0("shapiro.test(", data_var, ")")
    commands$homoscedasticity <- paste0("if(require(lmtest)) bptest(", data_var, ") else 'lmtest package needed'")
    commands$autocorrelation <- paste0("if(require(lmtest)) dwtest(", data_var, ") else 'lmtest package needed'")
  } else if (plot_type == "map") {
    commands$spatial_analysis <- paste0("summary(", data_var, ")")
    commands$coordinates_check <- paste0("if(require(sf)) st_bbox(", data_var, ") else 'sf package needed'")
    commands$data_structure <- paste0("str(", data_var, ")")
  } else if (plot_type == "correlation_plot") {
    commands$correlation_matrix <- paste0("cor(", data_var, ")")
    commands$correlation_significance <- paste0("if(require(Hmisc)) rcorr(as.matrix(", data_var, ")) else 'Hmisc package needed'")
    commands$correlation_heatmap <- paste0("if(require(corrplot)) corrplot(cor(", data_var, "), method='color') else 'corrplot package needed'")
  } else if (plot_type == "correlation") {
    # For our new correlation plot type
    commands$correlation_matrix <- paste0("cor(", data_var, ")")
    commands$correlation_significance <- paste0("if(require(Hmisc)) rcorr(as.matrix(", data_var, ")) else 'Hmisc package needed'")
    commands$correlation_heatmap <- paste0("if(require(corrplot)) corrplot(cor(", data_var, "), method='color') else 'corrplot package needed'")
    commands$correlation_summary <- paste0("summary(", data_var, ")")
  } else if (plot_type == "qqplot") {
    # For our new Q-Q plot type
    commands$normality_test <- paste0("shapiro.test(", data_var, ")")
    commands$summary_stats <- paste0("summary(", data_var, ")")
    commands$skewness <- paste0("if(require(e1071)) skewness(", data_var, ") else 'e1071 package needed'")
    commands$kurtosis <- paste0("if(require(e1071)) kurtosis(", data_var, ") else 'e1071 package needed'")
    commands$quantiles <- paste0("quantile(", data_var, ", probs = c(0.25, 0.5, 0.75))")
    commands$outliers <- paste0("boxplot.stats(", data_var, ")$out")
  } else if (plot_type == "residual") {
    # For our new residual plot type
    commands$residual_analysis <- paste0("summary(", data_var, ")")
    commands$normality_test <- paste0("shapiro.test(", data_var, ")")
    commands$homoscedasticity <- paste0("if(require(lmtest)) bptest(", data_var, ") else 'lmtest package needed'")
    commands$autocorrelation <- paste0("if(require(lmtest)) dwtest(", data_var, ") else 'lmtest package needed'")
    commands$residual_summary <- paste0("summary(residuals(", data_var, "))")
    commands$residual_std <- paste0("sd(residuals(", data_var, "), na.rm=TRUE)")
  } else if (plot_type == "heatmap") {
    commands$data_summary <- paste0("summary(", data_var, ")")
    commands$data_structure <- paste0("str(", data_var, ")")
    commands$range_analysis <- paste0("range(", data_var, ", na.rm=TRUE)")
  } else if (plot_type == "time_series") {
    commands$time_series_summary <- paste0("summary(", data_var, ")")
    commands$autocorrelation <- paste0("acf(", data_var, ", plot=FALSE)")
    commands$partial_autocorrelation <- paste0("pacf(", data_var, ", plot=FALSE)")
    commands$stationarity <- paste0("if(require(tseries)) adf.test(", data_var, ") else 'tseries package needed'")
  } else if (plot_type == "autocorrelation") {
    commands$acf_analysis <- paste0("acf(", data_var, ", plot=FALSE)")
    commands$pacf_analysis <- paste0("pacf(", data_var, ", plot=FALSE)")
    commands$lag_analysis <- paste0("lag(", data_var, ")")
  } else if (plot_type == "seasonal_decomposition") {
    commands$decomposition <- paste0("decompose(", data_var, ")")
    commands$seasonal_analysis <- paste0("seasonal(", data_var, ")")
    commands$trend_analysis <- paste0("trend(", data_var, ")")
  } else if (plot_type == "pca_plot") {
    commands$pca_summary <- paste0("summary(", data_var, ")")
    commands$variance_explained <- paste0("(", data_var, ")$sdev^2 / sum((", data_var, ")$sdev^2)")
    commands$loadings <- paste0("(", data_var, ")$rotation")
    commands$scores <- paste0("(", data_var, ")$x")
  } else if (plot_type == "cluster_plot") {
    commands$cluster_summary <- paste0("summary(", data_var, ")")
    commands$cluster_sizes <- paste0("table(", data_var, "$cluster)")
    commands$cluster_centers <- paste0("(", data_var, ")$centers")
    commands$within_ss <- paste0("(", data_var, ")$withinss")
  } else if (plot_type == "jitter_plot" && is.list(data_var) && !is.null(data_var$data_frame)) {
    # Handle jitter plots (categorical data analysis)
    if (!is.null(data_var$x) && !is.null(data_var$y)) {
      commands$categorical_analysis <- paste0("table(", data_var$data_frame, "$", data_var$x, ")")
      commands$group_summary <- paste0("if(require(dplyr)) ", data_var$data_frame, " %>% group_by(", data_var$x, ") %>% summarise(mean_", data_var$y, " = mean(", data_var$y, ")) else 'dplyr package needed'")
      commands$anova_test <- paste0("aov(", data_var$data_frame, "$", data_var$y, " ~ ", data_var$data_frame, "$", data_var$x, ")")
    } else {
      commands$data_summary <- paste0("summary(", data_var$data_frame, ")")
      commands$data_structure <- paste0("str(", data_var$data_frame, ")")
    }
  } else if (plot_type == "ecdf_plot" && is.list(data_var) && !is.null(data_var$data_frame)) {
    # Handle ECDF plots (cumulative distribution)
    if (!is.null(data_var$y)) {
      commands$ecdf_analysis <- paste0("ecdf(", data_var$data_frame, "$", data_var$y, ")")
      commands$quantile_analysis <- paste0("quantile(", data_var$data_frame, "$", data_var$y, ")")
      commands$percentile_analysis <- paste0("summary(", data_var$data_frame, "$", data_var$y, ")")
    } else {
      commands$data_summary <- paste0("summary(", data_var$data_frame, ")")
      commands$data_structure <- paste0("str(", data_var$data_frame, ")")
    }
  } else if (plot_type == "pie_chart" && is.list(data_var) && !is.null(data_var$data_frame)) {
    # Handle pie charts (categorical data)
    if (!is.null(data_var$y)) {
      commands$category_counts <- paste0("table(", data_var$data_frame, "$", data_var$y, ")")
      commands$category_percentages <- paste0("prop.table(table(", data_var$data_frame, "$", data_var$y, ")) * 100")
      commands$category_summary <- paste0("summary(", data_var$data_frame, "$", data_var$y, ")")
    } else {
      commands$data_summary <- paste0("summary(", data_var$data_frame, ")")
      commands$data_structure <- paste0("str(", data_var$data_frame, ")")
    }
  } else if (plot_type == "violin") {
    # Handle violin plots specifically
    if (is.list(data_var) && !is.null(data_var$data_frame)) {
      if (!is.null(data_var$x) && !is.null(data_var$y)) {
        commands$summary_stats <- paste0("summary(", data_var$data_frame, "$", data_var$y, ")")
        commands$group_analysis <- paste0("if(require(dplyr)) ", data_var$data_frame, " %>% group_by(", data_var$x, ") %>% summarise(mean_", data_var$y, " = mean(", data_var$y, ")) else 'dplyr package needed'")
        commands$distribution <- paste0("density(", data_var$data_frame, "$", data_var$y, ")")
      } else {
        commands$summary_stats <- paste0("summary(", data_var$data_frame, ")")
        commands$structure <- paste0("str(", data_var$data_frame, ")")
      }
    } else {
      commands$summary_stats <- paste0("summary(", data_var, ")")
      commands$structure <- paste0("str(", data_var, ")")
    }
  } else if (plot_type == "scatter" && is.list(data_var) && !is.null(data_var$data_frame)) {
     # Handle ggplot2 scatter plots with expressions
    if (!is.null(data_var$x) && !is.null(data_var$y)) {
       commands$correlation <- paste0("with(", data_var$data_frame, ", cor(", data_var$x, ", ", data_var$y, ", use = \"complete.obs\"))")
       commands$regression <- paste0("with(", data_var$data_frame, ", summary(lm((", data_var$y, ") ~ (", data_var$x, "), na.action = na.exclude)))")
       commands$summary_stats <- paste0("with(", data_var$data_frame, ", summary(", data_var$y, "))")
    } else {
      commands$summary_stats <- paste0("summary(", data_var$data_frame, ")")
      commands$structure <- paste0("str(", data_var$data_frame, ")")
    }
  } else if ((plot_type == "line" || plot_type == "line_plot") && is.list(data_var) && !is.null(data_var$data_frame)) {
     # Handle ggplot2 line plots with expressions
    if (!is.null(data_var$x) && !is.null(data_var$y)) {
       commands$correlation <- paste0("with(", data_var$data_frame, ", cor(", data_var$x, ", ", data_var$y, ", use = \"complete.obs\"))")
       commands$time_series <- paste0("with(", data_var$data_frame, ", if(require(ts)) acf(", data_var$y, ") else 'ts package needed')")
    } else {
      commands$summary_stats <- paste0("summary(", data_var$data_frame, ")")
      commands$structure <- paste0("str(", data_var$data_frame, ")")
    }
  } else if (plot_type == "density" && is.list(data_var) && !is.null(data_var$data_frame)) {
    # Handle ggplot2 density plots
    if (!is.null(data_var$y)) {
      commands$basic_stats <- paste0("summary(", data_var$data_frame, "$", data_var$y, ")")
      commands$density_estimation <- paste0("density(", data_var$data_frame, "$", data_var$y, ")")
      commands$normality <- paste0("shapiro.test(", data_var$data_frame, "$", data_var$y, ")")
      commands$skewness <- paste0("if(require(e1071)) skewness(", data_var$data_frame, "$", data_var$y, ") else 'e1071 package needed'")
    } else {
      commands$summary_stats <- paste0("summary(", data_var$data_frame, ")")
      commands$structure <- paste0("str(", data_var$data_frame, ")")
    }
  } else if (plot_type == "ggplot") {
    # Generic ggplot2 handling
    if (is.list(data_var) && !is.null(data_var$data_frame)) {
      if (!is.null(data_var$x) && !is.null(data_var$y)) {
        commands$summary_stats <- paste0("summary(", data_var$data_frame, "$", data_var$y, ")")
        commands$correlation <- paste0("cor(", data_var$data_frame, "$", data_var$x, ", ", data_var$data_frame, "$", data_var$y, ")")
      } else {
        commands$summary_stats <- paste0("summary(", data_var$data_frame, ")")
        commands$structure <- paste0("str(", data_var$data_frame, ")")
      }
    } else {
      commands$summary_stats <- paste0("summary(", data_var, ")")
      commands$structure <- paste0("str(", data_var, ")")
    }
  } else if (plot_type == "boxplot") {
    commands$basic_stats <- paste0("summary(", data_var, ")")
    commands$outliers <- paste0("boxplot.stats(", data_var, ")$out")
  } else if (grepl("plotly", plot_type)) {
    # Handle plotly plots: plot_ly(data, x=~var1, y=~var2, ...)
    if (is.list(data_var) && !is.null(data_var$data_frame)) {
      commands$data_summary <- paste0("summary(", data_var$data_frame, ")")
      commands$data_structure <- paste0("str(", data_var$data_frame, ")")
      
      # Add specific analysis based on plot type and variables
      if (plot_type == "plotly_scatter" && !is.null(data_var$x) && !is.null(data_var$y)) {
        commands$correlation <- paste0("with(", data_var$data_frame, ", cor(", data_var$x, ", ", data_var$y, ", use = \"complete.obs\"))")
        commands$regression <- paste0("with(", data_var$data_frame, ", summary(lm(", data_var$y, " ~ ", data_var$x, ")))")
        commands$x_summary <- paste0("summary(", data_var$data_frame, "$", data_var$x, ")")
        commands$y_summary <- paste0("summary(", data_var$data_frame, "$", data_var$y, ")")
        
        # Add color grouping analysis if color variable exists
        if (!is.null(data_var$color)) {
          commands$group_analysis <- paste0("if(require(dplyr)) ", data_var$data_frame, " %>% group_by(", data_var$color, ") %>% summarise(mean_", data_var$y, " = mean(", data_var$y, ", na.rm=TRUE), count = n()) else 'dplyr package needed'")
          commands$color_distribution <- paste0("table(", data_var$data_frame, "$", data_var$color, ")")
        }
        
      } else if (plot_type == "plotly_histogram" && !is.null(data_var$x)) {
        commands$basic_stats <- paste0("summary(", data_var$data_frame, "$", data_var$x, ")")
        commands$distribution <- paste0("hist(", data_var$data_frame, "$", data_var$x, ", plot=FALSE)")
        commands$normality <- paste0("shapiro.test(", data_var$data_frame, "$", data_var$x, ")")
        
        if (!is.null(data_var$color)) {
          commands$group_comparison <- paste0("if(require(dplyr)) ", data_var$data_frame, " %>% group_by(", data_var$color, ") %>% summarise(mean = mean(", data_var$x, ", na.rm=TRUE), sd = sd(", data_var$x, ", na.rm=TRUE)) else 'dplyr package needed'")
        }
        
      } else if (plot_type == "plotly_box" && !is.null(data_var$y)) {
        commands$basic_stats <- paste0("summary(", data_var$data_frame, "$", data_var$y, ")")
        commands$outliers <- paste0("boxplot.stats(", data_var$data_frame, "$", data_var$y, ")$out")
        
        if (!is.null(data_var$x)) {
          commands$group_comparison <- paste0("if(require(dplyr)) ", data_var$data_frame, " %>% group_by(", data_var$x, ") %>% summarise(median = median(", data_var$y, ", na.rm=TRUE), iqr = IQR(", data_var$y, ", na.rm=TRUE)) else 'dplyr package needed'")
          commands$anova_test <- paste0("aov(", data_var$y, " ~ ", data_var$x, ", data = ", data_var$data_frame, ")")
        }
        
      } else if (plot_type == "plotly_bar" && !is.null(data_var$x)) {
        commands$category_counts <- paste0("table(", data_var$data_frame, "$", data_var$x, ")")
        commands$proportions <- paste0("prop.table(table(", data_var$data_frame, "$", data_var$x, "))")
        
        if (!is.null(data_var$y)) {
          commands$category_summary <- paste0("if(require(dplyr)) ", data_var$data_frame, " %>% group_by(", data_var$x, ") %>% summarise(total = sum(", data_var$y, ", na.rm=TRUE), mean = mean(", data_var$y, ", na.rm=TRUE)) else 'dplyr package needed'")
        }
      }
      
      # Interactive plot specific analysis
      commands$interactive_info <- paste0("# Interactive plotly plot with ", 
                                        ifelse(!is.null(data_var$x), "x-axis", ""), 
                                        ifelse(!is.null(data_var$y), " and y-axis", ""),
                                        ifelse(!is.null(data_var$color), " colored by groups", ""))
    }
  } else if (plot_type == "ggplotly") {
    # ggplotly inherits analysis from the underlying ggplot
    # The variable extraction should have been delegated to the ggplot parser
    if (is.list(data_var) && !is.null(data_var$data_frame)) {
      commands$data_summary <- paste0("summary(", data_var$data_frame, ")")
      commands$interactive_note <- "# This is an interactive version of a ggplot2 plot"
      
      # Add basic analysis if we have x/y variables
      if (!is.null(data_var$x) && !is.null(data_var$y)) {
        commands$correlation <- paste0("with(", data_var$data_frame, ", cor(", data_var$x, ", ", data_var$y, ", use = \"complete.obs\"))")
      }
    }
  }
  
  return(commands)
}

#' Execute analysis commands and collect results
execute_plot_analysis <- function(commands) {
  results <- list()
  
  for (name in names(commands)) {
    tryCatch({
      # Execute command and capture both output and return value
      output <- capture.output({
        result <- eval(parse(text = commands[[name]]))
      })
      
      # Combine output and result
      if (length(output) > 0) {
        results[[name]] <- c(output, paste("Result:", toString(result)))
      } else {
        results[[name]] <- paste("Result:", toString(result))
      }
    }, error = function(e) {
      results[[name]] <- paste("Error:", e$message)
    })
  }
  
  return(results)
}

#' Main plot analysis function
analyze_last_plot <- function() {
  tryCatch({
    # Step 1: Check for stored plot command from visualization agent first
    plot_info <- NULL
    
    if (exists("last_plot_command", envir = .GlobalEnv)) {
      stored_command <- get("last_plot_command", envir = .GlobalEnv)
      stored_timestamp <- get("last_plot_timestamp", envir = .GlobalEnv)
      
      # Check if the stored command is recent (within last 5 minutes)
      if (difftime(Sys.time(), stored_timestamp, units = "mins") < 5) {
        plot_info <- list(found = TRUE, command = stored_command, source = "visualization_agent")
      }
    }
    
    # If no stored command, fall back to history search
    if (is.null(plot_info)) {
      plot_info <- find_last_plot_command()
    }
    
    if (!plot_info$found) {
      return(list(
        success = FALSE,
        message = "No plot command found in recent history. Try creating a plot first!"
      ))
    }
    
    # Identify plot type
    plot_type <- identify_plot_type(plot_info$command)
    
    # Extract data variables
    data_var <- extract_plot_data(plot_info$command, plot_type)
    
    if (is.null(data_var)) {
      # Silenced debug: data extraction failed
      return(list(
        success = FALSE,
        message = "Could not extract data variables from plot command."
      ))
    }
    
    # Step 4: Generate analysis commands
    commands <- generate_analysis_commands(plot_type, data_var)
    
    # Step 5: Execute analysis
    results <- execute_plot_analysis(commands)
    
    # Step 6: Compile analysis
    analysis <- list(
      plot_command = plot_info$command,
      plot_type = plot_type,
      data_variables = data_var,
      analysis_results = results,
      timestamp = Sys.time()
    )
    
    return(list(
      success = TRUE,
      analysis = analysis
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Analysis failed:", e$message)
    ))
  })
}

#' Analyze visualization agent plot specifically
#' This function is designed for visualization agent plots and doesn't interfere with the analyze_last_plot button
analyze_visualization_plot <- function(plot_command, plot_type, data_variables) {
  tryCatch({
    # Generate analysis commands for this specific plot
    commands <- generate_analysis_commands(plot_type, data_variables)
    
    # Execute analysis
    results <- execute_plot_analysis(commands)
    
    # Create analysis summary
    analysis <- list(
      plot_command = plot_command,
      plot_type = plot_type,
      data_variables = data_variables,
      analysis_results = results,
      timestamp = Sys.time()
    )
    
    return(list(
      success = TRUE,
      analysis = analysis
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Visualization plot analysis failed:", e$message)
    ))
  })
}

#' Reconstruct base R QQ plot command possibly spanning multiple lines
reconstruct_qq_command <- function(history_lines, start_line) {
  tryCatch({
    line <- history_lines[start_line]
    command_lines <- c()
    
    # If starting at qqline, look up for qqnorm
    if (grepl("qqline\\(", line)) {
      # search backwards for nearest qqnorm line
      for (i in (start_line - 1):1) {
        if (grepl("qqnorm\\(", history_lines[i])) {
          command_lines <- c(history_lines[i], line)
          break
        } else if (nzchar(trimws(history_lines[i]))) {
          # Stop if we hit another non-empty unrelated command
          next
        }
      }
      if (length(command_lines) == 0) {
        # Fallback to just the qqline if no qqnorm found
        command_lines <- c(line)
      }
    } else if (grepl("qqnorm\\(", line)) {
      # Starting at qqnorm: include immediate following qqline if present
      command_lines <- c(line)
      if (start_line + 1 <= length(history_lines)) {
        next_line <- history_lines[start_line + 1]
        if (grepl("qqline\\(", next_line)) {
          command_lines <- c(command_lines, next_line)
        }
      }
    } else {
      # Unknown line, just return as is
      command_lines <- c(line)
    }
    
    full_command <- paste(command_lines, collapse = " ")
    return(list(command = full_command, line_number = start_line, found = TRUE))
  }, error = function(e) {
    return(list(command = history_lines[start_line], line_number = start_line, found = TRUE))
  })
}

#' Reconstruct multi-line base R plot command (plot, hist, boxplot, barplot)
reconstruct_base_plot_command <- function(history_lines, start_line, plot_cmd) {
  tryCatch({
    # Start with the line that contains the plot command
    command_lines <- c(history_lines[start_line])
    current_line <- start_line
    
    # Check if this line has a complete command (balanced parentheses)
    current_command <- paste(command_lines, collapse = " ")
    
    # Count parentheses to see if command is complete
    open_parens <- length(gregexpr("\\(", current_command)[[1]])
    if (open_parens == 1 && gregexpr("\\(", current_command)[[1]][1] == -1) open_parens <- 0
    close_parens <- length(gregexpr("\\)", current_command)[[1]])
    if (close_parens == 1 && gregexpr("\\)", current_command)[[1]][1] == -1) close_parens <- 0
    
    # If parentheses are balanced, command is complete on one line
    if (open_parens == close_parens && open_parens > 0) {
      return(list(
        command = current_command,
        line_number = start_line,
        found = TRUE
      ))
    }
    
    # If parentheses are not balanced, continue reading next lines
    max_lines_to_check <- min(10, length(history_lines) - start_line)
    
    for (i in 1:max_lines_to_check) {
      next_line_idx <- start_line + i
      if (next_line_idx > length(history_lines)) break
      
      next_line <- history_lines[next_line_idx]
      
      # Skip empty lines
      if (nzchar(trimws(next_line))) {
        command_lines <- c(command_lines, next_line)
        current_command <- paste(command_lines, collapse = " ")
        
        # Recount parentheses
        open_parens <- length(gregexpr("\\(", current_command)[[1]])
        if (open_parens == 1 && gregexpr("\\(", current_command)[[1]][1] == -1) open_parens <- 0
        close_parens <- length(gregexpr("\\)", current_command)[[1]])
        if (close_parens == 1 && gregexpr("\\)", current_command)[[1]][1] == -1) close_parens <- 0
        
        # If balanced, we found the complete command
        if (open_parens == close_parens && open_parens > 0) {
          return(list(
            command = current_command,
            line_number = start_line,
            found = TRUE
          ))
        }
        
        # If we have more closing than opening, something's wrong - stop
        if (close_parens > open_parens) {
          break
        }
        
        # If this line starts a new command (contains another function call), stop
        if (grepl("^[a-zA-Z_][a-zA-Z0-9_.]*\\s*\\(", trimws(next_line)) && 
            !grepl(paste0("^", plot_cmd), trimws(next_line))) {
          break
        }
      }
    }
    
    # If we get here, return what we have (might be incomplete)
    final_command <- paste(command_lines, collapse = " ")
    return(list(
      command = final_command,
      line_number = start_line,
      found = TRUE
    ))
    
  }, error = function(e) {
    # Fallback to single line
    return(list(
      command = history_lines[start_line],
      line_number = start_line,
      found = TRUE
    ))
  })
}

#' Package onLoad hook - automatically sets up and starts Rgent
.onLoad <- function(libname, pkgname) {
  cat("DEBUG .onLoad: Package loading, libname =", libname, ", pkgname =", pkgname, "\n")
  cat("DEBUG .onLoad: interactive() =", interactive(), "\n")
  tryCatch({
    # Set up .Rprofile for future auto-start (one-time setup)
    cat("DEBUG .onLoad: Calling setup_rprofile_auto_start()...\n")
    setup_rprofile_auto_start()
    
    # Auto-start Rgent if in RStudio
    # Don't rely on interactive() - check RStudio directly
    cat("DEBUG .onLoad: Checking for rstudioapi...\n")
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      cat("DEBUG .onLoad: rstudioapi available, checking if RStudio is available...\n")
      # Try to check if RStudio is available (might not be ready yet during startup)
      rstudio_available <- tryCatch({
        rstudioapi::isAvailable()
      }, error = function(e) {
        cat("DEBUG .onLoad: Error checking RStudio availability:", e$message, "\n")
        FALSE
      })
      
      cat("DEBUG .onLoad: rstudioapi::isAvailable() =", rstudio_available, "\n")
      
      if (rstudio_available) {
        cat("DEBUG .onLoad: RStudio is available, setting up task callback for auto-start...\n")
        # Use a task callback to delay the start slightly (gives RStudio time to fully initialize)
        start_callback <- function(expr, value, ok, visible) {
          cat("DEBUG .onLoad task callback: Executing, checking RStudio availability...\n")
          tryCatch({
            if (rstudioapi::isAvailable()) {
              cat("DEBUG .onLoad task callback: RStudio available, calling auto_start_rgent()...\n")
              auto_start_rgent()
            } else {
              cat("DEBUG .onLoad task callback: RStudio not available yet\n")
            }
          }, error = function(e) {
            cat("DEBUG .onLoad task callback: Error:", e$message, "\n")
          })
          # Only run once
          tryCatch({
            removeTaskCallback("rgent_auto_start")
          }, error = function(e) {
            # Ignore if already removed
          })
          cat("DEBUG .onLoad task callback: Removed callback, returning FALSE\n")
          return(FALSE)
        }
        addTaskCallback(start_callback, name = "rgent_auto_start")
        cat("DEBUG .onLoad: Task callback added\n")
      } else {
        cat("DEBUG .onLoad: RStudio API not available yet (might be starting up), setting up delayed check...\n")
        # RStudio might not be ready yet - keep trying until it's available
        # Create an environment to hold the attempt counter that persists across callback invocations
        callback_env <- new.env()
        callback_env$attempt_count <- 0
        max_attempts <- 100  # Try up to 100 times (should be enough for RStudio to start)
        
        delayed_callback <- function(expr, value, ok, visible) {
          callback_env$attempt_count <- callback_env$attempt_count + 1
          cat("DEBUG .onLoad delayed callback: Attempt", callback_env$attempt_count, "- Checking RStudio availability...\n")
          tryCatch({
            if (rstudioapi::isAvailable()) {
              cat("DEBUG .onLoad delayed callback: RStudio now available, calling auto_start_rgent()...\n")
              auto_start_rgent()
              # Success - remove callback
              tryCatch({
                removeTaskCallback("rgent_delayed_start")
              }, error = function(e) {
                # Ignore if already removed
              })
              cat("DEBUG .onLoad delayed callback: Removed callback after successful start\n")
              return(FALSE)
            } else {
              cat("DEBUG .onLoad delayed callback: RStudio still not available (attempt", callback_env$attempt_count, ")\n")
              # Keep trying if we haven't exceeded max attempts
              if (callback_env$attempt_count >= max_attempts) {
                cat("DEBUG .onLoad delayed callback: Max attempts reached, giving up\n")
                tryCatch({
                  removeTaskCallback("rgent_delayed_start")
                }, error = function(e) {
                  # Ignore if already removed
                })
                return(FALSE)
              }
              return(TRUE)  # Keep callback active to try again
            }
          }, error = function(e) {
            cat("DEBUG .onLoad delayed callback: Error:", e$message, "\n")
            # Keep trying unless we've exceeded max attempts
            if (callback_env$attempt_count >= max_attempts) {
              tryCatch({
                removeTaskCallback("rgent_delayed_start")
              }, error = function(e) {
                # Ignore if already removed
              })
              return(FALSE)
            }
            return(TRUE)  # Keep trying
          })
        }
        addTaskCallback(delayed_callback, name = "rgent_delayed_start")
        cat("DEBUG .onLoad: Delayed callback added (will keep checking until RStudio is ready, max", max_attempts, "attempts)\n")
        
        # Also add a simple callback that just executes immediately
        # This will fire on the next R command, which should happen soon after startup
        simple_callback <- function(expr, value, ok, visible) {
          cat("DEBUG .onLoad simple callback: Fired, checking RStudio...\n")
          tryCatch({
            if (rstudioapi::isAvailable()) {
              cat("DEBUG .onLoad simple callback: RStudio available, starting Rgent...\n")
              auto_start_rgent()
              # Remove this callback once successful
              tryCatch({
                removeTaskCallback("rgent_simple_start")
              }, error = function(e) {})
              return(FALSE)
            }
          }, error = function(e) {
            cat("DEBUG .onLoad simple callback: Error:", e$message, "\n")
          })
          return(TRUE)  # Keep it active for a few tries
        }
        addTaskCallback(simple_callback, name = "rgent_simple_start")
        cat("DEBUG .onLoad: Simple callback added (will fire on next command)\n")
        
        # Trigger a dummy command to fire the callbacks immediately if possible
        # We'll try to execute a simple expression to trigger callbacks
        tryCatch({
          # This won't work during .onLoad, but we can try
          eval(quote(NULL), envir = .GlobalEnv)
        }, error = function(e) {
          # Expected to fail, ignore
        })
      }
    } else {
      cat("DEBUG .onLoad: rstudioapi package not available\n")
    }
  }, error = function(e) {
    cat("DEBUG .onLoad: Error occurred:", e$message, "\n")
    cat("DEBUG .onLoad: Error stack trace:", toString(e), "\n")
    invisible(NULL)
  })
  cat("DEBUG .onLoad: Finished\n")
}