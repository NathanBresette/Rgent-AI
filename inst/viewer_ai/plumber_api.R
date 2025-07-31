# Plumber API for RStudio integration
# This provides local endpoints for the HTML interface to interact with RStudio

library(plumber)
library(rstudioapi)
library(jsonlite)

# Configure jsonlite to auto-unbox single values to prevent array wrapping
options(jsonlite.auto_unbox = TRUE)

# Global variables for continuous error monitoring
last_error_check <- Sys.time()
error_monitoring_active <- FALSE
last_error_message <- ""
old_error_handler <- NULL

# Global variables for user session management
user_session_info <- list(
  access_code = NULL,
  conversation_id = NULL
)

# Global variable to store the last error
last_error <- ""

# Simple error capture function (simplified)
capture_error <- function() {
  cat("=== CAPTURE_ERROR FUNCTION START ===\n")
  
  # Check if .Last.error exists
  if (exists(".Last.error", envir = .GlobalEnv)) {
    err <- get(".Last.error", envir = .GlobalEnv)
    msg <- conditionMessage(err)
    if (nchar(msg) > 0) {
      last_error <<- msg
      cat("Error captured from .Last.error:", msg, "\n")
    } else {
      cat("No error found in .Last.error\n")
    }
  } else {
    cat("No .Last.error found\n")
  }
  
  cat("=== CAPTURE_ERROR FUNCTION END ===\n")
}



#* @get /
#* @serializer html
function() {
  static_dir <- system.file("public", package = "rstudioai")
  html_file <- file.path(static_dir, "index.html")
  if (file.exists(html_file)) {
    paste(readLines(html_file), collapse = "\n")
  } else {
    "<h1>HTML file not found</h1>"
  }
}

#* @get /test
#* @serializer json
function() {
  list(
    success = TRUE,
    message = "Plumber server is working!",
    timestamp = Sys.time()
  )
}

#* @get /last-error
#* @serializer json
function() {
  tryCatch({
    cat("=== GET LAST ERROR CALLED ===\n")
    
    current_error <- ""
    
    # First try to read from the error file (most reliable for cross-process communication)
    error_file <- file.path(tempdir(), "rstudioai_error.rds")
    cat("Checking for error file:", error_file, "\n")
    cat("File exists:", file.exists(error_file), "\n")
    
    if (file.exists(error_file)) {
      tryCatch({
        error_data <- readRDS(error_file)
        cat("Error data from file:", str(error_data), "\n")
        if (!is.null(error_data$error_message) && nchar(error_data$error_message) > 0) {
          current_error <- error_data$error_message
          cat("Current error from file:", current_error, "\n")
        } else {
          cat("No error message in file data\n")
        }
      }, error = function(e) {
        cat("Error reading error file:", e$message, "\n")
      })
    } else {
      cat("Error file does not exist\n")
    }
    
    # If no error from file, try geterrmessage() in this process
    if (nchar(current_error) == 0) {
      tryCatch({
        error_msg <- geterrmessage()
        cat("geterrmessage() returned:", error_msg, "\n")
        if (nchar(error_msg) > 0) {
          current_error <- error_msg
          cat("Current error from geterrmessage():", current_error, "\n")
        }
      }, error = function(e) {
        cat("Error in geterrmessage():", e$message, "\n")
      })
    }
    
    # If still no error, check .Last.error in this process
    if (nchar(current_error) == 0) {
      tryCatch({
        if (exists(".Last.error", envir = .GlobalEnv)) {
          err <- get(".Last.error", envir = .GlobalEnv)
          msg <- conditionMessage(err)
          cat("Last.error in this process:", msg, "\n")
          if (nchar(msg) > 0) {
            current_error <- msg
            cat("Current error from .Last.error:", current_error, "\n")
          }
        } else {
          cat("No .Last.error in this process\n")
        }
      }, error = function(e) {
        cat("Error accessing .Last.error:", e$message, "\n")
      })
    }
    
    cat("Final error to return:", current_error, "\n")
    
    # Defensive return - ensure we don't trigger errors in the return itself
    result <- list(
      success = TRUE,
      error = ifelse(nchar(current_error) > 0, current_error, ""),
      has_error = nchar(current_error) > 0
    )
    
    # Validate the result before returning
    if (!is.list(result) || !all(c("success", "error", "has_error") %in% names(result))) {
      cat("Invalid result structure, returning safe fallback\n")
      result <- list(
        success = TRUE,
        error = "",
        has_error = FALSE
      )
    }
    
    cat("Returning result:", str(result), "\n")
    result
    
  }, error = function(e) {
    cat("Error in last-error endpoint:", e$message, "\n")
    list(
      success = FALSE,
      error = paste("Error getting last error:", e$message),
      has_error = FALSE
    )
  })
}

#* @post /set-test-error
#* @serializer json
function(req) {
  tryCatch({
    test_error <- req$body$error
    if (is.null(test_error)) {
      test_error <- "Test error message"
    }
    
    # Set the error in multiple places for testing
    last_error <<- test_error
    
    # Save to .Last.error
    e <- simpleError(test_error)
    assign(".Last.error", e, envir = .GlobalEnv)
    
    # Save to main session error
    assign("main_session_error", test_error, envir = .GlobalEnv)
    
    # Save to error file
    error_data <- list(
      error_message = test_error,
      timestamp = Sys.time(),
      session_id = Sys.getpid()
    )
    saveRDS(error_data, file.path(tempdir(), "rstudioai_error.rds"))
    
    cat("Set test error to:", test_error, "\n")
    cat("Error saved to .Last.error, main_session_error, and error file\n")
    
    list(
      success = TRUE,
      message = paste("Test error set to:", test_error)
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("Error setting test error:", e$message)
    )
  })
}

#* @get /debug-error-capture
#* @serializer json
function() {
  tryCatch({
    cat("=== DEBUG ERROR CAPTURE ===\n")
    
    debug_info <- list(
      geterrmessage = geterrmessage(),
      has_last_error = exists(".Last.error", envir = .GlobalEnv),
      has_main_session_error = exists("main_session_error", envir = .GlobalEnv),
      error_file_exists = file.exists(file.path(tempdir(), "rstudioai_error.rds")),
      error_monitoring_active = error_monitoring_active,
      session_pid = Sys.getpid(),
      temp_dir = tempdir()
    )
    
    # Get .Last.error if it exists
    if (debug_info$has_last_error) {
      err <- get(".Last.error", envir = .GlobalEnv)
      debug_info$last_error_message <- conditionMessage(err)
    }
    
    # Get main session error if it exists
    if (debug_info$has_main_session_error) {
      debug_info$main_session_error_message <- get("main_session_error", envir = .GlobalEnv)
    }
    
    # Get error file content if it exists
    if (debug_info$error_file_exists) {
      tryCatch({
        error_data <- readRDS(file.path(tempdir(), "rstudioai_error.rds"))
        debug_info$error_file_content <- error_data
      }, error = function(e) {
        debug_info$error_file_read_error <- e$message
      })
    }
    
    cat("Debug info:", str(debug_info), "\n")
    
    list(
      success = TRUE,
      debug_info = debug_info
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("Error in debug endpoint:", e$message)
    )
  })
}

#* @get /test-error-file
#* @serializer json
function() {
  tryCatch({
    error_file <- file.path(tempdir(), "rstudioai_error.rds")
    cat("Testing error file access\n")
    cat("File exists:", file.exists(error_file), "\n")
    
    if (file.exists(error_file)) {
      error_data <- readRDS(error_file)
      cat("Error data:", str(error_data), "\n")
      
      list(
        success = TRUE,
        file_exists = TRUE,
        error_message = error_data$error_message,
        timestamp = as.character(error_data$timestamp),
        session_id = error_data$session_id
      )
    } else {
      list(
        success = TRUE,
        file_exists = FALSE,
        error_message = NULL,
        timestamp = NULL,
        session_id = NULL
      )
    }
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("Error testing file:", e$message)
    )
  })
}

#* @post /clear-error
#* @serializer json
function() {
  tryCatch({
    last_error <<- ""
    cat("Cleared stored error\n")
    
    list(
      success = TRUE,
      message = "Stored error cleared"
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("Error clearing error:", e$message)
    )
  })
}

#* @post /capture-error
#* @serializer json
function() {
  tryCatch({
    cat("=== CAPTURE ERROR CALLED ===\n")
    cat("Current last_error before capture:", last_error, "\n")
    
    capture_error()
    
    cat("Last error after capture:", last_error, "\n")
    cat("Error length:", nchar(last_error), "\n")
    
    list(
      success = TRUE,
      error = last_error,
      has_error = nchar(last_error) > 0
    )
  }, error = function(e) {
    cat("Error in capture-error endpoint:", e$message, "\n")
    list(
      success = FALSE,
      error = paste("Error capturing error:", e$message),
      has_error = FALSE
    )
  })
}

#* @post /capture-current-error
#* @serializer json
function() {
  tryCatch({
    cat("=== CAPTURE CURRENT ERROR CALLED ===\n")
    
    # Clear old error data first
    cat("Clearing old error data...\n")
    if (exists(".Last.error", envir = .GlobalEnv)) {
      rm(".Last.error", envir = .GlobalEnv)
    }
    if (exists("main_session_error", envir = .GlobalEnv)) {
      rm("main_session_error", envir = .GlobalEnv)
    }
    
    # Clear error file
    error_file <- file.path(tempdir(), "rstudioai_error.rds")
    if (file.exists(error_file)) {
      unlink(error_file)
      cat("Cleared old error file\n")
    }
    
    # Try to get the current error from geterrmessage()
    current_error <- ""
    
    tryCatch({
      error_msg <- geterrmessage()
      if (nchar(error_msg) > 0) {
        current_error <- error_msg
        cat("Current error from geterrmessage():", current_error, "\n")
      }
    }, error = function(e) {
      cat("Error in geterrmessage():", e$message, "\n")
    })
    
    # If no error from geterrmessage(), check if we have a stored error from the main session
    if (nchar(current_error) == 0) {
      tryCatch({
        if (exists("main_session_error", envir = .GlobalEnv)) {
          current_error <- get("main_session_error", envir = .GlobalEnv)
          cat("Current error from main session:", current_error, "\n")
        }
      }, error = function(e) {
        cat("Error accessing main session error:", e$message, "\n")
      })
    }
    
    # If still no error, check the error file
    if (nchar(current_error) == 0) {
      tryCatch({
        error_file <- file.path(tempdir(), "rstudioai_error.rds")
        if (file.exists(error_file)) {
          error_data <- readRDS(error_file)
          if (!is.null(error_data$error_message) && nchar(error_data$error_message) > 0) {
            current_error <- error_data$error_message
            cat("Current error from file:", current_error, "\n")
          }
        }
      }, error = function(e) {
        cat("Error reading error file:", e$message, "\n")
      })
    }
    
    # If we found an error, save it to .Last.error
    if (nchar(current_error) > 0) {
      e <- simpleError(current_error)
      assign(".Last.error", e, envir = .GlobalEnv)
      cat("Error saved to .Last.error:", current_error, "\n")
    }
    
    list(
      success = TRUE,
      has_error = nchar(current_error) > 0,
      message = ifelse(nchar(current_error) > 0, 
                      paste("Error captured:", current_error), 
                      "No current error found. Try running a command that produces an error first, then click the error button again.")
    )
  }, error = function(e) {
    list(
      success = FALSE,
      has_error = FALSE,
      message = paste("Error capturing current error:", e$message)
    )
  })
}

#* @post /clear-errors
#* @serializer json
function() {
  tryCatch({
    cat("=== CLEAR ERRORS CALLED ===\n")
    
    # Clear all error storage locations
    if (exists(".Last.error", envir = .GlobalEnv)) {
      rm(".Last.error", envir = .GlobalEnv)
      cat("Cleared .Last.error\n")
    }
    
    if (exists("main_session_error", envir = .GlobalEnv)) {
      rm("main_session_error", envir = .GlobalEnv)
      cat("Cleared main_session_error\n")
    }
    
    # Clear error file
    error_file <- file.path(tempdir(), "rstudioai_error.rds")
    if (file.exists(error_file)) {
      unlink(error_file)
      cat("Cleared error file\n")
    }
    
    # Clear the global last_error variable
    last_error <<- ""
    
    list(
      success = TRUE,
      message = "All error data cleared successfully"
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("Error clearing errors:", e$message)
    )
  })
}

#* @post /send-error-to-claude
#* @serializer json
function(req) {
  tryCatch({
    error_msg <- req$body$error
    if (is.null(error_msg) || nchar(error_msg) == 0) {
      return(list(success = FALSE, message = "No error message provided"))
    }
    
    # Send to Claude via the main backend
    backend_url <- "https://rgent.onrender.com"
    response <- httr::POST(
      url = sprintf("%s/api/chat", backend_url),
      httr::content_type("application/json"),
      body = jsonlite::toJSON(list(
        access_code = user_session_info$access_code || "AUTO_ERROR",
        prompt = paste0("⚠️ The user just encountered this error in their R console:\n\n", error_msg, "\n\nCan you help fix it?"),
        context_data = list(
          console_history = character(0),
          workspace_objects = list(), # Will be captured by the main backend
          environment_info = list(
            r_version = as.character(R.version.string),
            working_directory = as.character(getwd())
          )
        ),
        context_type = "rstudio",
        conversation_id = user_session_info$conversation_id || paste0("error_", as.numeric(Sys.time())),
        metadata = list(
          source = "console_error", 
          timestamp = as.character(Sys.time())
        )
      ), auto_unbox = TRUE)
    )
    
    if (httr::status_code(response) == 200) {
      list(success = TRUE, message = "Error sent to Claude successfully!")
    } else {
      list(success = FALSE, message = sprintf("Failed to send error: %d", httr::status_code(response)))
    }
  }, error = function(e) {
    list(success = FALSE, message = paste("Error sending to Claude:", e$message))
  })
}

#* @get /monitor-errors
#* @serializer json
function() {
  tryCatch({
    # Check for console errors continuously
    console_output <- capture.output({
      # Simulate checking for recent errors
      cat("Checking for console errors...\n")
    })
    
    # Look for error patterns
    error_detected <- FALSE
    error_message <- ""
    
    # Check for common R error patterns
    if (length(console_output) > 0) {
      for (line in console_output) {
        if (grepl("Error:", line, ignore.case = TRUE) ||
            grepl("Error in", line, ignore.case = TRUE) ||
            grepl("Warning:", line, ignore.case = TRUE) ||
            grepl("object not found", line, ignore.case = TRUE) ||
            grepl("could not find function", line, ignore.case = TRUE) ||
            grepl("unexpected", line, ignore.case = TRUE)) {
          error_detected <- TRUE
          error_message <- paste(error_message, line, sep = "\n")
        }
      }
    }
    
    # Return error status
    list(
      error_detected = error_detected,
      error_message = error_message,
      timestamp = Sys.time(),
      console_output = console_output
    )
  }, error = function(e) {
    list(
      error_detected = FALSE,
      error_message = paste("Error in monitoring:", e$message),
      timestamp = Sys.time(),
      console_output = character(0)
    )
  })
}

#* @post /start-error-monitoring
#* @serializer json
function() {
  tryCatch({
    # Start R's built-in error monitoring
    error_monitoring_active <<- TRUE
    
    # Store the current error handler
    old_error_handler <<- getOption("error")
    
    # Set up immediate error detection
    options(error = function() {
      tryCatch({
        # Get the error message immediately
        error_msg <- geterrmessage()
        
        # Only send if it's a new error (not a duplicate)
        if (error_msg != last_error_message) {
          last_error_message <<- error_msg
          
          # Print to console so user knows what happened
          message("⚠️ Error intercepted and sent to Claude:")
          message(error_msg)
          
          # Send to Claude via backend
          tryCatch({
            response <- httr::POST(
              url = "https://rgent.onrender.com/api/chat",
              httr::content_type("application/json"),
              body = jsonlite::toJSON(list(
                access_code = "AUTO_ERROR",
                prompt = paste("I encountered an error in my R console:", error_msg, "\n\nCan you help me understand and fix this error?", sep = ""),
                context_data = list(
                  console_history = character(0),  # No need to capture console
                  workspace_objects = if (exists("captured_workspace_objects", envir = .GlobalEnv)) captured_workspace_objects else list(),
                  environment_info = list(
                    r_version = as.character(R.version.string),
                    working_directory = as.character(getwd())
                  )
                ),
                context_type = "rstudio",
                conversation_id = paste0("auto_error_", as.numeric(Sys.time()))
              ), auto_unbox = TRUE)
            )
            
            if (httr::status_code(response) == 200) {
              message("✅ Error sent to Claude successfully!")
            } else {
              message("❌ Failed to send error to Claude")
            }
          }, error = function(e) {
            message("❌ Error sending to Claude:", e$message)
          })
        }
        
        # Call the original error handler if it exists
        if (!is.null(old_error_handler)) {
          old_error_handler()
        }
      }, error = function(e) {
        # If our error handler fails, restore original and call it
        options(error = old_error_handler)
        if (!is.null(old_error_handler)) {
          old_error_handler()
        }
      })
    })
    
    list(
      success = TRUE,
      message = "Immediate error monitoring started",
      timestamp = Sys.time()
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("Failed to start monitoring:", e$message),
      timestamp = Sys.time()
    )
  })
}

#* @post /stop-error-monitoring
#* @serializer json
function() {
  tryCatch({
    # Stop error monitoring and restore original error handler
    error_monitoring_active <<- FALSE
    
    # Restore the original error handler
    if (!is.null(old_error_handler)) {
      options(error = old_error_handler)
      old_error_handler <<- NULL
    }
    
    list(
      success = TRUE,
      message = "Error monitoring stopped and original error handler restored",
      timestamp = Sys.time()
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("Failed to stop monitoring:", e$message),
      timestamp = Sys.time()
    )
  })
}

#* @get /context
#* @serializer json
function() {
  tryCatch({
    # Get document content as a simple string
    document_content <- ""
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      tryCatch({
        ctx <- rstudioapi::getActiveDocumentContext()
        document_content <- paste(ctx$contents, collapse = "\n")
      }, error = function(e) {
        document_content <- ""
      })
    }
    
    # Capture console output in real-time
    console_history <- tryCatch({
      # Capture the output of the last few commands
      console_output <- capture.output({
        # Try to get recent console activity
        cat("=== CONSOLE CAPTURE ===\n")
        
        # Show current workspace objects
        cat("Current workspace objects:\n")
        if (length(ls()) > 0) {
          for (obj in ls()) {
            tryCatch({
              obj_value <- get(obj)
              obj_class <- class(obj_value)
              obj_length <- if (is.data.frame(obj_value)) nrow(obj_value) else length(obj_value)
              cat(sprintf("  %s: %s (length: %d)\n", obj, paste(obj_class, collapse = ", "), obj_length))
            }, error = function(e) {
              cat(sprintf("  %s: error reading object\n", obj))
            })
          }
        } else {
          cat("  No objects in workspace\n")
        }
        
        # Show recent commands (if available)
        cat("\nRecent activity:\n")
        cat("  Console capture active\n")
        cat("  Ready for R commands\n")
      })
      
      # Return the captured console output
      if (length(console_output) > 0) {
        console_output
      } else {
        character(0)
      }
    }, error = function(e) {
      character(0)  # Return empty vector on error
    })
    
    # Get workspace objects from captured data (passed from main R session)
    workspace_objects <- tryCatch({
      if (exists("captured_workspace_objects", envir = .GlobalEnv)) {
        captured_workspace_objects
      } else {
        # Fallback: try to get objects from current environment
        objects <- ls(envir = .GlobalEnv)
        if (length(objects) > 0) {
          obj_dict <- list()
          for (obj_name in objects) {
            tryCatch({
              obj <- get(obj_name, envir = .GlobalEnv)
              
              # Create object info as a dictionary
              obj_info <- list(
                class = paste(class(obj), collapse = ", "),
                rows = if (is.data.frame(obj)) nrow(obj) else length(obj),
                columns = if (is.data.frame(obj)) ncol(obj) else NULL,
                preview = paste(utils::capture.output(print(utils::head(obj, 3))), collapse = "\n")
              )
              
              # Add the object to the dictionary with its name as the key
              obj_dict[[obj_name]] <- obj_info
            }, error = function(e) {
              obj_dict[[obj_name]] <<- list(
                class = "error", 
                rows = NULL, 
                columns = NULL, 
                preview = paste("Error reading object:", e$message)
              )
            })
          }
          obj_dict
        } else {
          list()  # Return empty list instead of error
        }
      }
    }, error = function(e) {
      list()  # Return empty list on error
    })
    
    # Get environment information as a simple dictionary
    environment_info <- tryCatch({
      # Get loaded packages as a simple string
      loaded_packages <- tryCatch({
        pkgs <- names(sessionInfo()$otherPkgs)
        if (length(pkgs) > 0) {
          pkg_versions <- sapply(pkgs, function(pkg) {
            tryCatch({
              version <- packageVersion(pkg)
              paste(pkg, "v", version)
            }, error = function(e) pkg)
          })
          paste(pkg_versions, collapse = ", ")
        } else {
          "No packages loaded"
        }
      }, error = function(e) {
        "Error reading packages"
      })
      
      list(
        r_version = as.character(R.version.string),
        platform = as.character(R.version$platform),
        working_directory = as.character(getwd()),
        packages = loaded_packages
      )
    }, error = function(e) {
      list(
        r_version = "Unknown",
        platform = "Unknown", 
        working_directory = "Unknown",
        packages = "Unknown"
      )
    })
    
    # Get custom functions as a clean character vector
    custom_functions <- tryCatch({
      objects <- ls(envir = .GlobalEnv)
      funcs <- objects[sapply(objects, function(obj) {
        tryCatch({
          val <- get(obj, envir = .GlobalEnv)
          is.function(val)
        }, error = function(e) FALSE)
      })]
      # Convert to simple character vector
      as.character(funcs)
    }, error = function(e) {
      character(0)  # Return empty vector on error
    })
    
    # Get plot history as a clean character vector
    plot_history <- tryCatch({
      plot_objects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(obj) {
        tryCatch({
          val <- get(obj, envir = .GlobalEnv)
          inherits(val, c("ggplot", "trellis", "plot"))
        }, error = function(e) FALSE)
      })]
      # Convert to simple character vector
      as.character(plot_objects)
    }, error = function(e) {
      character(0)  # Return empty vector on error
    })
    
    # Get error history as a simple list of strings
    error_history <- tryCatch({
      errors <- c()
      if (exists(".Last.error", envir = .GlobalEnv)) {
        last_error <- get(".Last.error", envir = .GlobalEnv)
        errors <- c(errors, paste("Last Error:", paste(capture.output(print(last_error)), collapse = " ")))
      }
      if (exists("last.warning", envir = .GlobalEnv)) {
        last_warnings <- get("last.warning", envir = .GlobalEnv)
        if (length(last_warnings) > 0) {
          errors <- c(errors, paste("Warnings:", paste(capture.output(print(last_warnings)), collapse = " ")))
        }
      }
      if (length(errors) == 0) {
        character(0)  # Return empty vector instead of error message
      } else {
        errors
      }
    }, error = function(e) {
      character(0)  # Return empty vector on error
    })
    
    # Create the result list with explicit handling to prevent array wrapping
    result <- list(
      document_content = if(length(document_content) == 1) document_content[[1]] else document_content,
      console_history = console_history,
      workspace_objects = workspace_objects,
      environment_info = environment_info,
      custom_functions = custom_functions,
      plot_history = plot_history,
      error_history = error_history,
      timestamp = as.character(Sys.time()),
      source = "rstudio_plumber_context"
    )
    
    # Convert to JSON and back to ensure proper formatting
    json_string <- jsonlite::toJSON(result, auto_unbox = TRUE, pretty = FALSE)
    result <- jsonlite::fromJSON(json_string)
    
    result
    
  }, error = function(e) {
    list(
      error = paste("Error capturing context:", e$message),
      timestamp = as.character(Sys.time()),
      source = "rstudio_plumber_context"
    )
  })
}

#* @post /insert_code
#* @serializer json
function(req) {
  code <- req$body$code
  tryCatch({
    # Get current document context
    ctx <- rstudioapi::getActiveDocumentContext()
    active_doc <- ctx$path
    active_doc_name <- basename(active_doc)
    
    # Insert the code
    rstudioapi::insertText(code)
    
    list(
      success = TRUE, 
      message = paste("Code inserted successfully into:", active_doc_name),
      document = active_doc_name,
      path = active_doc
    )
  }, error = function(e) {
    list(
      success = FALSE, 
      message = paste("Error inserting code:", e$message),
      document = "unknown",
      path = "unknown"
    )
  })
}

#* @get /health
#* @serializer json
function() {
  list(status = "healthy", service = "rstudio-plumber-api")
}

#* @post /api/chat
#* @serializer json
function(req) {
  tryCatch({
    # Forward the request to the backend
    backend_url <- "https://rgent.onrender.com"
    
    # Forward the request to the backend
    response <- httr::POST(
      url = sprintf("%s/api/chat", backend_url),
      httr::content_type("application/json"),
      body = req$postBody
    )
    
    # Return the response from the backend
    if (httr::status_code(response) == 200) {
      httr::content(response, "text")
    } else {
      list(
        error = TRUE,
        message = sprintf("Backend error: %d", httr::status_code(response)),
        status_code = httr::status_code(response)
      )
    }
  }, error = function(e) {
    list(
      error = TRUE,
      message = paste("Error forwarding to backend:", e$message)
    )
  })
}

#* @post /set-user-session
#* @serializer json
function(req) {
  tryCatch({
    # Store user session info for error monitoring
    user_session_info <<- list(
      access_code = req$body$access_code,
      conversation_id = req$body$conversation_id
    )
    
    # Also set environment variables for the error handler
    Sys.setenv("RSTUDIOAI_ACCESS_CODE" = req$body$access_code)
    Sys.setenv("RSTUDIOAI_CONVERSATION_ID" = req$body$conversation_id)
    
    list(
      success = TRUE,
      message = "User session info stored for error monitoring",
      access_code = req$body$access_code,
      conversation_id = req$body$conversation_id
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("Error storing user session:", e$message)
    )
  })
}

#* @get /get-user-session
#* @serializer json
function() {
  tryCatch({
    list(
      success = TRUE,
      access_code = user_session_info$access_code,
      conversation_id = user_session_info$conversation_id
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("Error getting user session:", e$message)
    )
  })
}
