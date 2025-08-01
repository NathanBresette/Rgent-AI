# Plumber API for RStudio integration
# This provides local endpoints for the HTML interface to interact with RStudio

library(plumber)
library(rstudioapi)
library(jsonlite)

# Configure jsonlite to auto-unbox single values to prevent array wrapping
options(jsonlite.auto_unbox = TRUE)

# Global variables for user session management
user_session_info <- list(
  access_code = NULL,
  conversation_id = NULL
)

# Global variable to track previous workspace state
previous_workspace_state <- list(
  objects = list(),
  timestamp = NULL
)



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
    
    # Get workspace changes since last context capture
    workspace_changes <- tryCatch({
      current_objects <- ls(envir = .GlobalEnv)
      current_obj_dict <- list()
      
      # Get current state of all objects
      for (obj_name in current_objects) {
        tryCatch({
          obj <- get(obj_name, envir = .GlobalEnv)
          
          # Create object info as a dictionary
          obj_info <- list(
            class = paste(class(obj), collapse = ", "),
            rows = if (is.data.frame(obj)) nrow(obj) else length(obj),
            columns = if (is.data.frame(obj)) ncol(obj) else NULL,
            preview = paste(utils::capture.output(print(utils::head(obj, 3))), collapse = "\n")
          )
          
          current_obj_dict[[obj_name]] <- obj_info
        }, error = function(e) {
          current_obj_dict[[obj_name]] <<- list(
            class = "error", 
            rows = NULL, 
            columns = NULL, 
            preview = paste("Error reading object:", e$message)
          )
        })
      }
      
      # Find new or modified objects
      new_or_modified <- list()
      for (obj_name in names(current_obj_dict)) {
        if (!obj_name %in% names(previous_workspace_state$objects) || 
            !identical(current_obj_dict[[obj_name]], previous_workspace_state$objects[[obj_name]])) {
          new_or_modified[[obj_name]] <- current_obj_dict[[obj_name]]
        }
      }
      
      # Update the previous state
      previous_workspace_state$objects <<- current_obj_dict
      previous_workspace_state$timestamp <<- Sys.time()
      
      # Return only the changes
      new_or_modified
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
      workspace_changes = workspace_changes,
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
