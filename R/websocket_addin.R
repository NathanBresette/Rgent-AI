# Debug wrapper functions to track down invalid 'envir' argument error
safe_ls <- function(envir) {
  cat('DEBUG: Calling ls with envir of class:', class(envir), 'type:', typeof(envir), '\n')
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
    cat('DEBUG: Getting object:', name, '\n')
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
  cat("DEBUG: global_env initialized with proper environment structure\n")
}

# Initialize conversation history
if (!exists("conversation_history") || !is.list(conversation_history)) {
  .GlobalEnv$conversation_history <- list()
  cat("DEBUG: conversation_history initialized\n")
}

# Helper function to get current access code
get_current_access_code <- function() {
  if(exists("current_access_code", envir = .GlobalEnv)) {
    .GlobalEnv$current_access_code
  } else {
    stop("No access code set. Please validate your access code in the interface first.")
  }
}

#' Launch WebSocket-based AI Chat Addin
#' @export
launch_websocket_chat <- function() {
  # Check if RStudio API is available
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("RStudio API is not available. Please run this in RStudio.")
  }
  
  # Check for required packages
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    stop("httpuv package is required. Install with: install.packages('httpuv')")
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
        cat("🔍 Auto-capture triggered by task callback!\n")
        auto_capture_error()
      }
    }
    return(TRUE)  # Keep the callback active
  }
  
  # Add the task callback
  addTaskCallback(error_callback, name = "error_monitor")
  
  cat("✅ Auto-capture system initialized with task callback\n")
  cat("Errors will be automatically detected and captured!\n")
  
  # Initialize auto-fix mode as disabled
  if (!exists(".GlobalEnv$auto_fix_mode")) {
    .GlobalEnv$auto_fix_mode <- FALSE
  }
  
  # Start WebSocket server
  start_websocket_server()
  
  # Create and launch HTML interface
  launch_html_interface()
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
          
          cat("Detected RStudio theme:", theme_data$editor, "\n")
          cat("Global theme:", theme_data$global, "\n")
          cat("Is dark theme:", theme_data$dark, "\n")
          cat("Colors:", toString(theme_data[c("foreground", "background", "console_background", "console_foreground")]), "\n")
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
      cat("Stopped existing WebSocket server\n")
    }, error = function(e) {
      cat("No existing server to stop\n")
    })
  }
  
  # Clear any existing server reference
  .GlobalEnv$websocket_server <- NULL
  
  # Define message handler function
  message_handler <- function(ws, isBinary, data) {
    cat("Raw message received:", data, "\n")
    cat("DEBUG: Message length:", nchar(data), "\n")
    
    # Global error handler to catch any unhandled errors
    tryCatch({
      # Parse JSON message
      request <- jsonlite::fromJSON(data)
      cat("Parsed request:", request$action, "\n")
                  cat("DEBUG: Request details:", tryCatch(toString(request), error = function(e) "Error converting request to string"), "\n")
      
      # Handle different actions
      response <- switch(request$action,
        "set_access_code" = {
          # Store access code from frontend
          .GlobalEnv$current_access_code <- request$access_code
          cat("Access code set to:", request$access_code, "\n")
          list(action = "access_code_set", status = "success")
        },
        "get_context" = {
          # Get current R context
          context <- capture_context()
          list(action = "context", data = context)
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
          # Execute code and capture results
          tryCatch({
            # Capture output
            output <- capture.output({
              result <- eval(parse(text = request$code))
            })
            
            # Get last error if any
            last_error <- geterrmessage()
            
            list(
              action = "executed", 
              status = "success",
              result = if(exists("result")) result else NULL,
              output = output,
              error = if(last_error != "") last_error else NULL
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
              error_context <- capture_context()
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
            error_context <- capture_context()
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
          cat("Using intelligent indexing system for chat...\n")
          
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
              cat("Detected changes, storing...\n")
              
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
                cat("Changes stored\n")
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
            cat("DEBUG: current_context type:", class(current_context), "\n")
            cat("DEBUG: current_context keys:", names(current_context), "\n")
            cat("DEBUG: current_context length:", length(current_context), "\n")
            cat("DEBUG: current_context structure:", str(current_context, max.level = 1), "\n")
            
            # Check if context has meaningful data
            if (!is.null(current_context$workspace_objects)) {
              cat("DEBUG: Number of workspace objects:", length(current_context$workspace_objects), "\n")
              object_names <- sapply(current_context$workspace_objects, function(obj) obj$name)
              cat("DEBUG: Object names:", paste(object_names, collapse = ", "), "\n")
            }
            
            if (!is.null(current_context$file_info)) {
              cat("DEBUG: Has file info:", !is.null(current_context$file_info$file_contents), "\n")
              if (!is.null(current_context$file_info$file_contents)) {
                cat("DEBUG: File content length:", nchar(current_context$file_info$file_contents), "\n")
              }
            }
            
            # Debug intelligent_context
            cat("DEBUG: intelligent_context type:", class(intelligent_context), "\n")
            cat("DEBUG: intelligent_context length:", length(intelligent_context), "\n")
            cat("DEBUG: intelligent_context first 200 chars:", substr(intelligent_context, 1, 200), "\n")
            cat("DEBUG: intelligent_context contains 'dataframe':", grepl("dataframe", tolower(intelligent_context)), "\n")
            cat("DEBUG: intelligent_context contains 'sales_data':", grepl("sales_data", intelligent_context), "\n")
            
            # Debug current_context structure
            cat("DEBUG: current_context type:", class(current_context), "\n")
            cat("DEBUG: current_context keys:", names(current_context), "\n")
            cat("DEBUG: current_context length:", length(current_context), "\n")
            
            # Debug workspace_objects
            if ("workspace_objects" %in% names(current_context)) {
              cat("DEBUG: workspace_objects type:", class(current_context$workspace_objects), "\n")
              cat("DEBUG: workspace_objects length:", length(current_context$workspace_objects), "\n")
              if (length(current_context$workspace_objects) > 0) {
                cat("DEBUG: First workspace object:", names(current_context$workspace_objects)[1], "\n")
                cat("DEBUG: First object structure:", str(current_context$workspace_objects[[1]]), "\n")
              }
            }
            
            # Debug file_info
            if ("file_info" %in% names(current_context)) {
              cat("DEBUG: file_info type:", class(current_context$file_info), "\n")
              cat("DEBUG: file_info keys:", names(current_context$file_info), "\n")
              if ("file_contents" %in% names(current_context$file_info)) {
                file_content <- current_context$file_info$file_contents
                cat("DEBUG: file_contents type:", class(file_content), "\n")
                cat("DEBUG: file_contents length:", length(file_content), "\n")
                if (length(file_content) > 0) {
                  cat("DEBUG: First line of file:", toString(file_content[1]), "\n")
                }
              }
            }
            
            # Debug environment_info
            if ("environment_info" %in% names(current_context)) {
              cat("DEBUG: environment_info type:", class(current_context$environment_info), "\n")
              cat("DEBUG: environment_info keys:", names(current_context$environment_info), "\n")
            }
            
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
            cat("DEBUG: Request body structure:\n")
            cat("DEBUG: - access_code:", request_body$access_code, "\n")
            cat("DEBUG: - prompt:", request_body$prompt, "\n")
            cat("DEBUG: - context_data keys:", names(request_body$context_data), "\n")
            cat("DEBUG: - context_data workspace_objects length:", length(request_body$context_data$workspace_objects), "\n")
            cat("DEBUG: - context_data file_info keys:", names(request_body$context_data$file_info), "\n")
            cat("DEBUG: - context_data environment_info keys:", names(request_body$context_data$environment_info), "\n")
            
            cat("DEBUG: Sending request with context_data length:", length(request_body$context_data), "\n")
            cat("DEBUG: Request URL: https://rgent.onrender.com/chat/stream\n")
            cat("DEBUG: Request body keys:", names(request_body), "\n")
            cat("DEBUG: About to make HTTP request to backend...\n")
            
            # Debug the actual JSON being sent
            request_json <- jsonlite::toJSON(request_body, auto_unbox = TRUE)
            cat("DEBUG: Request JSON length:", nchar(request_json), "\n")
            cat("DEBUG: Request JSON preview:", substr(request_json, 1, 500), "\n")
            
            response <- httr::POST(
              "https://rgent.onrender.com/chat/stream",
              body = request_body,
              encode = "json",
              httr::timeout(30),  # 30 second timeout
              httr::add_headers("Accept" = "text/event-stream")
            )
            
            cat("Response status:", httr::status_code(response), "\n")
            cat("DEBUG: Response headers:", toString(httr::headers(response)), "\n")
            
            if (httr::status_code(response) == 200) {
              # Handle streaming response
              response_text <- httr::content(response, "text")
              cat("Streaming response received\n")
              cat("DEBUG: Response text length:", nchar(response_text), "\n")
              cat("DEBUG: First 200 chars of response:", substr(response_text, 1, 200), "\n")
              
              # Parse Server-Sent Events (SSE) format
              lines <- strsplit(response_text, "\n")[[1]]
              cat("DEBUG: Number of lines:", length(lines), "\n")
              chunks <- c()
              full_response <- ""
              
              for (line in lines) {
                cat("DEBUG: Processing line:", line, "\n")
                if (grepl("^data: ", line)) {
                  # Extract JSON data from SSE format
                  json_data <- substring(line, 7)  # Remove "data: " prefix
                  cat("DEBUG: Extracted JSON data:", json_data, "\n")
                  if (json_data != "[DONE]") {
                    tryCatch({
                      chunk_data <- jsonlite::fromJSON(json_data)
                      cat("DEBUG: Parsed chunk_data:", toString(chunk_data), "\n")
                      
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
                        
                        # Small delay for streaming effect
                          Sys.sleep(0.1)
                        }
                      }
                      
                      # Check if this is the final chunk
                      if (!is.null(chunk_data$done) && chunk_data$done) {
                        cat("DEBUG: Received final chunk, finishing stream\n")
                        
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
                cat("DEBUG: Added AI response to conversation history. Length:", nchar(full_response), "\n")
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
              cat("DEBUG: Full error response:", toString(error_content), "\n")
              list(action = "ai_response", message = paste("Error connecting to AI service. Status:", httr::status_code(response)))
            }
          }, error = function(e) {
            cat("Exception during backend call:", e$message, "\n")
            cat("DEBUG: Exception details:", toString(e), "\n")
            cat("DEBUG: Exception class:", class(e), "\n")
            list(action = "ai_response", message = paste("Error connecting to AI service:", e$message))
          })
        },
        "initialize_session" = {
          # Initialize RAG session with initial context
          cat("Initializing RAG session...\n")
          
          # Generate session ID if not provided
          session_id <- if (!is.null(request$session_id)) request$session_id else paste0("session_", as.numeric(Sys.time()))
          
          # Capture initial context
          initial_context <- capture_context()
          
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
          
          current_context <- capture_context()
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
            # Detect changes first
            current_context <- capture_context()
            last_context <- .GlobalEnv$last_context_state
            changes <- if (!is.null(last_context)) detect_context_changes(last_context, current_context) else list()
            
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
                 cat("DEBUG: Full error response:", toString(error_content), "\n")
                 
                 error_response <- list(
                   action = "debug_ai_response",
                   streaming = FALSE,
                   error = paste("AI analysis failed:", httr::status_code(response))
                 )
                 ws$send(jsonlite::toJSON(error_response, auto_unbox = TRUE))
               }
                          }, error = function(e) {
               cat("DEBUG: Exception details:", toString(e), "\n")
               cat("DEBUG: Exception class:", class(e), "\n")
               
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
          cat("Starting new conversation - clearing history\n")
          
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
            new_status <- toggle_auto_fix()
            list(action = "toggle_auto_fix", status = "success", auto_fix_mode = new_status)
          }, error = function(e) {
            list(action = "toggle_auto_fix", status = "error", message = e$message)
          })
        },
        "analyze_last_plot" = {
          # Analyze the last plot command and provide insights
          cat("Analyzing last plot...\n")
          
          tryCatch({
            # Step 1: Find and analyze the last plot
            analysis_result <- analyze_last_plot()
            
            if (!analysis_result$success) {
              # No plot found or analysis failed
              list(action = "plot_analysis", 
                   success = FALSE, 
                   message = analysis_result$message)
            } else {
              # Send step 2 progress
              progress_msg <- list(
                action = "chat_with_ai",
                message = "*Step 2: Running statistical analysis...*"
              )
              ws$send(jsonlite::toJSON(progress_msg, auto_unbox = TRUE))
              
              # Step 3: Send to AI for interpretation
              progress_msg <- list(
                action = "chat_with_ai",
                message = "*Step 3: Generating insights and recommendations...*"
              )
              ws$send(jsonlite::toJSON(progress_msg, auto_unbox = TRUE))
              # Step 2: Send analysis to AI for interpretation
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
              
              # Send to AI for interpretation using regular chat endpoint
              response <- httr::POST(
                "https://rgent.onrender.com/chat",
                body = list(
                  access_code = get_current_access_code(),
                  prompt = prompt,
                  context_data = list(
                    plot_analysis = analysis,
                    workspace_objects = capture_context()$workspace_objects
                  )
                ),
                encode = "json",
                httr::timeout(60)
              )
              
              if (httr::status_code(response) == 200) {
                ai_result <- httr::content(response)
                list(action = "plot_analysis", 
                     success = TRUE,
                     plot_info = analysis,
                     ai_interpretation = ai_result$response)
              } else {
                list(action = "plot_analysis", 
                     success = FALSE,
                     message = paste("AI analysis failed. Status:", httr::status_code(response)))
              }
            }
          }, error = function(e) {
            list(action = "plot_analysis", 
                 success = FALSE,
                 message = paste("Plot analysis failed:", e$message))
          })
        },
        list(action = "error", message = "Unknown action")
      )
      
      # Send response back to JavaScript
      tryCatch({
        cat("DEBUG: Sending response to frontend...\n")
        response_json <- jsonlite::toJSON(response, auto_unbox = TRUE)
        cat("DEBUG: Response JSON length:", nchar(response_json), "\n")
        ws$send(response_json)
        cat("DEBUG: Response sent successfully\n")
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
        cat("DEBUG: Sending error response to frontend...\n")
        error_json <- jsonlite::toJSON(error_response, auto_unbox = TRUE)
        cat("DEBUG: Error JSON length:", nchar(error_json), "\n")
        ws$send(error_json)
        cat("DEBUG: Error response sent successfully\n")
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
          cat("WebSocket connection opened\n")
          
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
            cat("Theme information sent to client\n")
          }, error = function(e) {
            cat("Error sending theme info:", e$message, "\n")
          })
          
          # Set up message handler
          ws$onMessage(function(isBinary, data) {
            message_handler(ws, isBinary, data)
          })
        },
        onWSClose = function(ws) {
          cat("WebSocket connection closed\n")
        }
      )
    )
    
    cat("WebSocket server started on ws://127.0.0.1:8888\n")
    
  }, error = function(e) {
    cat("Error starting WebSocket server on port 8888:", e$message, "\n")
    cat("Trying alternative port 8889...\n")
    
    # Try alternative port
    tryCatch({
      .GlobalEnv$websocket_server <- httpuv::startServer(
        "127.0.0.1", 
        8889,
        list(
          onWSOpen = function(ws) {
            cat("WebSocket connection opened on port 8889\n")
            
            # Send theme information immediately upon connection
            tryCatch({
              theme_info <- get_rstudio_theme()
              theme_message <- list(
                action = "theme_info",
                is_dark = theme_info$is_dark,
                theme_name = theme_info$theme_name
              )
              ws$send(jsonlite::toJSON(theme_message, auto_unbox = TRUE))
              cat("Theme information sent to client\n")
            }, error = function(e) {
              cat("Error sending theme info:", e$message, "\n")
            })
            
            # Set up message handler
            ws$onMessage(function(isBinary, data) {
              message_handler(ws, isBinary, data)
            })
          },
          onWSClose = function(ws) {
            cat("WebSocket connection closed\n")
          }
        )
      )
      cat("WebSocket server started on ws://127.0.0.1:8889\n")
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
  
  cat("Found HTML file at:", html_file, "\n")
  
  # Start HTTPS server for clipboard API support
  https_url <- start_https_server(html_file)
  
  if (!is.null(https_url)) {
    # Open HTTPS URL in browser
    browseURL(https_url)
    cat("WebSocket AI Chat opened with HTTPS support.\n")
    cat("Clipboard API should now work properly!\n")
  } else {
    # Fallback to regular viewer pane
  temp_file <- tempfile(fileext = ".html")
  file.copy(html_file, temp_file)
  rstudioapi::viewer(temp_file)
    cat("WebSocket AI Chat opened in viewer pane (HTTP fallback).\n")
    cat("Clipboard API may not work due to browser restrictions.\n")
  }
  
  cat("You can continue using the R console while the chat is active.\n")
  cat("To stop the server, run: stop_websocket_server()\n")
}

#' Start HTTPS server for HTML content
start_https_server <- function(html_file) {
  # Check if we can create SSL certificates
  if (!requireNamespace("openssl", quietly = TRUE)) {
    cat("openssl package not available - installing...\n")
    install.packages("openssl", repos = "https://cran.r-project.org")
  }
  
  # Check if magrittr is available for pipe operator
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    cat("magrittr package not available - installing...\n")
    install.packages("magrittr", repos = "https://cran.r-project.org")
  }
  
  tryCatch({
    # Generate self-signed certificate
    cert_dir <- tempdir()
    key_file <- file.path(cert_dir, "server.key")
    cert_file <- file.path(cert_dir, "server.crt")
    
    # Generate certificate using openssl package (without pipe)
    key <- openssl::rsa_keygen(2048)
    openssl::write_pem(key, key_file)
    
    # Create certificate using openssl command line
    system(paste0("openssl req -new -x509 -key ", key_file, " -out ", cert_file, " -days 365 -subj '/CN=localhost'"))
    
    # Start HTTPS server using httpuv
    port <- 8443
    server <- httpuv::startServer(
      "127.0.0.1", 
      port,
      list(
        call = function(req) {
          # Serve the HTML file
          list(
            status = 200L,
            headers = list("Content-Type" = "text/html"),
            body = readLines(html_file, warn = FALSE)
          )
        }
      ),
      ssl = list(
        cert = cert_file,
        key = key_file
      )
    )
    
    # Store server reference
    .GlobalEnv$https_server <- server
    
    https_url <- paste0("https://127.0.0.1:", port)
    cat("HTTPS server started at:", https_url, "\n")
    
    return(https_url)
    
  }, error = function(e) {
    cat("Failed to start HTTPS server:", e$message, "\n")
    return(NULL)
  })
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
  
  cat("DEBUG: Added message to conversation history. Total messages:", length(conversation_history), "\n")
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
        cat("Processing object:", obj_name, "class:", obj_info$class, "\n")
        
        if (is.data.frame(obj)) {
          cat("  - Is data frame, adding concise info\n")
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
          cat("  - Concise data frame info added\n")
          
        } else if (is.vector(obj) && !is.list(obj)) {
          if (length(obj) <= 5) {
            cat("  - Is small vector, adding values\n")
            tryCatch({
              obj_info$values <- as.character(obj)
            }, error = function(e) {
              cat("  - Error converting vector to character:", e$message, "\n")
              obj_info$values <- paste("Error: Could not convert to character")
            })
          } else {
            cat("  - Is large vector, adding basic summary\n")
            obj_info$summary <- list(
              total_length = length(obj),
              unique_count = tryCatch(length(unique(obj)), error = function(e) "unknown"),
              na_count = tryCatch(sum(is.na(obj)), error = function(e) "unknown")
            )
          }
          cat("  - Vector info added\n")
          
        } else if (is.list(obj) && !is.data.frame(obj)) {
          if (length(obj) <= 3) {
            cat("  - Is small list, adding basic structure\n")
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
            cat("  - Is large list, adding basic summary\n")
            tryCatch({
              obj_info$list_summary <- list(
                total_items = length(obj),
                item_names = names(obj)[1:5]  # Only first 5 names
              )
            }, error = function(e) {
              cat("  - Error processing list summary:", e$message, "\n")
              obj_info$list_summary <- list(total_items = length(obj), item_names = "unknown")
            })
          }
          cat("  - List info added\n")
          
        } else if (is.function(obj)) {
          cat("  - Is function, adding basic info\n")
          tryCatch({
            obj_info$function_args <- names(formals(obj))
          }, error = function(e) {
            cat("  - Error getting function args:", e$message, "\n")
            obj_info$function_args <- "unknown"
          })
          obj_info$function_source <- if (is.primitive(obj)) "primitive" else "user-defined"
          cat("  - Function info added\n")
          
        } else if (is.matrix(obj) || is.array(obj)) {
          cat("  - Is matrix/array, adding basic info\n")
          obj_info$dimensions <- dim(obj)
          cat("  - Matrix/array info added\n")
          
        } else if (is.environment(obj)) {
          cat("  - Is environment, adding all contents\n")
          tryCatch({
            obj_info$env_contents <- ls(obj)
          }, error = function(e) {
            cat("  - Error listing environment contents:", e$message, "\n")
            obj_info$env_contents <- "unknown"
          })
          cat("  - Environment info added\n")
          
        } else {
          cat("  - Generic object, adding minimal info\n")
          obj_info$is_s4 <- isS4(obj)
          cat("  - Generic object info added\n")
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
    cat("DEBUG: Creating context_data list...\n")
    context_data <- list(
      workspace_objects = workspace_objects,
      environment_info = environment_info,
      file_info = file_info,
      timestamp = as.character(Sys.time())
    )
    cat("DEBUG: Context data created successfully\n")
    
    # Convert to JSON and back to flatten nested structures
    cat("DEBUG: Starting JSON conversion...\n")
    tryCatch({
      cat("DEBUG: Converting to JSON...\n")
      json_str <- jsonlite::toJSON(context_data, auto_unbox = TRUE)
      cat("DEBUG: JSON conversion successful, length:", nchar(json_str), "\n")
      cat("DEBUG: Converting back from JSON...\n")
      context_data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      cat("DEBUG: JSON back-conversion successful\n")
    }, error = function(e) {
      cat("Warning: Could not flatten context structure:", e$message, "\n")
      cat("Error details:", tryCatch(toString(e), error = function(e2) "Error converting error to string"), "\n")
      cat("Using original context structure\n")
    })
    
    cat("DEBUG: Returning context_data\n")
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
      cat("WebSocket server stopped\n")
    }, error = function(e) {
      cat("Error stopping server:", e$message, "\n")
    })
  } else {
    cat("No WebSocket server running\n")
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
      
      if (old_obj$class != new_obj$class || old_obj$length != new_obj$length) {
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
    
    # Get relevant context from index
    relevant_context <- get_relevant_context_for_error(console_context$code, last_error)
    
    # Get error details
    error_details <- list(
      has_error = TRUE,
      error_message = last_error,
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
      
      # If auto-fix mode is enabled, automatically analyze
      if (exists(".GlobalEnv$auto_fix_mode") && .GlobalEnv$auto_fix_mode) {
        cat("Auto-fix mode enabled - analyzing error...\n")
        send_error_to_ai(last_error, console_context)
      }
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
    cat("🔍 Auto-capture triggered by execute_with_capture!\n")
    auto_capture_error()
    stop(e$message)  # Re-throw the error
  })
}

#' Toggle auto-fix mode
#' @export
toggle_auto_fix <- function() {
  if (!exists(".GlobalEnv$auto_fix_mode")) {
    .GlobalEnv$auto_fix_mode <- FALSE
  }
  
  .GlobalEnv$auto_fix_mode <- !.GlobalEnv$auto_fix_mode
  
  if (.GlobalEnv$auto_fix_mode) {
    cat("✅ Auto-fix mode ENABLED - errors will be automatically analyzed\n")
  } else {
    cat("❌ Auto-fix mode DISABLED - use debug button manually\n")
  }
  
  return(.GlobalEnv$auto_fix_mode)
}

#' Get auto-fix mode status
#' @export
get_auto_fix_status <- function() {
  if (exists(".GlobalEnv$auto_fix_mode")) {
    return(.GlobalEnv$auto_fix_mode)
  } else {
    return(FALSE)
  }
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
  cat("🔍 Manually triggering auto-capture...\n")
  auto_capture_error()
}

#' Capture error after it occurs
#' @export
capture_last_error <- function() {
  cat("🔍 Capturing last error using .Last.error...\n")
  
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
    cat("DEBUG: Updating workspace index...\n")
    
    # Get all workspace objects
    workspace_objects <- ls(envir = globalenv())
    cat("DEBUG: Found", length(workspace_objects), "workspace objects\n")
    
    # Clear old index using global environment
    .GlobalEnv$workspace_index$objects <- list()
    .GlobalEnv$workspace_index$data_frames <- list()
    .GlobalEnv$workspace_index$functions <- list()
    .GlobalEnv$workspace_index$variables <- list()
    
    # Scan each object safely
    for (obj_name in workspace_objects) {
      cat("DEBUG: Processing object:", obj_name, "\n")
      obj_info <- safe_get_object_info(obj_name)
      cat("DEBUG: Object info - is_data_frame:", obj_info$is_data_frame, "is_function:", obj_info$is_function, "\n")
      
      # Store in appropriate category using global environment
      .GlobalEnv$workspace_index$objects[[obj_name]] <- obj_info
      
      if (obj_info$is_data_frame) {
        .GlobalEnv$workspace_index$data_frames[[obj_name]] <- obj_info
        cat("DEBUG: Added", obj_name, "to data_frames\n")
      } else if (obj_info$is_function) {
        .GlobalEnv$workspace_index$functions[[obj_name]] <- obj_info
        cat("DEBUG: Added", obj_name, "to functions\n")
      } else {
        .GlobalEnv$workspace_index$variables[[obj_name]] <- obj_info
        cat("DEBUG: Added", obj_name, "to variables\n")
      }
    }
    
    .GlobalEnv$workspace_index$last_updated <- Sys.time()
    cat("DEBUG: Workspace index updated with", length(workspace_objects), "objects\n")
    cat("DEBUG: Final counts - objects:", length(.GlobalEnv$workspace_index$objects), "data_frames:", length(.GlobalEnv$workspace_index$data_frames), "functions:", length(.GlobalEnv$workspace_index$functions), "variables:", length(.GlobalEnv$workspace_index$variables), "\n")
    
  }, error = function(e) {
    cat("ERROR: Failed to update workspace index:", e$message, "\n")
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
    cat("DEBUG: Assembling simple context...\n")
    
    # Get workspace_index from global environment
    workspace_index <- .GlobalEnv$workspace_index
    
    # Debug workspace_index
    cat("DEBUG: workspace_index type:", class(workspace_index), "\n")
    cat("DEBUG: workspace_index keys:", names(workspace_index), "\n")
    cat("DEBUG: workspace_index$objects length:", length(workspace_index$objects), "\n")
    cat("DEBUG: workspace_index$data_frames length:", length(workspace_index$data_frames), "\n")
    cat("DEBUG: workspace_index$functions length:", length(workspace_index$functions), "\n")
    
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
      cat("DEBUG: Adding workspace objects section with", length(object_names), "objects\n")
      context_parts <- c(context_parts, list(
        paste("=== WORKSPACE OBJECTS ==="),
        paste("Total objects:", length(object_names)),
        paste("Objects:", paste(object_names, collapse = ", "))
      ))
    } else {
      cat("DEBUG: No workspace objects found in workspace_index$objects\n")
    }
    
    # 3. Get data frames info
    if (length(workspace_index$data_frames) > 0) {
      df_names <- names(workspace_index$data_frames)
      cat("DEBUG: Adding data frames section with", length(df_names), "dataframes\n")
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
      cat("DEBUG: No data frames found in workspace_index$data_frames\n")
    }
    
    # 4. Get functions info
    if (length(workspace_index$functions) > 0) {
      func_names <- names(workspace_index$functions)
      cat("DEBUG: Adding functions section with", length(func_names), "functions\n")
      context_parts <- c(context_parts, list(
        paste("=== FUNCTIONS ==="),
        paste("Functions:", paste(func_names, collapse = ", "))
      ))
    } else {
      cat("DEBUG: No functions found in workspace_index$functions\n")
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
    
    cat("DEBUG: Simple context assembled successfully\n")
    cat("DEBUG: Final context length:", nchar(final_context), "\n")
    cat("DEBUG: Final context contains 'WORKSPACE OBJECTS':", grepl("WORKSPACE OBJECTS", final_context), "\n")
    cat("DEBUG: Final context contains 'DATA FRAMES':", grepl("DATA FRAMES", final_context), "\n")
    
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
      cat("DEBUG: Function chunking failed, using line-based fallback\n")
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
      cat("DEBUG: Creating session_index in global environment for cleanup\n")
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
      cat("DEBUG: Memory cleanup completed\n")
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
    cat("DEBUG: Using direct workspace scanning for context\n")
    
    # Cleanup memory
    cleanup_old_data()
    
    # Track performance
    perf_tracker()
    
  }, error = function(e) {
    cat("ERROR: Index update failed:", e$message, "\n")
    # Fallback to simple context capture
    return(capture_context())
  })
}

update_file_index <- function(file_path, file_content) {
  # Update index for a specific file with comprehensive metadata
  
  tryCatch({
    # Ensure session_index is accessible
    global_env <- globalenv()
    if (!exists("session_index", envir = global_env)) {
      cat("DEBUG: Creating session_index in global environment\n")
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
    
    cat("DEBUG: Updated index for", file_path, "with", length(chunks), "chunks\n")
    cat("DEBUG: File metadata:", file_metadata$function_count, "functions,", 
        file_metadata$chunk_count, "chunks\n")
    
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
cat("DEBUG: Initializing simple, safe indexing system\n")

# Initialize workspace_index in global environment
if (!exists("workspace_index", envir = .GlobalEnv)) {
  .GlobalEnv$workspace_index <- list(
    objects = list(),
    data_frames = list(),
    functions = list(),
    variables = list(),
    last_updated = NULL
  )
  cat("DEBUG: workspace_index initialized in global environment\n")
}

update_workspace_index()

#' Smart filtering functions for workspace objects
#' @export
smart_filter_objects <- function(query, objects) {
  cat("DEBUG: Smart filtering query:", query, "\n")
  cat("DEBUG: Total objects available:", length(objects), "\n")
  
  # Extract keywords from query
  keywords <- extract_keywords(query)
  cat("DEBUG: Extracted keywords:", paste(keywords, collapse = ", "), "\n")
  
  # Try exact matches first
  exact_matches <- find_exact_matches(keywords, objects)
  cat("DEBUG: Exact matches found:", length(exact_matches), "\n")
  
  # If no exact matches, try fuzzy/partial
  if (length(exact_matches) == 0) {
    cat("DEBUG: No exact matches, trying fuzzy/partial matching\n")
    matches <- find_partial_matches(keywords, objects)
    cat("DEBUG: Partial/fuzzy matches found:", length(matches), "\n")
  } else {
    matches <- exact_matches
  }
  
  # If still no matches, return everything (fallback)
  if (length(matches) == 0) {
    cat("DEBUG: No matches found, using comprehensive mode (all objects)\n")
    return(objects)
  }
  
  cat("DEBUG: Returning filtered objects:", length(matches), "\n")
  return(matches)
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
  matches <- list()
  
  for (obj in objects) {
    obj_name <- obj$name
    obj_name_lower <- tolower(obj_name)
    
    # Check for exact matches
    for (keyword in keywords) {
      if (obj_name_lower == keyword) {
        cat("DEBUG: Exact match found:", obj_name, "for keyword:", keyword, "\n")
        matches[[obj_name]] <- obj
        break
      }
    }
  }
  
  return(matches)
}

#' Find partial and fuzzy matches for keywords in object names
find_partial_matches <- function(keywords, objects) {
  matches <- list()
  
  for (obj in objects) {
    obj_name <- obj$name
    obj_name_lower <- tolower(obj_name)
    
    # Check for partial matches
    for (keyword in keywords) {
      # Check if keyword is contained in object name
      if (grepl(keyword, obj_name_lower, fixed = TRUE)) {
        cat("DEBUG: Partial match found:", obj_name, "for keyword:", keyword, "\n")
        matches[[obj_name]] <- obj
        break
      }
      
      # Check if object name is contained in keyword (for abbreviations)
      if (grepl(obj_name_lower, keyword, fixed = TRUE)) {
        cat("DEBUG: Reverse partial match found:", obj_name, "for keyword:", keyword, "\n")
        matches[[obj_name]] <- obj
        break
      }
      
      # Handle underscore variations
      # "salesdata" should match "sales_data"
      obj_no_underscore <- gsub("_", "", obj_name_lower)
      keyword_no_underscore <- gsub("_", "", keyword)
      
      if (obj_no_underscore == keyword_no_underscore) {
        cat("DEBUG: Underscore variation match found:", obj_name, "for keyword:", keyword, "\n")
        matches[[obj_name]] <- obj
        break
      }
      
      # Handle camelCase variations
      # "salesData" should match "sales_data"
      obj_camel <- gsub("_([a-z])", "\\U\\1", obj_name_lower, perl = TRUE)
      if (obj_camel == keyword) {
        cat("DEBUG: CamelCase variation match found:", obj_name, "for keyword:", keyword, "\n")
        matches[[obj_name]] <- obj
        break
      }
    }
    
    # Check for data type matches
    for (keyword in keywords) {
      if (keyword == "dataframe" && obj$is_data_frame) {
        cat("DEBUG: Dataframe type match found:", obj_name, "\n")
        matches[[obj_name]] <- obj
        break
      }
      if (keyword == "plot" && (grepl("plot", obj_name_lower) || grepl("graph", obj_name_lower) || grepl("chart", obj_name_lower))) {
        cat("DEBUG: Plot type match found:", obj_name, "\n")
        matches[[obj_name]] <- obj
        break
      }
      if (keyword == "function" && obj$is_function) {
        cat("DEBUG: Function type match found:", obj_name, "\n")
        matches[[obj_name]] <- obj
        break
      }
      if (keyword == "vector" && obj$is_vector) {
        cat("DEBUG: Vector type match found:", obj_name, "\n")
        matches[[obj_name]] <- obj
        break
      }
    }
  }
  
  return(matches)
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
          cat("Processing object:", obj_name, "class:", obj_info$class, "\n")
          
          if (is.data.frame(obj)) {
            cat("  - Is data frame, adding concise info\n")
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
            cat("  - Concise data frame info added\n")
            
          } else if (is.vector(obj) && !is.list(obj)) {
            if (length(obj) <= 5) {
              cat("  - Is small vector, adding values\n")
              tryCatch({
                obj_info$values <- as.character(obj)
              }, error = function(e) {
                cat("  - Error converting vector to character:", e$message, "\n")
                obj_info$values <- paste("Error: Could not convert to character")
              })
            } else {
              cat("  - Is large vector, adding basic summary\n")
              obj_info$summary <- list(
                total_length = length(obj),
                unique_count = tryCatch(length(unique(obj)), error = function(e) "unknown"),
                na_count = tryCatch(sum(is.na(obj)), error = function(e) "unknown")
              )
            }
            cat("  - Vector info added\n")
            
          } else if (is.list(obj) && !is.data.frame(obj)) {
            if (length(obj) <= 3) {
              cat("  - Is small list, adding basic structure\n")
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
              cat("  - Is large list, adding basic summary\n")
              tryCatch({
                obj_info$list_summary <- list(
                  total_items = length(obj),
                  item_names = names(obj)[1:5]  # Only first 5 names
                )
              }, error = function(e) {
                cat("  - Error processing list summary:", e$message, "\n")
                obj_info$list_summary <- list(total_items = length(obj), item_names = "unknown")
              })
            }
            cat("  - List info added\n")
            
          } else if (is.function(obj)) {
            cat("  - Is function, adding basic info\n")
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
            cat("  - Function info added\n")
            
          } else {
            cat("  - Is other object type, adding basic info\n")
            tryCatch({
              obj_info$summary <- list(
                class = paste(class(obj), collapse = ", "),
                length = length(obj),
                mode = mode(obj)
              )
            }, error = function(e) {
              cat("  - Error processing other object:", e$message, "\n")
              obj_info$summary <- list(class = "unknown", length = "unknown", mode = "unknown")
            })
            cat("  - Other object info added\n")
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
        cat("DEBUG: Applying smart filtering for query:", query, "\n")
        filtered_objects <- smart_filter_objects(query, all_objects)
        cat("DEBUG: Filtered from", length(all_objects), "to", length(filtered_objects), "objects\n")
        # Return structured list with filtered objects
        return(list(
          workspace_objects = filtered_objects,
          environment_info = environment_info,
          file_info = file_info,
          timestamp = Sys.time()
        ))
      } else {
        cat("DEBUG: No query provided, returning all objects\n")
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
    cat("DEBUG: Updating workspace index incrementally...\n")
    
    # Ensure workspace_index exists
    if (!exists("workspace_index", envir = .GlobalEnv)) {
      cat("DEBUG: workspace_index not found, initializing...\n")
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
    cat("DEBUG: Found", length(current_objects), "current workspace objects\n")
    
    # Get last scan info
    last_scan <- .GlobalEnv$workspace_index$last_scan_objects
    if (is.null(last_scan)) {
      cat("DEBUG: No previous scan found, doing full scan\n")
      return(update_workspace_index_full())
    }
    
    # Find new, removed, and potentially changed objects
    new_objects <- setdiff(current_objects, last_scan)
    removed_objects <- setdiff(last_scan, current_objects)
    
    cat("DEBUG: New objects:", length(new_objects), paste(new_objects, collapse = ", "), "\n")
    cat("DEBUG: Removed objects:", length(removed_objects), paste(removed_objects, collapse = ", "), "\n")
    
    # Process only new objects
    if (length(new_objects) > 0) {
      cat("DEBUG: Processing", length(new_objects), "new objects\n")
      for (obj_name in new_objects) {
        cat("DEBUG: Processing new object:", obj_name, "\n")
        obj_info <- safe_get_object_info(obj_name)
        
        # Store in appropriate category
        .GlobalEnv$workspace_index$objects[[obj_name]] <- obj_info
        
        if (obj_info$is_data_frame) {
          .GlobalEnv$workspace_index$data_frames[[obj_name]] <- obj_info
          cat("DEBUG: Added", obj_name, "to data_frames\n")
        } else if (obj_info$is_function) {
          .GlobalEnv$workspace_index$functions[[obj_name]] <- obj_info
          cat("DEBUG: Added", obj_name, "to functions\n")
        } else {
          .GlobalEnv$workspace_index$variables[[obj_name]] <- obj_info
          cat("DEBUG: Added", obj_name, "to variables\n")
        }
      }
    }
    
    # Remove deleted objects
    if (length(removed_objects) > 0) {
      cat("DEBUG: Removing", length(removed_objects), "deleted objects\n")
      for (obj_name in removed_objects) {
        cat("DEBUG: Removing object:", obj_name, "\n")
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
            cat("DEBUG: Object modified:", obj_name, "\n")
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
    
    cat("DEBUG: Modified objects:", length(modified_objects), paste(modified_objects, collapse = ", "), "\n")
    
    # Update last scan info
    .GlobalEnv$workspace_index$last_scan_objects <- current_objects
    .GlobalEnv$workspace_index$last_updated <- Sys.time()
    
    cat("DEBUG: Incremental update complete\n")
    cat("DEBUG: Final counts - objects:", length(.GlobalEnv$workspace_index$objects), 
        "data_frames:", length(.GlobalEnv$workspace_index$data_frames), 
        "functions:", length(.GlobalEnv$workspace_index$functions), 
        "variables:", length(.GlobalEnv$workspace_index$variables), "\n")
    
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
    cat("DEBUG: Performing full workspace index update...\n")
    
    # Get all workspace objects
    workspace_objects <- ls(envir = globalenv())
    cat("DEBUG: Found", length(workspace_objects), "workspace objects\n")
    
    # Clear old index using global environment
    .GlobalEnv$workspace_index$objects <- list()
    .GlobalEnv$workspace_index$data_frames <- list()
    .GlobalEnv$workspace_index$functions <- list()
    .GlobalEnv$workspace_index$variables <- list()
    
    # Scan each object safely
    for (obj_name in workspace_objects) {
      cat("DEBUG: Processing object:", obj_name, "\n")
      obj_info <- safe_get_object_info(obj_name)
      cat("DEBUG: Object info - is_data_frame:", obj_info$is_data_frame, "is_function:", obj_info$is_function, "\n")
      
      # Store in appropriate category using global environment
      .GlobalEnv$workspace_index$objects[[obj_name]] <- obj_info
      
      if (obj_info$is_data_frame) {
        .GlobalEnv$workspace_index$data_frames[[obj_name]] <- obj_info
        cat("DEBUG: Added", obj_name, "to data_frames\n")
      } else if (obj_info$is_function) {
        .GlobalEnv$workspace_index$functions[[obj_name]] <- obj_info
        cat("DEBUG: Added", obj_name, "to functions\n")
      } else {
        .GlobalEnv$workspace_index$variables[[obj_name]] <- obj_info
        cat("DEBUG: Added", obj_name, "to variables\n")
      }
    }
    
    # Store last scan info for incremental updates
    .GlobalEnv$workspace_index$last_scan_objects <- workspace_objects
    .GlobalEnv$workspace_index$last_updated <- Sys.time()
    
    cat("DEBUG: Full workspace index updated with", length(workspace_objects), "objects\n")
    cat("DEBUG: Final counts - objects:", length(.GlobalEnv$workspace_index$objects), 
        "data_frames:", length(.GlobalEnv$workspace_index$data_frames), 
        "functions:", length(.GlobalEnv$workspace_index$functions), 
        "variables:", length(.GlobalEnv$workspace_index$variables), "\n")
    
  }, error = function(e) {
    cat("ERROR: Failed to update workspace index:", e$message, "\n")
  })
}

#' Smart context capture with incremental processing
#' @export
capture_context_smart_incremental <- function(query = NULL) {
  tryCatch({
    cat("DEBUG: Capturing context with incremental processing...\n")
    
    # No incremental update needed - process all objects directly
    
    # Simple approach - process all objects each time
    workspace_objects <- tryCatch({
      cat("DEBUG: Processing all workspace objects\n")
      
      # Get all objects in global environment
      object_names <- ls(envir = globalenv())
      cat("DEBUG: Found", length(object_names), "objects\n")
      
      # Process each object with basic info
      all_objects <- lapply(object_names, function(obj_name) {
        cat("DEBUG: About to process object:", obj_name, "\n")
        tryCatch({
          cat("DEBUG: Getting object:", obj_name, "\n")
          obj <- get(obj_name, envir = globalenv())
          cat("DEBUG: Successfully got object:", obj_name, "class:", class(obj), "\n")
          
          # Basic object info
          cat("DEBUG: Creating obj_info for:", obj_name, "\n")
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
          cat("DEBUG: Created obj_info for:", obj_name, "\n")
          
          # Add basic details for dataframes only
          if (obj_info$is_data_frame) {
            cat("  - Processing dataframe:", obj_name, "\n")
            obj_info$dimensions <- dim(obj)
            obj_info$column_names <- names(obj)
          } else if (obj_info$is_function) {
            cat("  - Processing function:", obj_name, "\n")
            tryCatch({
              obj_info$arguments <- names(formals(obj))
            }, error = function(e) {
              obj_info$arguments <- list()
            })
          } else {
            cat("  - Processing other object:", obj_name, "\n")
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
        cat("DEBUG: Applying smart filtering for query:", query, "\n")
        filtered_objects <- smart_filter_objects(query, all_objects)
        cat("DEBUG: Filtered from", length(all_objects), "to", length(filtered_objects), "objects\n")
        return(filtered_objects)
      } else {
        cat("DEBUG: No query provided, returning all indexed objects\n")
        return(all_objects)
      }
      
    }, error = function(e) {
      cat("ERROR getting workspace objects:", e$message, "\n")
      return(list())
    })
    
    # Get environment information
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
        list(
          file_path = doc$path,
          file_contents = doc$contents,
          selection = doc$selection,
          cursor_position = doc$selection[[1]]$range$start
        )
      } else {
        list(file_path = NULL, file_contents = NULL)
      }
    }, error = function(e) {
      cat("ERROR getting file info:", e$message, "\n")
      list(error = e$message)
    })
    
    # Create context data
    cat("DEBUG: Creating context_data list...\n")
    context_data <- list(
      workspace_objects = workspace_objects,
      environment_info = environment_info,
      file_info = file_info
    )
    
    # Convert to JSON and back to ensure proper structure
    json_str <- jsonlite::toJSON(context_data, auto_unbox = TRUE)
    cat("DEBUG: JSON conversion successful, length:", nchar(json_str), "\n")
    
    # Convert back to list for consistency
    context_data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    
    cat("DEBUG: Returning context_data\n")
    context_data
    
  }, error = function(e) {
    cat("ERROR in capture_context_smart_incremental:", e$message, "\n")
    cat("Falling back to regular capture_context_smart...\n")
    capture_context_smart(query)
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
        "ggplot(", "qplot(", "ggplotly(",
        "plotly::", "leaflet::", "dygraph(",
        "density(", "pairs(", "ggpairs(", "GGally::",
        "qqnorm(", "qqplot(", "qqline(", "residuals(", "fitted(",
        "geom_qq(", "geom_qq_line(", "geom_smooth("
      )
      
      # First pass: Look specifically for ggplot commands
      for (i in length(history_lines):1) {
        line <- history_lines[i]
        if (grepl("ggplot\\(", line, fixed = TRUE)) {
          cat("DEBUG: Found ggplot line at position", i, ":", line, "\n")
          # For ggplot2, we need to reconstruct the multi-line command
          return(reconstruct_ggplot_command(history_lines, i))
        }
      }
      
      # Second pass: Look for other plot commands
      for (i in length(history_lines):1) {
        line <- history_lines[i]
        for (cmd in plot_commands) {
          if (grepl(cmd, line, fixed = TRUE)) {
            cat("DEBUG: Found plot command at position", i, ":", line, "\n")
            return(list(
              command = line,
              line_number = i,
              found = TRUE
            ))
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
    cat("DEBUG: reconstruct_ggplot_command called with start_line:", start_line, "\n")
    # Start from the ggplot line and collect subsequent lines
    command_lines <- c()
    current_line <- start_line
    
    # Add the ggplot line
    command_lines <- c(command_lines, history_lines[current_line])
    cat("DEBUG: Added ggplot line:", history_lines[current_line], "\n")
    
    # Look for continuation lines (lines with + or %>%)
    current_line <- current_line + 1
    cat("DEBUG: Starting to look for continuation lines from line", current_line, "\n")
    while (current_line <= length(history_lines)) {
      line <- history_lines[current_line]
      cat("DEBUG: Checking line", current_line, ":", line, "\n")
      
      # Check if this line continues the ggplot command
      if (grepl("^\\s*\\+", line) || grepl("^\\s*%>%", line) || 
          grepl("geom_", line) || grepl("labs\\(", line) || 
          grepl("theme\\(", line) || grepl("scale_", line) ||
          grepl("facet_", line) || grepl("coord_", line)) {
        cat("DEBUG: Found continuation line:", line, "\n")
        command_lines <- c(command_lines, line)
        current_line <- current_line + 1
      } else {
        # Check if the previous line ended with + (indicating continuation)
        if (length(command_lines) > 0 && grepl("\\+\\s*$", command_lines[length(command_lines)])) {
          cat("DEBUG: Previous line ended with +, adding:", line, "\n")
          command_lines <- c(command_lines, line)
          current_line <- current_line + 1
        } else {
          cat("DEBUG: No more continuation lines found\n")
          break
        }
      }
    }
    
    # Combine all lines into a single command
    full_command <- paste(command_lines, collapse = " ")
    cat("DEBUG: Final reconstructed command:", full_command, "\n")
    
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
  } else if (grepl("plotly", command_lower)) {
    return("interactive")
  } else if (grepl("leaflet", command_lower)) {
    return("map")
  } else {
    return("unknown")
  }
}

#' Extract data variables from plot command
extract_plot_data <- function(command, plot_type) {
  tryCatch({
    cat("DEBUG: extract_plot_data called with plot_type:", plot_type, "\n")
    cat("DEBUG: Command to extract from:", command, "\n")
    
    # Simple extraction - look for common patterns
    if (plot_type == "histogram") {
      # Extract data from hist(data)
      match <- regexpr("hist\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 5, match + attr(match, "match.length") - 2)
        return(data_var)
      }
    } else if (plot_type == "scatter" || plot_type == "line_plot") {
      # Extract x and y from plot(x, y)
      match <- regexpr("plot\\(([^,]+),\\s*([^,)]+)[,)]", command)
      if (match > 0) {
        x_var <- substr(command, match + 5, match + attr(match, "match.length") - 1)
        # Extract y variable
        y_match <- regexpr(",\\s*([^,)]+)[,)]", substr(command, match + 5, nchar(command)))
        if (y_match > 0) {
          y_var <- substr(command, match + 5 + y_match, match + 5 + y_match + attr(y_match, "match.length") - 2)
          return(list(x = x_var, y = y_var))
        }
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
      # Extract data from qqnorm(data) or qqplot(data)
      match <- regexpr("(qqnorm|qqplot)\\(([^,)]+)[,)]", command)
      if (match > 0) {
        data_var <- substr(command, match + 8, match + attr(match, "match.length") - 2)
        return(data_var)
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
    } else if (plot_type == "violin" || plot_type == "ggplot" || plot_type == "scatter" || plot_type == "line_plot" || plot_type == "density") {
      cat("DEBUG: Processing ggplot2 plot type:", plot_type, "\n")
      # For ggplot2 plots, extract the data frame and variables from aes()
      # First extract the data frame
      data_match <- regexpr("ggplot\\(([^,]+)", command)
      cat("DEBUG: Data frame regex match position:", data_match, "\n")
      if (data_match > 0) {
        data_frame <- substr(command, data_match + 7, data_match + attr(data_match, "match.length") - 1)
        cat("DEBUG: Extracted data frame:", data_frame, "\n")
        
        # Then extract variables from aes()
        aes_match <- regexpr("aes\\(([^)]+)\\)", command)
        cat("DEBUG: aes() regex match position:", aes_match, "\n")
        if (aes_match > 0) {
          aes_content <- substr(command, aes_match + 5, aes_match + attr(aes_match, "match.length") - 2)
          cat("DEBUG: Extracted aes content:", aes_content, "\n")
          
          # Extract x and y variables from aes(x=..., y=...)
          # Look for x= and y= patterns
          x_pattern <- "x\\s*=\\s*([^,]+)"
          y_pattern <- "y\\s*=\\s*([^,]+)"
          
          cat("DEBUG: Looking for x and y variables in aes content\n")
          x_match <- regexpr(x_pattern, aes_content)
          y_match <- regexpr(y_pattern, aes_content)
          cat("DEBUG: x_match position:", x_match, "\n")
          cat("DEBUG: y_match position:", y_match, "\n")
          
          if (x_match > 0 && y_match > 0) {
            # Extract x variable
            x_start <- x_match + attr(x_match, "match.length")
            x_end <- x_start + attr(x_match, "match.length") - 1
            x_var <- substr(aes_content, x_start, x_end)
            
            # Extract y variable  
            y_start <- y_match + attr(y_match, "match.length")
            y_end <- y_start + attr(y_match, "match.length") - 1
            y_var <- substr(aes_content, y_start, y_end)
            
            # Clean up variable names
            x_var <- gsub("^\\s+|\\s+$", "", x_var)
            y_var <- gsub("^\\s+|\\s+$", "", y_var)
            
            cat("DEBUG: Successfully extracted both x and y variables\n")
            cat("DEBUG: x_var:", x_var, "\n")
            cat("DEBUG: y_var:", y_var, "\n")
            return(list(
              data_frame = data_frame,
              x = x_var,
              y = y_var
            ))
          } else if (x_match > 0) {
            # Only x variable found
            cat("DEBUG: Only x variable found\n")
            x_start <- x_match + attr(x_match, "match.length")
            x_end <- x_start + attr(x_match, "match.length") - 1
            x_var <- substr(aes_content, x_start, x_end)
            x_var <- gsub("^\\s+|\\s+$", "", x_var)
            cat("DEBUG: x_var:", x_var, "\n")
            
            return(list(
              data_frame = data_frame,
              x = x_var
            ))
          } else {
            # If we can't extract variables, just return the data frame
            cat("DEBUG: Could not extract x,y variables, returning data frame only\n")
            return(data_frame)
          }
        } else {
          # If no aes() found, just return the data frame
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
  } else if (plot_type == "line_plot") {
    commands$correlation <- paste0("cor(", data_var$x, ", ", data_var$y, ")")
    commands$trend_analysis <- paste0("summary(lm(", data_var$y, " ~ ", data_var$x, "))")
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
    # Handle ggplot2 scatter plots
    if (!is.null(data_var$x) && !is.null(data_var$y)) {
      commands$correlation <- paste0("cor(", data_var$data_frame, "$", data_var$x, ", ", data_var$data_frame, "$", data_var$y, ")")
      commands$regression <- paste0("summary(lm(", data_var$data_frame, "$", data_var$y, " ~ ", data_var$data_frame, "$", data_var$x, "))")
      commands$summary_stats <- paste0("summary(", data_var$data_frame, "$", data_var$y, ")")
    } else {
      commands$summary_stats <- paste0("summary(", data_var$data_frame, ")")
      commands$structure <- paste0("str(", data_var$data_frame, ")")
    }
  } else if (plot_type == "line_plot" && is.list(data_var) && !is.null(data_var$data_frame)) {
    # Handle ggplot2 line plots
    if (!is.null(data_var$x) && !is.null(data_var$y)) {
      commands$correlation <- paste0("cor(", data_var$data_frame, "$", data_var$x, ", ", data_var$data_frame, "$", data_var$y, ")")
      commands$trend_analysis <- paste0("summary(lm(", data_var$data_frame, "$", data_var$y, " ~ ", data_var$data_frame, "$", data_var$x, "))")
      commands$time_series <- paste0("if(require(ts)) acf(", data_var$data_frame, "$", data_var$y, ") else 'ts package needed'")
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
    cat("DEBUG: Starting plot analysis...\n")
    
    # Step 1: Find last plot command
    cat("DEBUG: Step 1 - Finding last plot command...\n")
    plot_info <- find_last_plot_command()
    
    if (!plot_info$found) {
      cat("DEBUG: No plot command found\n")
      return(list(
        success = FALSE,
        message = "No plot command found in recent history. Try creating a plot first!"
      ))
    }
    
    cat("DEBUG: Found plot command:", plot_info$command, "\n")
    cat("DEBUG: Command length:", nchar(plot_info$command), "\n")
    
    # Step 2: Identify plot type
    cat("DEBUG: Step 2 - Identifying plot type...\n")
    plot_type <- identify_plot_type(plot_info$command)
    cat("DEBUG: Identified plot type:", plot_type, "\n")
    
    # Step 3: Extract data variables
    cat("DEBUG: Step 3 - Extracting data variables...\n")
    data_var <- extract_plot_data(plot_info$command, plot_type)
    
    cat("DEBUG: Data variable extraction result:", class(data_var), "\n")
    if (is.list(data_var)) {
      cat("DEBUG: Data variable list names:", names(data_var), "\n")
      cat("DEBUG: Data variable contents:", toString(data_var), "\n")
    } else {
      cat("DEBUG: Data variable (not list):", toString(data_var), "\n")
    }
    
    if (is.null(data_var)) {
      cat("DEBUG: Data extraction failed - returning NULL\n")
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