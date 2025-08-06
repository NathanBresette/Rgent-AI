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
  
  # Start WebSocket server
  start_websocket_server()
  
  # Create and launch HTML interface
  launch_html_interface()
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
    tryCatch({
      # Parse JSON message
      request <- jsonlite::fromJSON(data)
      cat("Parsed request:", request$action, "\n")
      
      # Handle different actions
      response <- switch(request$action,
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
                  access_code = "DEMO123",
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
                access_code = "DEMO123",
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
          # Call backend API with context
          cat("Calling AI backend...\n")
          context <- capture_context()
          cat("Context captured, making POST request...\n")
          
          # Ensure context is properly structured as a dictionary
          context_dict <- list(
            workspace_objects = context$workspace_objects,
            environment_info = context$environment_info,
            file_info = context$file_info,
            timestamp = context$timestamp
          )
          
          tryCatch({
            response <- httr::POST(
              "https://rgent.onrender.com/chat",
              body = list(
                access_code = "DEMO123",
                prompt = request$message,
                context_data = context_dict
              ),
              encode = "json",
              httr::timeout(30)  # 30 second timeout
            )
            
            cat("Response status:", httr::status_code(response), "\n")
            
            if (httr::status_code(response) == 200) {
              result <- httr::content(response)
              cat("AI response received successfully\n")
              list(action = "ai_response", message = result$response)
            } else {
              cat("Backend returned error status:", httr::status_code(response), "\n")
              error_content <- httr::content(response)
              cat("Error content:", toString(error_content), "\n")
              list(action = "ai_response", message = paste("Error connecting to AI service. Status:", httr::status_code(response)))
            }
          }, error = function(e) {
            cat("Exception during backend call:", e$message, "\n")
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
                access_code = "DEMO123",
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
                  access_code = "DEMO123",
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
                "https://rgent.onrender.com/api/chat-with-rag",
                body = list(
                  access_code = "DEMO123",
                  session_id = session_id,
                  message = request$message,
                  changes = changes
                ),
                encode = "json",
                httr::timeout(30)
              )
              
              if (httr::status_code(response) == 200) {
                result <- httr::content(response)
                list(action = "rag_response", 
                     message = result$response,
                     relevant_chunks = result$relevant_chunks,
                     context_summary = result$context_summary)
              } else {
                list(action = "rag_error", 
                     message = paste("RAG chat failed. Status:", httr::status_code(response)))
              }
            }, error = function(e) {
              list(action = "rag_error", message = paste("Error in RAG chat:", e$message))
            })
          }
        },
        list(action = "error", message = "Unknown action")
      )
      
      # Send response back to JavaScript
      ws$send(jsonlite::toJSON(response, auto_unbox = TRUE))
      
    }, error = function(e) {
      # Send error response
      cat("Error in message handler:", e$message, "\n")
      error_response <- list(action = "error", message = e$message)
      tryCatch({
        ws$send(jsonlite::toJSON(error_response, auto_unbox = TRUE))
      }, error = function(e2) {
        cat("Failed to send error response:", e2$message, "\n")
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

#' Launch HTML interface in RStudio viewer
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
  
  # Create a temporary copy to ensure proper viewer pane opening
  temp_file <- tempfile(fileext = ".html")
  file.copy(html_file, temp_file)
  
  # Open in viewer pane
  rstudioapi::viewer(temp_file)
  
  cat("WebSocket AI Chat opened in viewer pane.\n")
  cat("You can continue using the R console while the chat is active.\n")
  cat("To stop the server, run: stop_websocket_server()\n")
}

#' Capture current workspace context
#' @export
capture_context <- function() {
  tryCatch({
    # Get workspace objects with detailed information
    workspace_objects <- lapply(ls(envir = .GlobalEnv), function(obj_name) {
      tryCatch({
        obj <- get(obj_name, envir = .GlobalEnv)
        
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
        
        # Add object-specific details with debug output
        cat("Processing object:", obj_name, "class:", obj_info$class, "\n")
        
        if (is.data.frame(obj)) {
          cat("  - Is data frame, adding enhanced info\n")
          obj_info$dimensions <- dim(obj)
          obj_info$column_names <- names(obj)
          obj_info$sample_data <- if (nrow(obj) > 0) {
            # Show first few rows as sample
            head_data <- head(obj, 3)
            lapply(1:ncol(head_data), function(i) {
              list(
                column = names(head_data)[i],
                sample_values = as.character(head_data[[i]])
              )
            })
          } else {
            list()
          }
          cat("  - Enhanced data frame info added\n")
        } else if (is.vector(obj) && length(obj) <= 10) {
          cat("  - Is small vector, adding values\n")
          # For small vectors, show the actual values
          obj_info$values <- as.character(obj)
          cat("  - Vector values added\n")
        } else if (is.vector(obj) && length(obj) > 10) {
          cat("  - Is large vector, adding summary\n")
          # For large vectors, show summary
          obj_info$summary <- list(
            first_values = as.character(head(obj, 5)),
            last_values = as.character(tail(obj, 5)),
            total_length = length(obj)
          )
          cat("  - Vector summary added\n")
        } else if (is.list(obj) && length(obj) <= 5) {
          cat("  - Is small list, adding structure\n")
          # For small lists, show structure
          obj_info$list_structure <- lapply(names(obj), function(name) {
            list(
              name = name,
              class = paste(class(obj[[name]]), collapse = ", "),
              length = length(obj[[name]])
            )
          })
          cat("  - List structure added\n")
        } else if (is.function(obj)) {
          cat("  - Is function, adding arguments\n")
          # For functions, show arguments
          obj_info$function_args <- names(formals(obj))
          obj_info$function_body <- paste(deparse(obj), collapse = "\n")
          cat("  - Function details added\n")
        } else if (is.matrix(obj) || is.array(obj)) {
          cat("  - Is matrix/array, adding dimensions\n")
          obj_info$dimensions <- dim(obj)
          obj_info$dim_names <- dimnames(obj)
          cat("  - Matrix/array info added\n")
        } else {
          cat("  - No enhanced info for this object type\n")
        }
        
        obj_info
      }, error = function(e) {
        cat("  - Error processing object:", obj_name, ":", e$message, "\n")
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
    
    # Get environment info
    environment_info <- list(
      r_version = as.character(R.version.string),
      platform = as.character(R.version$platform),
      working_directory = as.character(getwd()),
      packages = names(sessionInfo()$otherPkgs),
      loaded_namespaces = loadedNamespaces()
    )
    
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
          
          file_info <- list(
            active_file = file_path,
            active_file_name = file_name,
            cursor_line = cursor_line,
            cursor_column = cursor_column,
            total_lines = length(doc_context$contents),
            file_contents = doc_context$contents,
            file_modified = doc_context$modified
          )
        }
      }, error = function(e) {
        file_info <- list(error = paste("Error getting file info:", e$message))
      })
    }
    
    # Return context data
    list(
      workspace_objects = workspace_objects,
      environment_info = environment_info,
      file_info = file_info,
      timestamp = as.character(Sys.time())
    )
  }, error = function(e) {
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
  changes <- list(
    file_changes = list(),
    workspace_changes = list()
  )
  
  # Detect file changes
  if (!is.null(old_context$file_info) && !is.null(new_context$file_info)) {
    old_file <- old_context$file_info$file_contents
    new_file <- new_context$file_info$file_contents
    
    if (!is.null(old_file) && !is.null(new_file)) {
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
        if (old_lines[i] != new_lines[i]) {
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
} 