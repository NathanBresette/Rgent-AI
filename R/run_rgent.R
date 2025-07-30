#' Launch the AI assistant in the browser
#' One-command solution that opens the HTML interface connected to Render backend
#' @param port Port for the local plumber server (default: auto-detect)
#' @export
run_rgent <- function(port = NULL) {
  cat("Launching RStudio AI Assistant...\n")
  
  # 1. Install required packages if not already installed
  required_packages <- c("plumber", "callr", "httr", "jsonlite", "rstudioapi")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat("Installing required packages:", paste(missing_packages, collapse = ", "), "\n")
    utils::install.packages(missing_packages, repos = "https://cran.rstudio.com/")
    
    # Check if installation was successful
    still_missing <- missing_packages[!sapply(missing_packages, requireNamespace, quietly = TRUE)]
    if (length(still_missing) > 0) {
      cat("Failed to install packages:", paste(still_missing, collapse = ", "), "\n")
      return(invisible(FALSE))
    }
  }
  
  cat("All required packages are available\n")
  
  # 2. Start local plumber server for RStudio integration
  cat("Starting local plumber server for RStudio integration...\n")
  
  # Always find an available port if not specified
  if (is.null(port)) {
    tryCatch({
      port <- find_available_port()
      cat("Using dynamic port:", port, "\n")
    }, error = function(e) {
      cat("Could not find available port:", e$message, "\n")
      cat("Try specifying a port manually: run_rgent(port = 8889)\n")
      return(invisible(FALSE))
    })
  }
  # Ensure port is numeric and not NULL
  if (is.null(port) || !is.numeric(port) || is.na(port)) {
    cat("Invalid port. Please specify a valid port number.\n")
    return(invisible(FALSE))
  }
  
  # Capture workspace objects in main R session
  cat("Capturing workspace objects...\n")
  workspace_objects <- tryCatch({
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
            preview = if (is.data.frame(obj) && nrow(obj) > 0 && ncol(obj) > 0) {
              # Create preview manually for data frames
              preview_lines <- character(0)
              
              # Add column names
              preview_lines <- c(paste(names(obj), collapse = "\t"))
              
              # Add first 3 rows
              for (i in 1:min(3, nrow(obj))) {
                row_data <- sapply(obj[i, ], as.character)
                preview_lines <- c(preview_lines, paste(row_data, collapse = "\t"))
              }
              
              paste(preview_lines, collapse = "\n")
            } else if (is.function(obj)) {
              # Create preview for functions - show complete function definition
              function_lines <- capture.output(print(obj))
              paste(function_lines, collapse = "\n")
            } else if (is.vector(obj) && length(obj) > 0) {
              # Create preview for vectors - show all elements in bracket format
              paste("[", paste(obj, collapse = ", "), "]", sep = "")
            } else {
              paste("Object of class:", paste(class(obj), collapse = ", "))
            }
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
      list()
    }
  }, error = function(e) {
    cat("Error capturing workspace objects:", e$message, "\n")
    list()
  })
  
  cat("Found", length(workspace_objects), "workspace objects\n")
  
  # Start plumber server in background with workspace data and theme info
  plumber_api_file <- file.path(system.file("viewer_ai", package = "rstudioai"), "plumber_api.R")
  
  cat("Plumber API file path is:", plumber_api_file, "\n")
  cat("File exists:", file.exists(plumber_api_file), "\n")
  
  if (file.exists(plumber_api_file)) {
    plumber_process <- callr::r_bg(function(api_file, server_port, workspace_data) {
      # Store workspace data in global variables that the plumber API can access
      .GlobalEnv$captured_workspace_objects <- workspace_data
      
      pr <- plumber::plumb(api_file)
      pr$run(host = "127.0.0.1", port = server_port)
    }, args = list(api_file = plumber_api_file, server_port = port, workspace_data = workspace_objects))

    cat("plumber_process class:", class(plumber_process), "\n")

    # Wait a moment for the server to start
    Sys.sleep(2)
    cat("Local plumber server started on port", port, "\n")
  } else {
    cat("Plumber API file not found\n")
    return(invisible(FALSE))
  }
  
  # Helper: Wait for server to be ready
  wait_for_server <- function(port, timeout = 30, plumber_process = NULL) {
    url <- sprintf("http://127.0.0.1:%d/health", port)
    start_time <- Sys.time()
    cat("Waiting for server at:", url, "\n")
    repeat {
      res <- tryCatch(httr::GET(url), error = function(e) NULL)
      if (!is.null(res) && httr::status_code(res) == 200) {
        cat("Server is ready!\n")
        break
      }
      if (as.numeric(Sys.time() - start_time, units = "secs") > timeout) {
        cat("\n--- Server startup timeout ---\n")
        cat("Tried to connect to:", url, "\n")
        cat("Process class:", class(plumber_process), "\n")
        if (!is.null(plumber_process)) {
          cat("Process status:", ifelse(plumber_process$is_alive(), "alive", "dead"), "\n")
          cat("\n--- Plumber process output ---\n")
          tryCatch({
            cat(plumber_process$read_all_output(), sep = "\n")
          }, error = function(e) cat("Could not read output:", e$message, "\n"))
          cat("\n--- Plumber process error ---\n")
          tryCatch({
            cat(plumber_process$read_all_error(), sep = "\n")
          }, error = function(e) cat("Could not read error:", e$message, "\n"))
        }
        cat("Please check if the Plumber API file exists and is valid.\n")
        stop("Server did not start in time.")
      }
      Sys.sleep(0.5)
      cat(".")
    }
  }
  
  wait_for_server(port, timeout = 30, plumber_process = plumber_process)
  
  # 3. Test connection to Render backend
  
  # 4. Check for console errors and auto-send to Claude
  cat("Checking for console errors...\n")
  
  # Capture recent console output to check for errors
  console_output <- tryCatch({
    capture.output({
      # Check for common error patterns
      cat("Checking console for errors...\n")
    })
  }, error = function(e) {
    character(0)
  })
  
  # Check if there are any error patterns in the console
  error_detected <- FALSE
  error_message <- ""
  
  # Look for common R error patterns
  if (length(console_output) > 0) {
    for (line in console_output) {
      if (grepl("Error:", line, ignore.case = TRUE) ||
          grepl("Error in", line, ignore.case = TRUE) ||
          grepl("Warning:", line, ignore.case = TRUE) ||
          grepl("Error:", line, ignore.case = TRUE) ||
          grepl("object not found", line, ignore.case = TRUE) ||
          grepl("could not find function", line, ignore.case = TRUE) ||
          grepl("unexpected", line, ignore.case = TRUE)) {
        error_detected <- TRUE
        error_message <- paste(error_message, line, sep = "\n")
      }
    }
  }
  
  # If error detected, auto-send to Claude
  if (error_detected) {
    cat("Console error detected! Auto-sending to Claude for help...\n")
    
    # Prepare error message for Claude
    claude_message <- paste(
      "I encountered an error in my R console:",
      error_message,
      "\n\nCan you help me understand and fix this error?",
      sep = ""
    )
    
    # Send to Claude via the local API
    tryCatch({
      response <- httr::POST(
        url = sprintf("http://127.0.0.1:%d/api/chat", port),
        httr::content_type("application/json"),
        body = jsonlite::toJSON(list(
          access_code = "AUTO_ERROR",
          prompt = claude_message,
          context_data = list(
            console_history = console_output,
            workspace_objects = workspace_objects,
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
        cat("Error sent to Claude successfully!\n")
      } else {
        cat("Failed to send error to Claude. Status:", httr::status_code(response), "\n")
      }
    }, error = function(e) {
      cat("Error sending to Claude:", e$message, "\n")
    })
  } else {
    cat("No console errors detected.\n")
  }
  
  # 5. Open the HTML UI in RStudio Viewer
  cat("Opening AI Assistant in RStudio Viewer...\n")
  
  # Open in RStudio Viewer using the local HTTP server
  viewer_url <- sprintf("http://127.0.0.1:%d/", port)
  cat("Opening URL:", viewer_url, "\n")
  
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::viewer(viewer_url)
    cat("AI Assistant launched successfully in the RStudio Viewer pane!\n")
  } else {
    stop("RStudio Viewer is not available. Please run this addin inside RStudio.")
  }
  
  invisible(TRUE)
} 