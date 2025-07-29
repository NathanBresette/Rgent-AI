#' Launch the AI assistant in the browser
#' One-command solution that opens the HTML interface connected to Render backend
#' @param port Port for the local plumber server (default: auto-detect)
#' @export
ai_addin_viewer <- function(port = NULL) {
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
      cat("Try specifying a port manually: ai_addin_viewer(port = 8889)\n")
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
      list()
    }
  }, error = function(e) {
    cat("Error capturing workspace objects:", e$message, "\n")
    list()
  })
  
  cat("Found", length(workspace_objects), "workspace objects\n")
  
  # Start plumber server in background with workspace data
  plumber_api_file <- file.path(system.file("viewer_ai", package = "rstudioai"), "plumber_api.R")
  
  cat("Plumber API file path is:", plumber_api_file, "\n")
  cat("File exists:", file.exists(plumber_api_file), "\n")
  
  if (file.exists(plumber_api_file)) {
    plumber_process <- callr::r_bg(function(api_file, server_port, workspace_data) {
      # Store workspace data in a global variable that the plumber API can access
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
  
  # 4. Open the HTML UI in RStudio Viewer
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