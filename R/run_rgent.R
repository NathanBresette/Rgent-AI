#' Launch the AI assistant in RStudio viewer
#' One-command solution that opens the HTML interface connected to Render backend
#' @export
run_rgent <- function() {
  cat("Launching RStudio AI Assistant...\n")
  
  # 1. Install required packages if not already installed
  required_packages <- c("httr", "jsonlite", "rstudioapi")
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
  
  # 2. Check if RStudio API is available
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    cat("RStudio API is not available. Please run this in RStudio.\n")
    cat("But let's test the HTML file reading anyway...\n")
  }
  
  # 3. Create HTML content for RStudio viewer
  cat("Creating HTML interface...\n")
  
  # Read the HTML file
  html_file <- system.file("public/index.html", package = "rstudioai")
  cat("Looking for HTML file at:", html_file, "\n")
  cat("File exists:", file.exists(html_file), "\n")
  
  if (!file.exists(html_file)) {
    cat("HTML file not found:", html_file, "\n")
    return(invisible(FALSE))
  }
  
  # Read HTML content
  cat("Reading HTML content...\n")
  html_lines <- readLines(html_file)
  html_content <- paste(html_lines, collapse = "\n")
  cat("HTML content length:", nchar(html_content), "characters\n")
  
  # 4. Capture current context
  cat("Capturing current R workspace context...\n")
  context_data <- capture_context()
  cat("Context captured successfully\n")
  
  # 5. Inject context data into HTML
  cat("Injecting context data into HTML...\n")
  context_json <- jsonlite::toJSON(context_data, auto_unbox = TRUE)
  # Replace placeholder context with real data
  # Use a more specific pattern to match the exact JavaScript structure
  pattern <- 'let contextData = \\{[^}]*workspace_objects:\\[\\][^}]*environment_info:[^}]*\\};'
  replacement <- paste0('let contextData = ', context_json, ';')
  html_content <- gsub(pattern, replacement, html_content, perl = TRUE)
  cat("Context data injected into HTML\n")
  
  # 6. Open in RStudio viewer
  cat("Opening AI Assistant in RStudio Viewer...\n")
  tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      # Create a temporary HTML file for RStudio viewer
      temp_html <- tempfile(fileext = ".html")
      writeLines(html_content, temp_html)
      
      # Open in RStudio viewer pane
      rstudioapi::viewer(temp_html)
      cat("✅ AI Assistant opened successfully in RStudio Viewer!\n")
      cat("💡 You can now chat with the AI while continuing to work in RStudio.\n")
      cat("📊 Context data includes", length(context_data$workspace_objects), "workspace objects\n")
    } else {
      cat("❌ RStudio API is not available. Please run this in RStudio.\n")
      cat("📝 HTML content is ready but needs RStudio to display in viewer.\n")
      return(invisible(FALSE))
    }
  }, error = function(e) {
    cat("❌ Failed to open viewer:", e$message, "\n")
    return(invisible(FALSE))
  })
  
  return(invisible(TRUE))
}

#' Capture current workspace context
#' @export
capture_context <- function() {
  tryCatch({
    # Get workspace objects
    workspace_objects <- lapply(ls(envir = .GlobalEnv), function(obj_name) {
      tryCatch({
        obj <- get(obj_name, envir = .GlobalEnv)
        list(
          name = obj_name,
          class = paste(class(obj), collapse = ", "),
          length = length(obj),
          is_function = is.function(obj),
          is_data_frame = is.data.frame(obj)
        )
      }, error = function(e) {
        list(
          name = obj_name,
          class = "error",
          length = NULL,
          is_function = FALSE,
          is_data_frame = FALSE
        )
      })
    })
    
    # Get environment info
    environment_info <- list(
      r_version = as.character(R.version.string),
      platform = as.character(R.version$platform),
      working_directory = as.character(getwd()),
      packages = names(sessionInfo()$otherPkgs)
    )
    
    # Get file information if RStudio API is available
    file_info <- list()
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      tryCatch({
        # Get active document context
        doc_context <- rstudioapi::getActiveDocumentContext()
        if (!is.null(doc_context)) {
          file_info <- list(
            active_file = doc_context$path,
            active_file_name = basename(doc_context$path),
            cursor_line = doc_context$selection[[1]]$range$start$row,
            cursor_column = doc_context$selection[[1]]$range$start$column,
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

#' Insert code at specific location in document
#' @param code The code to insert
#' @param file_path Optional file path (if NULL, uses active document)
#' @param line_number Optional line number (if NULL, uses cursor position)
#' @param replace_selection Whether to replace current selection
#' @export
insert_code_at_location <- function(code, file_path = NULL, line_number = NULL, replace_selection = FALSE) {
  tryCatch({
    if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
      cat("RStudio API is not available. Please run this in RStudio.\n")
      return(FALSE)
    }
    
    # If no code provided, try to read from clipboard
    if (is.null(code)) {
      if (requireNamespace("clipr", quietly = TRUE)) {
        code <- clipr::read_clip()
        if (length(code) == 0) {
          cat("❌ No code found in clipboard\n")
          return(FALSE)
        }
        cat("📋 Reading code from clipboard...\n")
      } else {
        cat("❌ No code provided and clipr package not available\n")
        cat("💡 Install clipr: install.packages('clipr')\n")
        return(FALSE)
      }
    }
    
    # Get document context
    doc_context <- rstudioapi::getActiveDocumentContext()
    
    # If specific file requested, switch to it
    if (!is.null(file_path)) {
      # Try to open the file if it exists
      if (file.exists(file_path)) {
        rstudioapi::navigateToFile(file_path)
        # Wait a moment for file to open
        Sys.sleep(0.5)
        doc_context <- rstudioapi::getActiveDocumentContext()
      } else {
        cat("❌ File not found:", file_path, "\n")
        return(FALSE)
      }
    }
    
    # If specific line requested, navigate to it
    if (!is.null(line_number)) {
      # Set cursor to specific line
      rstudioapi::setCursorPosition(rstudioapi::document_position(line_number, 1))
      cat("📍 Positioned cursor at line", line_number, "\n")
    }
    
    # Insert the code
    if (replace_selection && length(doc_context$selection) > 0) {
      # Replace current selection
      rstudioapi::insertText(code)
      cat("✅ Code inserted, replacing selection\n")
    } else {
      # Insert at cursor position
      rstudioapi::insertText(code)
      cat("✅ Code inserted at cursor position\n")
    }
    
    return(TRUE)
  }, error = function(e) {
    cat("❌ Failed to insert code:", e$message, "\n")
    return(FALSE)
  })
}

#' Insert code into active document
#' @param code The code to insert (if NULL, reads from clipboard)
#' @export
insert_code <- function(code = NULL) {
  tryCatch({
    if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
      cat("RStudio API is not available. Please run this in RStudio.\n")
      return(FALSE)
    }
    
    # If no code provided, try to read from clipboard
    if (is.null(code)) {
      if (requireNamespace("clipr", quietly = TRUE)) {
        code <- clipr::read_clip()
        if (length(code) == 0) {
          cat("❌ No code found in clipboard\n")
          return(FALSE)
        }
        cat("📋 Reading code from clipboard...\n")
      } else {
        cat("❌ No code provided and clipr package not available\n")
        cat("💡 Install clipr: install.packages('clipr')\n")
        return(FALSE)
      }
    }
    
    rstudioapi::insertText(code)
    cat("✅ Code inserted successfully!\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Failed to insert code:", e$message, "\n")
    return(FALSE)
  })
} 