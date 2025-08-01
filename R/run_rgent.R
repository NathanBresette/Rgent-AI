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
  
  # 4. Open in RStudio viewer
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
    } else {
      cat("❌ RStudio API not available. Please run this in RStudio.\n")
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
    
    # Return context data
    list(
      workspace_objects = workspace_objects,
      environment_info = environment_info,
      timestamp = as.character(Sys.time())
    )
  }, error = function(e) {
    list(
      error = paste("Error capturing context:", e$message),
      timestamp = as.character(Sys.time())
    )
  })
}

#' Insert code into active document
#' @param code The code to insert
#' @export
insert_code <- function(code) {
  tryCatch({
    if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
      cat("RStudio API is not available. Please run this in RStudio.\n")
      return(FALSE)
    }
    
    rstudioapi::insertText(code)
    cat("✅ Code inserted successfully!\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Failed to insert code:", e$message, "\n")
    return(FALSE)
  })
} 