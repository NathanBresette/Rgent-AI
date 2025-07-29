# Plumber API for RStudio integration
# This provides local endpoints for the HTML interface to interact with RStudio

library(plumber)
library(rstudioapi)
library(jsonlite)

# Configure jsonlite to auto-unbox single values to prevent array wrapping
options(jsonlite.auto_unbox = TRUE)



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
    
    # Get console history as a simple list of strings
    console_history <- tryCatch({
      history_file <- Sys.getenv("R_HISTFILE", file.path(Sys.getenv("HOME"), ".Rhistory"))
      if (file.exists(history_file)) {
        lines <- readLines(history_file, n = 20)
        # Convert to simple character vector, not list
        as.character(lines)
      } else {
        character(0)  # Return empty vector instead of error message
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
