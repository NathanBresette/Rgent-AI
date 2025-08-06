#' AI-driven code insertion with intelligent placement
#' @param code The code to insert
#' @param context_context Optional context to help determine placement
#' @export
ai_insert_code <- function(code, context_context = NULL) {
  tryCatch({
    if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
      cat("RStudio API is not available. Please run this in RStudio.\n")
      return(FALSE)
    }
    
    # Get current document context
    doc_context <- rstudioapi::getActiveDocumentContext()
    if (is.null(doc_context)) {
      cat("❌ No active document found\n")
      return(FALSE)
    }
    
    # Determine best insertion point based on context
    insertion_line <- 1  # Default to top
    
    if (!is.null(context_context)) {
      # Simple heuristic: if context mentions "function", insert after existing functions
      if (grepl("function", context_context, ignore.case = TRUE)) {
        # Find last function definition
        for (i in length(doc_context$contents):1) {
          if (grepl("function\\s*\\(", doc_context$contents[i])) {
            insertion_line <- i + 1
            break
          }
        }
      }
    }
    
    # Position cursor and insert
    rstudioapi::setCursorPosition(rstudioapi::document_position(insertion_line, 1))
    rstudioapi::insertText(paste0(code, "\n"))
    
    cat("✅ AI inserted code at line", insertion_line, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Failed to insert code:", e$message, "\n")
    return(FALSE)
  })
}

#' AI-driven code replacement
#' @param old_code_pattern Pattern to identify code to replace
#' @param new_code The replacement code
#' @export
ai_replace_code <- function(old_code_pattern, new_code) {
  tryCatch({
    if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
      cat("RStudio API is not available. Please run this in RStudio.\n")
      return(FALSE)
    }
    
    # Get current document context
    doc_context <- rstudioapi::getActiveDocumentContext()
    if (is.null(doc_context)) {
      cat("❌ No active document found\n")
      return(FALSE)
    }
    
    # Find the code to replace
    start_line <- NULL
    end_line <- NULL
    
    for (i in 1:length(doc_context$contents)) {
      if (grepl(old_code_pattern, doc_context$contents[i])) {
        start_line <- i
        # Find the end of the function/block
        for (j in i:length(doc_context$contents)) {
          if (grepl("^\\s*$", doc_context$contents[j]) || 
              (j > i && grepl("^[a-zA-Z]", doc_context$contents[j]))) {
            end_line <- j - 1
            break
          }
        }
        if (is.null(end_line)) end_line <- length(doc_context$contents)
        break
      }
    }
    
    if (is.null(start_line)) {
      cat("❌ Could not find code matching pattern:", old_code_pattern, "\n")
      return(FALSE)
    }
    
    # Replace the code range
    rstudioapi::modifyRange(
      rstudioapi::document_range(
        start = rstudioapi::document_position(start_line, 1),
        end = rstudioapi::document_position(end_line + 1, 1)
      ),
      paste0(new_code, "\n")
    )
    
    cat("✅ AI replaced code from line", start_line, "to", end_line, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Failed to replace code:", e$message, "\n")
    return(FALSE)
  })
}

#' Smart code insertion with AI context
#' @param code The code to insert
#' @param location_hint Optional hint about where to insert
#' @export
ai_smart_insert <- function(code, location_hint = NULL) {
  tryCatch({
    if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
      cat("RStudio API is not available. Please run this in RStudio.\n")
      return(FALSE)
    }
    
    # Get current document context
    doc_context <- rstudioapi::getActiveDocumentContext()
    if (is.null(doc_context)) {
      cat("❌ No active document found\n")
      return(FALSE)
    }
    
    # Determine insertion point based on hint
    insertion_line <- 1
    
    if (!is.null(location_hint)) {
      if (grepl("top|beginning|start", location_hint, ignore.case = TRUE)) {
        insertion_line <- 1
      } else if (grepl("bottom|end", location_hint, ignore.case = TRUE)) {
        insertion_line <- length(doc_context$contents) + 1
      } else if (grepl("after.*function", location_hint, ignore.case = TRUE)) {
        # Find last function
        for (i in length(doc_context$contents):1) {
          if (grepl("function\\s*\\(", doc_context$contents[i])) {
            insertion_line <- i + 1
            break
          }
        }
      } else if (grepl("cursor", location_hint, ignore.case = TRUE)) {
        # Use current cursor position
        if (length(doc_context$selection) > 0) {
          selection <- doc_context$selection[[1]]
          start_pos <- selection$range$start
          insertion_line <- start_pos[["row"]]
        }
      }
    }
    
    # Insert the code
    rstudioapi::setCursorPosition(rstudioapi::document_position(insertion_line, 1))
    rstudioapi::insertText(paste0(code, "\n"))
    
    cat("✅ AI smart-inserted code at line", insertion_line, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Failed to smart-insert code:", e$message, "\n")
    return(FALSE)
  })
}

#' Execute code directly in R console
#' @param code The code to execute
#' @export
execute_code <- function(code) {
  tryCatch({
    # Evaluate the code in the global environment
    result <- eval(parse(text = code), envir = .GlobalEnv)
    
    # Print the result if it's not NULL
    if (!is.null(result)) {
      print(result)
    }
    
    cat("✅ Code executed successfully\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error executing code:", e$message, "\n")
    return(FALSE)
  })
}

#' Execute R function directly from frontend
#' @param function_name Name of the R function to call
#' @param param1 First parameter
#' @param param2 Second parameter
#' @export
execute_r_function <- function(function_name, param1, param2 = NULL) {
  tryCatch({
    # Validate function name
    valid_functions <- c("ai_smart_insert", "ai_replace_code", "ai_insert_code")
    if (!function_name %in% valid_functions) {
      cat("❌ Invalid function name:", function_name, "\n")
      return(FALSE)
    }
    
    # Call the appropriate function
    if (function_name == "ai_smart_insert") {
      result <- ai_smart_insert(param1, param2)
    } else if (function_name == "ai_replace_code") {
      result <- ai_replace_code(param1, param2)
    } else if (function_name == "ai_insert_code") {
      result <- ai_insert_code(param1, param2)
    }
    
    return(result)
    
  }, error = function(e) {
    cat("❌ Error executing R function:", e$message, "\n")
    return(FALSE)
  })
} 