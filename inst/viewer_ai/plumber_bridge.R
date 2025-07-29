# Plumber API bridge for RStudio AI Assistant
# This file provides HTTP endpoints for the non-blocking add-in

library(plumber)
library(rstudioapi)
library(jsonlite)

#* @post /insert_code
#* @param req The request object
function(req) {
  tryCatch({
    # Parse JSON body
    body <- fromJSON(req$postBody)
    code <- body$code
    
    if (is.null(code) || code == "") {
      return(list(error = "No code provided"))
    }
    
    # Insert code at cursor position
    rstudioapi::insertText(code)
    
    return(list(success = TRUE, message = "Code inserted successfully"))
  }, error = function(e) {
    return(list(error = e$message))
  })
}

#* @get /health
function() {
  list(status = "healthy", timestamp = Sys.time())
} 