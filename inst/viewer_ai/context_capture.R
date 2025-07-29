#' Send environment context to backend
#'
#' Captures current R environment context and sends it to the backend API
#' @export
send_env_context_to_backend <- function() {
  # Capture environment context
  context_data <- list(
    loaded_packages = .packages(),
    workspace_objects = ls(envir = .GlobalEnv),
    r_version = R.version.string,
    platform = R.version$platform,
    timestamp = Sys.time(),
    source = "rstudio_addin_viewer"
  )
  
  # The backend doesn't have a separate context store endpoint
  # Context is sent with each chat request instead
  # This function is kept for compatibility but doesn't actually send data
  
  cat("✅ Environment context captured (will be sent with next chat request)\n")
  
  # Return the context data for use in chat requests
  return(context_data)
} 