
# Find an available port for the plumber server (fast, robust)
find_available_port <- function() {
  httpuv::randomPort()
}
