#!/usr/bin/env Rscript

# Build script for rstudioai package
# Automatically increments version and rebuilds package

# Read current DESCRIPTION
desc_file <- "DESCRIPTION"
desc_lines <- readLines(desc_file)

# Find and increment version
version_line_idx <- grep("^Version:", desc_lines)
if (length(version_line_idx) == 0) {
  stop("Version line not found in DESCRIPTION")
}

current_version <- desc_lines[version_line_idx]
version_match <- regmatches(current_version, regexec("Version: (\\d+)\\.(\\d+)\\.(\\d+)", current_version))[[1]]

if (length(version_match) < 4) {
  stop("Could not parse version number")
}

major <- as.numeric(version_match[2])
minor <- as.numeric(version_match[3])
patch <- as.numeric(version_match[4])

# Increment patch version
patch <- patch + 1
new_version <- paste(major, minor, patch, sep = ".")

cat("Updating version from", gsub("Version: ", "", current_version), "to", new_version, "\n")

# Update DESCRIPTION
desc_lines[version_line_idx] <- paste("Version:", new_version)
writeLines(desc_lines, desc_file)

# Build package
cat("Building package...\n")
build_result <- system("R CMD build .", intern = TRUE)
if (length(build_result) > 0) {
  cat(paste(build_result, collapse = "\n"), "\n")
}

# Check if build was successful
if (file.exists(paste0("rstudioai_", new_version, ".tar.gz"))) {
  cat("✅ Package built successfully: rstudioai_", new_version, ".tar.gz\n", sep = "")
  
  # Copy to Desktop
  desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
  if (dir.exists(desktop_path)) {
    file.copy(paste0("rstudioai_", new_version, ".tar.gz"), 
              file.path(desktop_path, paste0("rstudioai_", new_version, ".tar.gz")), 
              overwrite = TRUE)
    cat("✅ Copied to Desktop\n")
  }
  
  cat("\n📦 Install command:\n")
  cat("install.packages(\"~/Desktop/rstudioai_", new_version, ".tar.gz\", repos = NULL, type = \"source\")\n", sep = "")
  
} else {
  cat("❌ Package build failed\n")
  quit(status = 1)
}
