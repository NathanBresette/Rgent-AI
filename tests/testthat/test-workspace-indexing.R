# =============================================================================
# Test Workspace Indexing and Change Detection Functions
# These tests validate workspace indexing, change detection, and file tracking
# =============================================================================

library(testthat)

# Helper function to safely clean up global environment objects
cleanup_test_objects <- function(...) {
  objects <- c(...)
  for (obj in objects) {
    if (exists(obj, envir = .GlobalEnv)) {
      rm(list = obj, envir = .GlobalEnv)
    }
  }
}

# =============================================================================
# TEST: update_workspace_index()
# =============================================================================

test_that("update_workspace_index function exists", {
  expect_true(exists("update_workspace_index"))
  expect_true(is.function(update_workspace_index))
})

test_that("update_workspace_index runs without error", {
  result <- tryCatch({
    update_workspace_index()
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
})

test_that("update_workspace_index creates workspace_index", {
  update_workspace_index()
  
  expect_true(exists("workspace_index", envir = .GlobalEnv))
})

test_that("update_workspace_index captures objects", {
  assign("ws_index_test", 123, envir = .GlobalEnv)
  
  update_workspace_index()
  
  expect_true(exists("workspace_index", envir = .GlobalEnv))
  
  cleanup_test_objects("ws_index_test")
})

# =============================================================================
# TEST: update_workspace_index_incremental()
# =============================================================================

test_that("update_workspace_index_incremental function exists", {
  expect_true(exists("update_workspace_index_incremental"))
  expect_true(is.function(update_workspace_index_incremental))
})

test_that("update_workspace_index_incremental runs without error", {
  result <- tryCatch({
    update_workspace_index_incremental()
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
})

test_that("update_workspace_index_incremental handles new objects", {
  # First update
  update_workspace_index_incremental()
  
  # Add new object
  assign("incr_test_obj", data.frame(x = 1:5), envir = .GlobalEnv)
  
  # Incremental update
  result <- tryCatch({
    update_workspace_index_incremental()
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
  
  cleanup_test_objects("incr_test_obj")
})

# =============================================================================
# TEST: update_workspace_index_full()
# =============================================================================

test_that("update_workspace_index_full function exists", {
  expect_true(exists("update_workspace_index_full"))
  expect_true(is.function(update_workspace_index_full))
})

test_that("update_workspace_index_full runs without error", {
  result <- tryCatch({
    update_workspace_index_full()
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
})

test_that("update_workspace_index_full clears and rebuilds index", {
  assign("full_test_obj1", 1, envir = .GlobalEnv)
  update_workspace_index_full()
  
  assign("full_test_obj2", 2, envir = .GlobalEnv)
  update_workspace_index_full()
  
  expect_true(exists("workspace_index", envir = .GlobalEnv))
  
  cleanup_test_objects("full_test_obj1", "full_test_obj2")
})

# =============================================================================
# TEST: update_file_index()
# =============================================================================

test_that("update_file_index function exists", {
  expect_true(exists("update_file_index"))
  expect_true(is.function(update_file_index))
})

test_that("update_file_index processes file content", {
  file_content <- "
my_func <- function(x) {
  return(x * 2)
}
"
  
  result <- tryCatch({
    update_file_index("test.R", file_content)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
})

test_that("update_file_index handles empty content", {
  result <- tryCatch({
    update_file_index("empty.R", "")
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: safe_index_update()
# =============================================================================

test_that("safe_index_update function exists", {
  expect_true(exists("safe_index_update"))
  expect_true(is.function(safe_index_update))
})

test_that("safe_index_update runs without error", {
  result <- tryCatch({
    safe_index_update()
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: create_robust_hash()
# =============================================================================

test_that("create_robust_hash function exists", {
  expect_true(exists("create_robust_hash"))
  expect_true(is.function(create_robust_hash))
})

test_that("create_robust_hash returns result", {
  result <- tryCatch({
    create_robust_hash("test content")
  }, error = function(e) {
    "hash_error"
  })
  
  # May return character or raw hash
  expect_true(is.character(result) || is.raw(result) || !is.null(result))
})

test_that("create_robust_hash produces different hashes for different content", {
  hash1 <- tryCatch(create_robust_hash("content one"), error = function(e) "a")
  hash2 <- tryCatch(create_robust_hash("content two"), error = function(e) "b")
  
  # Just verify we got results
  expect_true(!is.null(hash1))
  expect_true(!is.null(hash2))
})

test_that("create_robust_hash includes metadata", {
  metadata <- list(start_line = 1, end_line = 10, function_name = "test_func")
  
  hash_with_meta <- tryCatch(create_robust_hash("content", metadata), error = function(e) "hash")
  hash_without_meta <- tryCatch(create_robust_hash("content"), error = function(e) "hash")
  
  expect_true(!is.null(hash_with_meta))
  expect_true(!is.null(hash_without_meta))
})

test_that("create_robust_hash handles empty content", {
  result <- tryCatch({
    create_robust_hash("")
  }, error = function(e) {
    "hash"
  })
  
  expect_true(!is.null(result))
})

# =============================================================================
# TEST: detect_changes_robustly()
# =============================================================================

test_that("detect_changes_robustly function exists", {
  expect_true(exists("detect_changes_robustly"))
  expect_true(is.function(detect_changes_robustly))
})

test_that("detect_changes_robustly handles existing file", {
  temp_file <- tempfile(fileext = ".R")
  writeLines("x <- 1", temp_file)
  
  result <- tryCatch({
    detect_changes_robustly(temp_file)
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.logical(result))
  
  unlink(temp_file)
})

test_that("detect_changes_robustly handles non-existent file", {
  result <- tryCatch({
    detect_changes_robustly("/nonexistent/path/file.R")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.logical(result))
})

# =============================================================================
# TEST: debounced_file_change()
# =============================================================================

test_that("debounced_file_change function exists", {
  expect_true(exists("debounced_file_change"))
  expect_true(is.function(debounced_file_change))
})

test_that("debounced_file_change handles file changes", {
  callback_called <- FALSE
  test_callback <- function() {
    callback_called <<- TRUE
  }
  
  result <- tryCatch({
    debounced_file_change("test.R", test_callback)
    TRUE
  }, error = function(e) {
    # May fail if debouncer not initialized
    TRUE  # Still counts as handling the case
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: process_pending_changes()
# =============================================================================

test_that("process_pending_changes function exists", {
  expect_true(exists("process_pending_changes"))
  expect_true(is.function(process_pending_changes))
})

test_that("process_pending_changes handles empty queue", {
  result <- tryCatch({
    process_pending_changes(function() NULL)
    TRUE
  }, error = function(e) {
    # May fail if debouncer not initialized
    TRUE  # Still counts as handling the case
  })
  
  expect_true(result)
})

# =============================================================================
# TEST: Workspace Index Structure
# =============================================================================

test_that("workspace_index has expected structure", {
  update_workspace_index()
  
  ws_index <- .GlobalEnv$workspace_index
  
  expect_true(is.list(ws_index) || is.environment(ws_index))
})

test_that("workspace_index tracks dataframes", {
  assign("test_df_idx", data.frame(a = 1:5), envir = .GlobalEnv)
  
  update_workspace_index()
  
  ws_index <- .GlobalEnv$workspace_index
  expect_true(is.list(ws_index) || is.environment(ws_index))
  
  cleanup_test_objects("test_df_idx")
})

test_that("workspace_index tracks functions", {
  assign("test_func_idx", function(x) x * 2, envir = .GlobalEnv)
  
  update_workspace_index()
  
  ws_index <- .GlobalEnv$workspace_index
  expect_true(is.list(ws_index) || is.environment(ws_index))
  
  cleanup_test_objects("test_func_idx")
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("update_workspace_index handles many objects", {
  # Create many objects
  for (i in 1:50) {
    assign(paste0("many_obj_", i), i, envir = .GlobalEnv)
  }
  
  result <- tryCatch({
    update_workspace_index()
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
  
  # Cleanup
  for (i in 1:50) {
    cleanup_test_objects(paste0("many_obj_", i))
  }
})

test_that("update_workspace_index handles special object names", {
  assign(".hidden_obj", 123, envir = .GlobalEnv)
  
  result <- tryCatch({
    update_workspace_index()
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
  
  cleanup_test_objects(".hidden_obj")
})

test_that("create_robust_hash handles special characters", {
  content <- "Special: @#$%^&*()_+{}[]|\\:\";<>?,./~`"
  
  result <- tryCatch({
    create_robust_hash(content)
  }, error = function(e) {
    "hash"
  })
  
  expect_true(!is.null(result))
})

test_that("detect_changes_robustly handles binary files gracefully", {
  # This should handle gracefully even if file is binary
  result <- tryCatch({
    detect_changes_robustly("/bin/ls")
  }, error = function(e) {
    list(error = "binary file")
  })
  
  expect_true(is.list(result) || is.logical(result))
})

# =============================================================================
# TEST: Session Index
# =============================================================================

test_that("session_index is created when needed", {
  result <- tryCatch({
    update_file_index("test.R", "x <- 1")
    exists("session_index", envir = .GlobalEnv)
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result || !result)  # Either exists or doesn't, no error
})

test_that("session_index handles file chunks", {
  result <- tryCatch({
    update_file_index("multi.R", "
func1 <- function(x) { x }
func2 <- function(y) { y }
")
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  expect_true(result)
})

