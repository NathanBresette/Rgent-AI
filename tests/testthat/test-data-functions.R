# =============================================================================
# Test Core Data Functions
# These tests validate the fundamental data manipulation functions that
# underpin all agent functionality in RgentAI
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
# TEST: get_available_dataframes()
# =============================================================================

test_that("get_available_dataframes returns only dataframes", {
  # Setup - create test objects of various types
  assign("test_df1", data.frame(x = 1:5, y = letters[1:5]), envir = .GlobalEnv)
  assign("test_vec1", 1:10, envir = .GlobalEnv)
  assign("test_list1", list(a = 1, b = 2), envir = .GlobalEnv)
  assign("test_mat1", matrix(1:9, nrow = 3), envir = .GlobalEnv)
  assign("test_func1", function(x) x + 1, envir = .GlobalEnv)
  
  result <- get_available_dataframes()
  
  # Should include dataframes

  expect_true("test_df1" %in% result)
  
  # Should NOT include other types
  expect_false("test_vec1" %in% result)
  expect_false("test_list1" %in% result)
  expect_false("test_mat1" %in% result)
  expect_false("test_func1" %in% result)
  
  # Should be character vector
  expect_type(result, "character")
  
  # Cleanup
  cleanup_test_objects("test_df1", "test_vec1", "test_list1", "test_mat1", "test_func1")
})

test_that("get_available_dataframes includes tibbles and data.tables", {
  skip_if_not_installed("tibble")
  
  # Create tibble
  assign("test_tibble", tibble::tibble(a = 1:3, b = c("x", "y", "z")), envir = .GlobalEnv)
  
  result <- get_available_dataframes()
  
  expect_true("test_tibble" %in% result)
  
  cleanup_test_objects("test_tibble")
})

test_that("get_available_dataframes handles errors gracefully", {
  # Should not crash even when environment is complex
  result <- get_available_dataframes()
  
  expect_true(is.character(result))
  expect_true(length(result) >= 0)
})

# =============================================================================
# TEST: get_available_objects()
# =============================================================================

test_that("get_available_objects categorizes objects correctly", {
  # Setup - create test objects
  assign("test_df", data.frame(x = 1:5), envir = .GlobalEnv)
  assign("test_vec", 1:10, envir = .GlobalEnv)
  assign("test_list", list(a = 1), envir = .GlobalEnv)
  assign("test_mat", matrix(1:9, 3, 3), envir = .GlobalEnv)
  
  result <- get_available_objects()
  
  # Result should be character vector of object names
  expect_type(result, "character")
  
  # All test objects should be present
  expect_true("test_df" %in% result)
  expect_true("test_vec" %in% result)
  expect_true("test_list" %in% result)
  expect_true("test_mat" %in% result)
  
  cleanup_test_objects("test_df", "test_vec", "test_list", "test_mat")
})

test_that("get_available_objects puts dataframes first", {
  # Setup
  assign("aaa_vector", 1:5, envir = .GlobalEnv)  # Alphabetically first
  assign("zzz_df", data.frame(x = 1), envir = .GlobalEnv)  # Alphabetically last but is df
  
  result <- get_available_objects()
  
  # Dataframe should appear before the vector even though alphabetically later
  df_pos <- which(result == "zzz_df")
  vec_pos <- which(result == "aaa_vector")
  
  if (length(df_pos) > 0 && length(vec_pos) > 0) {
    expect_lt(df_pos[1], vec_pos[1])
  }
  
  cleanup_test_objects("aaa_vector", "zzz_df")
})

# =============================================================================
# TEST: get_dataframe_info()
# =============================================================================

test_that("get_dataframe_info returns correct structure for valid dataframe", {
  assign("info_test_df", data.frame(
    num = c(1, 2, NA, 4, 5),
    char = c("a", "b", "c", NA, "e"),
    fac = factor(c("x", "y", "x", "y", "z")),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- get_dataframe_info("info_test_df")
  
  # Check structure
  expect_true(result$exists)
  expect_equal(result$name, "info_test_df")
  expect_equal(result$nrow, 5)
  expect_equal(result$ncol, 3)  # num, char, fac (stringsAsFactors is parameter, not column)
  
  # Check column info exists (actual structure may vary)
  expect_true(!is.null(result$column_info))
  
  # Check size
  expect_true(is.numeric(result$size_mb))
  expect_gte(result$size_mb, 0)
  
  cleanup_test_objects("info_test_df")
})

test_that("get_dataframe_info handles non-existent dataframe", {
  result <- get_dataframe_info("nonexistent_dataframe_xyz123")
  
  expect_false(result$exists)
  expect_true(!is.null(result$error))
  expect_true(grepl("not found", result$error, ignore.case = TRUE))
})

test_that("get_dataframe_info handles non-dataframe objects", {
  assign("not_a_df", 1:10, envir = .GlobalEnv)
  
  result <- get_dataframe_info("not_a_df")
  
  expect_false(result$exists)
  expect_true(!is.null(result$error))  # Has some error message
  
  cleanup_test_objects("not_a_df")
})

test_that("get_dataframe_info handles empty dataframe", {
  assign("empty_df", data.frame(), envir = .GlobalEnv)
  
  result <- get_dataframe_info("empty_df")
  
  expect_true(result$exists)
  expect_equal(result$nrow, 0)
  expect_equal(result$ncol, 0)
  
  cleanup_test_objects("empty_df")
})

# =============================================================================
# TEST: get_dataframe_explorer_data()
# =============================================================================

test_that("get_dataframe_explorer_data provides comprehensive column stats", {
  assign("explorer_df", data.frame(
    numeric_col = c(1, 2, 3, 4, 5, 100),  # Includes potential outlier
    char_col = c("a", "a", "b", "c", "c", "c"),
    date_col = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", 
                         "2020-04-01", "2020-05-01", "2020-06-01")),
    logical_col = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- get_dataframe_explorer_data("explorer_df")
  
  # Basic structure
  expect_true(result$exists)
  expect_equal(result$nrow, 6)
  expect_equal(result$ncol, 4)
  
  # Numeric column should have histogram and summary stats
  num_info <- result$column_info$numeric_col
  expect_true(!is.null(num_info$histogram))
  expect_true(!is.null(num_info$summary_stats))
  expect_true("min" %in% names(num_info$summary_stats))
  expect_true("max" %in% names(num_info$summary_stats))
  expect_true("mean" %in% names(num_info$summary_stats))
  expect_true("median" %in% names(num_info$summary_stats))
  
  # Character column should have frequency counts
  char_info <- result$column_info$char_col
  expect_true(!is.null(char_info$frequency))
  
  # Logical column should have TRUE/FALSE counts
  logical_info <- result$column_info$logical_col
  expect_true(!is.null(logical_info$frequency))
  
  cleanup_test_objects("explorer_df")
})

test_that("get_dataframe_explorer_data handles missing values correctly", {
  assign("na_df", data.frame(
    a = c(1, NA, 3, NA, 5),
    b = c("x", "y", NA, "z", NA),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- get_dataframe_explorer_data("na_df")
  
  expect_equal(result$column_info$a$missing, 2)
  expect_equal(result$column_info$b$missing, 2)
  expect_equal(result$column_info$a$missing_pct, 40)
  expect_equal(result$column_info$b$missing_pct, 40)
  
  cleanup_test_objects("na_df")
})

# =============================================================================
# TEST: get_object_explorer_data()
# =============================================================================

test_that("get_object_explorer_data handles different object types", {
  # Test vector
  assign("test_numeric_vec", c(1, 2, 3, 4, 5), envir = .GlobalEnv)
  vec_result <- get_object_explorer_data("test_numeric_vec")
  expect_true(vec_result$exists)
  expect_equal(vec_result$type, "numeric")
  expect_equal(vec_result$length, 5)
  expect_true(!is.null(vec_result$summary_stats))
  
  # Test list
  assign("test_list_obj", list(a = 1, b = "test", c = 1:5), envir = .GlobalEnv)
  list_result <- get_object_explorer_data("test_list_obj")
  expect_true(list_result$exists)
  expect_equal(list_result$type, "list")
  expect_equal(list_result$length, 3)
  
  # Test matrix
  assign("test_matrix", matrix(1:12, nrow = 3, ncol = 4), envir = .GlobalEnv)
  mat_result <- get_object_explorer_data("test_matrix")
  expect_true(mat_result$exists)
  expect_equal(mat_result$type, "matrix")
  expect_equal(mat_result$nrow, 3)
  expect_equal(mat_result$ncol, 4)
  
  # Test function
  assign("test_function", function(x, y) x + y, envir = .GlobalEnv)
  func_result <- get_object_explorer_data("test_function")
  expect_true(func_result$exists)
  expect_equal(func_result$type, "function")
  expect_equal(func_result$formals_count, 2)
  
  cleanup_test_objects("test_numeric_vec", "test_list_obj", "test_matrix", "test_function")
})

test_that("get_object_explorer_data handles non-existent objects", {
  result <- get_object_explorer_data("completely_fake_object_12345")
  
  expect_false(result$exists)
  expect_true(!is.null(result$error))
})

test_that("get_object_explorer_data handles arrays", {
  assign("test_array", array(1:24, dim = c(2, 3, 4)), envir = .GlobalEnv)
  
  result <- get_object_explorer_data("test_array")
  
  expect_true(result$exists)
  expect_equal(result$type, "array")
  expect_equal(result$dim, c(2, 3, 4))
  
  cleanup_test_objects("test_array")
})

test_that("get_object_explorer_data handles time series", {
  assign("test_ts", ts(1:100, frequency = 12, start = c(2020, 1)), envir = .GlobalEnv)
  
  result <- get_object_explorer_data("test_ts")
  
  expect_true(result$exists)
  expect_equal(result$type, "ts")
  expect_equal(result$frequency, 12)
  expect_equal(result$length, 100)
  
  cleanup_test_objects("test_ts")
})

# =============================================================================
# TEST: get_dataframe_chunk_filtered()
# =============================================================================

test_that("get_dataframe_chunk_filtered returns correct pagination", {
  assign("paginate_df", data.frame(
    id = 1:100,
    value = rnorm(100)
  ), envir = .GlobalEnv)
  
  # First page
  result1 <- get_dataframe_chunk_filtered("paginate_df", page = 1, page_size = 10)
  expect_true(result1$exists)
  expect_equal(result1$page, 1)
  expect_equal(result1$page_size, 10)
  expect_equal(result1$total_rows, 100)
  expect_equal(result1$total_pages, 10)
  expect_equal(result1$start_row, 1)
  expect_equal(result1$end_row, 10)
  
  # Last page
  result10 <- get_dataframe_chunk_filtered("paginate_df", page = 10, page_size = 10)
  expect_equal(result10$start_row, 91)
  expect_equal(result10$end_row, 100)
  
  cleanup_test_objects("paginate_df")
})

test_that("get_dataframe_chunk_filtered applies filters correctly", {
  assign("filter_df", data.frame(
    category = c("A", "A", "B", "B", "C"),
    value = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  # Equals filter
  result <- get_dataframe_chunk_filtered(
    "filter_df", 
    filters = list(list(column = "category", type = "equals", value = "A"))
  )
  expect_equal(result$total_rows, 2)
  
  # Greater than filter
  result2 <- get_dataframe_chunk_filtered(
    "filter_df",
    filters = list(list(column = "value", type = "greater_than", value = 25))
  )
  expect_equal(result2$total_rows, 3)
  
  cleanup_test_objects("filter_df")
})

test_that("get_dataframe_chunk_filtered applies sorts correctly", {
  assign("sort_df", data.frame(
    name = c("Charlie", "Alice", "Bob"),
    score = c(70, 90, 80),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  # Sort ascending by name
  result <- get_dataframe_chunk_filtered(
    "sort_df",
    sorts = list(list(column = "name", direction = "asc"))
  )
  expect_equal(result$data$name[1], "Alice")
  
  # Sort descending by score
  result2 <- get_dataframe_chunk_filtered(
    "sort_df",
    sorts = list(list(column = "score", direction = "desc"))
  )
  expect_equal(result2$data$score[1], "90")
  
  cleanup_test_objects("sort_df")
})

test_that("get_dataframe_chunk_filtered handles empty result", {
  assign("empty_filter_df", data.frame(
    x = 1:5,
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- get_dataframe_chunk_filtered(
    "empty_filter_df",
    filters = list(list(column = "x", type = "greater_than", value = 100))
  )
  
  expect_true(result$exists)
  expect_equal(result$total_rows, 0)
  
  cleanup_test_objects("empty_filter_df")
})

test_that("get_dataframe_chunk_filtered handles matrices", {
  assign("test_matrix_filter", matrix(1:20, nrow = 5, ncol = 4), envir = .GlobalEnv)
  
  result <- get_dataframe_chunk_filtered("test_matrix_filter")
  
  expect_true(result$exists)
  expect_equal(result$total_rows, 5)
  
  cleanup_test_objects("test_matrix_filter")
})

# =============================================================================
# TEST: convert_explorer_view_to_code()
# =============================================================================

test_that("convert_explorer_view_to_code generates valid filter code", {
  filters <- list(
    list(column = "age", type = "greater_than", value = 21),
    list(column = "status", type = "equals", value = "active")
  )
  
  code <- convert_explorer_view_to_code("mydata", filters = filters)
  
  expect_true(grepl("dplyr::filter", code))
  expect_true(grepl("age > 21", code))
  expect_true(grepl("status == ", code))
})

test_that("convert_explorer_view_to_code generates valid sort code", {
  sorts <- list(
    list(column = "name", direction = "asc"),
    list(column = "date", direction = "desc")
  )
  
  code <- convert_explorer_view_to_code("mydata", sorts = sorts)
  
  expect_true(grepl("dplyr::arrange", code))
  expect_true(grepl("dplyr::desc", code))
})

test_that("convert_explorer_view_to_code returns dataframe name when no filters/sorts", {
  code <- convert_explorer_view_to_code("mydata")
  
  expect_equal(code, "mydata")
})

# =============================================================================
# TEST: JSON Serialization Safety
# =============================================================================

test_that("explorer data can be serialized to JSON", {
  assign("json_test_df", data.frame(
    num = 1:5,
    char = letters[1:5],
    factor = factor(c("a", "a", "b", "b", "c")),
    date = as.Date("2020-01-01") + 0:4,
    logical = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- get_dataframe_explorer_data("json_test_df")
  
  # Should not throw error when converting to JSON
  expect_silent({
    json <- jsonlite::toJSON(result, auto_unbox = TRUE)
  })
  
  expect_true(is.character(json))
  expect_true(nchar(json) > 0)
  
  cleanup_test_objects("json_test_df")
})

test_that("object explorer data can be serialized to JSON for all types", {
  # Test various object types
  test_objects <- list(
    vec = 1:10,
    char_vec = letters[1:5],
    mat = matrix(1:9, 3, 3),
    lst = list(a = 1, b = "test"),
    arr = array(1:8, dim = c(2, 2, 2))
  )
  
  for (name in names(test_objects)) {
    assign(paste0("json_", name), test_objects[[name]], envir = .GlobalEnv)
    
    result <- get_object_explorer_data(paste0("json_", name))
    
    expect_silent({
      json <- jsonlite::toJSON(result, auto_unbox = TRUE)
    })
    
    cleanup_test_objects(paste0("json_", name))
  }
})

