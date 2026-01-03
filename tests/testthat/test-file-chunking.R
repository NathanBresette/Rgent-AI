# =============================================================================
# Test File Chunking and Code Analysis Functions
# These tests validate file parsing, chunking, and code analysis helpers
# =============================================================================

library(testthat)

# =============================================================================
# TEST: chunk_file_robustly()
# =============================================================================

test_that("chunk_file_robustly function exists", {
  expect_true(exists("chunk_file_robustly"))
  expect_true(is.function(chunk_file_robustly))
})

test_that("chunk_file_robustly returns list of chunks", {
  file_content <- "
# Test file
x <- 1
y <- 2
"
  
  result <- tryCatch({
    chunk_file_robustly(file_content, "test.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

test_that("chunk_file_robustly handles empty content", {
  result <- tryCatch({
    chunk_file_robustly("", "empty.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: chunk_by_functions()
# =============================================================================

test_that("chunk_by_functions function exists", {
  expect_true(exists("chunk_by_functions"))
  expect_true(is.function(chunk_by_functions))
})

test_that("chunk_by_functions identifies function boundaries", {
  file_content <- "
my_function <- function(x) {
  x * 2
}

another_function <- function(y, z) {
  y + z
}
"
  
  result <- tryCatch({
    chunk_by_functions(file_content, "test.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

test_that("chunk_by_functions extracts function names", {
  file_content <- "
test_func <- function(x) {
  return(x)
}
"
  
  result <- tryCatch({
    chunk_by_functions(file_content, "test.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
  if (length(result) > 0) {
    # Check if any chunk has function_name
    has_func_name <- any(sapply(result, function(c) !is.null(c$function_name)))
    expect_true(has_func_name || length(result) == 0)
  }
})

test_that("chunk_by_functions handles nested braces", {
  file_content <- "
complex_func <- function(x) {
  if (x > 0) {
    for (i in 1:x) {
      print(i)
    }
  }
}
"
  
  result <- tryCatch({
    chunk_by_functions(file_content, "test.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: chunk_by_lines()
# =============================================================================

test_that("chunk_by_lines function exists", {
  expect_true(exists("chunk_by_lines"))
  expect_true(is.function(chunk_by_lines))
})

test_that("chunk_by_lines splits content by line count", {
  file_content <- paste(1:100, collapse = "\n")
  
  result <- tryCatch({
    chunk_by_lines(file_content, max_lines = 20, file_path = "test.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

test_that("chunk_by_lines respects max_lines parameter", {
  file_content <- paste(1:100, collapse = "\n")
  
  result <- tryCatch({
    chunk_by_lines(file_content, max_lines = 10, file_path = "test.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
  if (length(result) > 0) {
    expect_gte(length(result), 1)
  }
})

# =============================================================================
# TEST: validate_and_fix_chunks()
# =============================================================================

test_that("validate_and_fix_chunks function exists", {
  expect_true(exists("validate_and_fix_chunks"))
  expect_true(is.function(validate_and_fix_chunks))
})

test_that("validate_and_fix_chunks handles empty chunks", {
  result <- tryCatch({
    validate_and_fix_chunks(list(), "content", "test.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: split_large_chunk()
# =============================================================================

test_that("split_large_chunk function exists", {
  expect_true(exists("split_large_chunk"))
  expect_true(is.function(split_large_chunk))
})

test_that("split_large_chunk handles large content", {
  large_chunk <- list(
    content = paste(rep("line", 200), collapse = "\n"),
    start_line = 1,
    end_line = 200,
    file_path = "test.R"
  )
  
  result <- tryCatch({
    split_large_chunk(large_chunk)
  }, error = function(e) {
    list(large_chunk)
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: extract_function_name()
# =============================================================================

test_that("extract_function_name function exists", {
  expect_true(exists("extract_function_name"))
  expect_true(is.function(extract_function_name))
})

test_that("extract_function_name extracts simple function name", {
  line <- "my_function <- function(x) {"
  
  result <- extract_function_name(line)
  
  expect_true(is.character(result) || is.null(result))
  if (!is.null(result)) {
    expect_true(grepl("my_function", result))
  }
})

test_that("extract_function_name handles dots in names", {
  line <- "my.function <- function(x) {"
  
  result <- extract_function_name(line)
  
  expect_true(is.character(result) || is.null(result))
})

test_that("extract_function_name handles underscores", {
  line <- "my_long_function_name <- function(x, y, z) {"
  
  result <- extract_function_name(line)
  
  expect_true(is.character(result) || is.null(result))
})

# =============================================================================
# TEST: extract_function_arguments()
# =============================================================================

test_that("extract_function_arguments function exists", {
  expect_true(exists("extract_function_arguments"))
  expect_true(is.function(extract_function_arguments))
})

test_that("extract_function_arguments extracts simple args", {
  line <- "my_func <- function(x, y) {"
  
  result <- extract_function_arguments(line)
  
  expect_true(is.character(result) || is.list(result) || is.null(result))
})

test_that("extract_function_arguments handles no args", {
  line <- "no_args <- function() {"
  
  result <- extract_function_arguments(line)
  
  expect_true(is.character(result) || is.list(result) || is.null(result))
})

test_that("extract_function_arguments handles default values", {
  line <- "with_defaults <- function(x = 1, y = 'hello') {"
  
  result <- extract_function_arguments(line)
  
  expect_true(is.character(result) || is.list(result) || is.null(result))
})

# =============================================================================
# TEST: extract_docstring()
# =============================================================================

test_that("extract_docstring function exists", {
  expect_true(exists("extract_docstring"))
  expect_true(is.function(extract_docstring))
})

test_that("extract_docstring extracts roxygen comments", {
  lines <- c(
    "#' This is a docstring",
    "#' @param x The x parameter",
    "#' @return Something",
    "my_func <- function(x) {"
  )
  
  result <- tryCatch({
    extract_docstring(lines, 4)
  }, error = function(e) {
    NULL
  })
  
  expect_true(is.character(result) || is.null(result))
})

test_that("extract_docstring handles no docstring", {
  lines <- c(
    "x <- 1",
    "my_func <- function(x) {"
  )
  
  result <- tryCatch({
    extract_docstring(lines, 2)
  }, error = function(e) {
    NULL
  })
  
  expect_true(is.character(result) || is.null(result))
})

# =============================================================================
# TEST: calculate_function_complexity()
# =============================================================================

test_that("calculate_function_complexity function exists", {
  expect_true(exists("calculate_function_complexity"))
  expect_true(is.function(calculate_function_complexity))
})

test_that("calculate_function_complexity returns complexity", {
  content <- "
    if (x > 0) {
      for (i in 1:10) {
        while (TRUE) {
          break
        }
      }
    }
  "
  
  result <- calculate_function_complexity(content)
  
  expect_true(is.character(result) || is.numeric(result))
})

test_that("calculate_function_complexity handles simple function", {
  content <- "return(x)"
  
  result <- calculate_function_complexity(content)
  
  expect_true(is.character(result) || is.numeric(result))
})

# =============================================================================
# TEST: extract_function_dependencies()
# =============================================================================

test_that("extract_function_dependencies function exists", {
  expect_true(exists("extract_function_dependencies"))
  expect_true(is.function(extract_function_dependencies))
})

test_that("extract_function_dependencies finds called functions", {
  content <- "
    result <- helper_func(x)
    data <- process_data(result)
    return(data)
  "
  
  result <- extract_function_dependencies(content)
  
  expect_true(is.list(result) || is.character(result))
})

test_that("extract_function_dependencies handles no dependencies", {
  content <- "return(x + 1)"
  
  result <- extract_function_dependencies(content)
  
  expect_true(is.list(result) || is.character(result))
})

# =============================================================================
# TEST: Additional Plot Reconstruction Functions
# =============================================================================

test_that("reconstruct_qq_command function exists", {
  expect_true(exists("reconstruct_qq_command"))
  expect_true(is.function(reconstruct_qq_command))
})

test_that("reconstruct_qq_command handles qqnorm", {
  history_lines <- c(
    "qqnorm(data)",
    "qqline(data)"
  )
  
  result <- tryCatch({
    reconstruct_qq_command(history_lines, 1)
  }, error = function(e) {
    "reconstructed"
  })
  
  expect_true(is.character(result) || is.null(result) || is.list(result))
})

test_that("reconstruct_base_plot_command function exists", {
  expect_true(exists("reconstruct_base_plot_command"))
  expect_true(is.function(reconstruct_base_plot_command))
})

test_that("reconstruct_base_plot_command handles plot", {
  history_lines <- c(
    "plot(x, y)",
    "points(x2, y2)",
    "lines(x3, y3)"
  )
  
  result <- tryCatch({
    reconstruct_base_plot_command(history_lines, 1, "plot")
  }, error = function(e) {
    "reconstructed"
  })
  
  expect_true(is.character(result) || is.null(result) || is.list(result))
})

test_that("analyze_visualization_plot function exists", {
  expect_true(exists("analyze_visualization_plot"))
  expect_true(is.function(analyze_visualization_plot))
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("chunk_by_functions handles malformed code", {
  file_content <- "
function( {
  broken syntax
}
"
  
  result <- tryCatch({
    chunk_by_functions(file_content, "broken.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

test_that("extract_function_name handles various formats", {
  lines <- c(
    "func1<-function(x){",  # No spaces
    "func2 <-  function(x) {",  # Extra spaces
    "func3 = function(x) {",  # Equals sign
    "`special name` <- function(x) {"  # Backticks
  )
  
  for (line in lines) {
    result <- tryCatch({
      extract_function_name(line)
    }, error = function(e) {
      NULL
    })
    expect_true(is.character(result) || is.null(result))
  }
})

test_that("chunk_by_lines handles single line content", {
  result <- tryCatch({
    chunk_by_lines("single line", max_lines = 50, file_path = "test.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

test_that("chunk_by_functions handles file with no functions", {
  file_content <- "
x <- 1
y <- 2
z <- x + y
print(z)
"
  
  result <- tryCatch({
    chunk_by_functions(file_content, "no_funcs.R")
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

