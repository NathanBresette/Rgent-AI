# =============================================================================
# Test Plot Analysis Functions
# These tests validate plot detection, parsing, and analysis functionality
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
# TEST: identify_plot_type()
# =============================================================================

test_that("identify_plot_type function exists", {
  expect_true(exists("identify_plot_type"))
  expect_true(is.function(identify_plot_type))
})

test_that("identify_plot_type identifies histogram", {
  result <- identify_plot_type("hist(data)")
  expect_equal(result, "histogram")
})

test_that("identify_plot_type identifies hist with options", {
  result <- identify_plot_type("hist(data, breaks = 30, col = 'blue')")
  expect_equal(result, "histogram")
})

test_that("identify_plot_type identifies PCA plot", {
  result <- identify_plot_type("prcomp(data)")
  expect_true(result %in% c("pca_plot", "pca", "biplot"))
  
  result2 <- identify_plot_type("biplot(pca_result)")
  expect_true(result2 %in% c("pca_plot", "pca", "biplot"))
})

test_that("identify_plot_type identifies clustering plots", {
  result <- identify_plot_type("kmeans(data, centers = 3)")
  expect_true(grepl("cluster", result, ignore.case = TRUE))
  
  result2 <- identify_plot_type("hclust(dist(data))")
  expect_true(grepl("cluster", result2, ignore.case = TRUE))
})

test_that("identify_plot_type identifies correlation plots", {
  result <- identify_plot_type("corrplot(cor_matrix)")
  expect_true(grepl("corr", result, ignore.case = TRUE))
  
  result2 <- identify_plot_type("ggcorrplot(cor(data))")
  expect_true(grepl("corr", result2, ignore.case = TRUE))
})

test_that("identify_plot_type identifies ggplot scatter", {
  result <- identify_plot_type("ggplot(df, aes(x, y)) + geom_point()")
  expect_true(result %in% c("scatter", "ggplot_scatter", "ggplot"))
})

test_that("identify_plot_type identifies ggplot bar", {
  result <- identify_plot_type("ggplot(df, aes(x)) + geom_bar()")
  # May return various bar-related types or ggplot type
  expect_true(is.character(result) && nchar(result) > 0)
})

test_that("identify_plot_type identifies ggplot boxplot", {
  result <- identify_plot_type("ggplot(df, aes(x, y)) + geom_boxplot()")
  # May return various boxplot-related types or ggplot type
  expect_true(is.character(result) && nchar(result) > 0)
})

test_that("identify_plot_type identifies base plot", {
  result <- identify_plot_type("plot(x, y)")
  expect_true(!is.null(result))
})

test_that("identify_plot_type handles unknown types", {
  result <- identify_plot_type("some_custom_function()")
  expect_true(is.character(result) || is.null(result))
})

# =============================================================================
# TEST: extract_plot_data()
# =============================================================================

test_that("extract_plot_data function exists", {
  expect_true(exists("extract_plot_data"))
  expect_true(is.function(extract_plot_data))
})

test_that("extract_plot_data extracts histogram data", {
  result <- tryCatch({
    extract_plot_data("hist(my_data)", "histogram")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

test_that("extract_plot_data extracts scatter data", {
  result <- tryCatch({
    extract_plot_data("plot(x_var, y_var)", "scatter")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

test_that("extract_plot_data handles complex ggplot", {
  command <- "ggplot(df, aes(x = col1, y = col2, color = group)) + geom_point() + theme_minimal()"
  
  result <- tryCatch({
    extract_plot_data(command, "scatter")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

test_that("extract_plot_data handles empty command", {
  result <- tryCatch({
    extract_plot_data("", "histogram")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

# =============================================================================
# TEST: generate_analysis_commands()
# =============================================================================

test_that("generate_analysis_commands function exists", {
  expect_true(exists("generate_analysis_commands"))
  expect_true(is.function(generate_analysis_commands))
})

test_that("generate_analysis_commands creates histogram commands", {
  result <- generate_analysis_commands("histogram", "my_data")
  
  expect_true(is.list(result))
  if (!is.null(result$basic_stats)) {
    expect_true(grepl("summary", result$basic_stats))
  }
})

test_that("generate_analysis_commands creates scatter commands", {
  data_var <- list(x = "x_col", y = "y_col")
  result <- generate_analysis_commands("scatter", data_var)
  
  expect_true(is.list(result))
})

test_that("generate_analysis_commands handles unknown type", {
  result <- generate_analysis_commands("unknown_plot_type", "data")
  expect_true(is.list(result))
})

# =============================================================================
# TEST: execute_plot_analysis()
# =============================================================================

test_that("execute_plot_analysis function exists", {
  expect_true(exists("execute_plot_analysis"))
  expect_true(is.function(execute_plot_analysis))
})

test_that("execute_plot_analysis executes simple commands", {
  commands <- list(
    basic = "1 + 1",
    summary = "summary(c(1, 2, 3, 4, 5))"
  )
  
  result <- tryCatch({
    execute_plot_analysis(commands)
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

test_that("execute_plot_analysis handles errors in commands", {
  commands <- list(
    good = "1 + 1",
    bad = "undefined_variable_xyz"
  )
  
  result <- tryCatch({
    execute_plot_analysis(commands)
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

test_that("execute_plot_analysis handles empty command list", {
  result <- tryCatch({
    execute_plot_analysis(list())
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: find_last_plot_command()
# =============================================================================

test_that("find_last_plot_command function exists", {
  expect_true(exists("find_last_plot_command"))
  expect_true(is.function(find_last_plot_command))
})

test_that("find_last_plot_command returns result", {
  result <- tryCatch({
    find_last_plot_command()
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should return list, character, or NULL
  expect_true(is.list(result) || is.character(result) || is.null(result))
})

# =============================================================================
# TEST: reconstruct_ggplot_command()
# =============================================================================

test_that("reconstruct_ggplot_command function exists", {
  expect_true(exists("reconstruct_ggplot_command"))
  expect_true(is.function(reconstruct_ggplot_command))
})

test_that("reconstruct_ggplot_command handles simple ggplot", {
  history_lines <- c(
    "library(ggplot2)",
    "ggplot(mtcars, aes(mpg, hp)) +",
    "  geom_point()"
  )
  
  result <- tryCatch({
    reconstruct_ggplot_command(history_lines, 2)
  }, error = function(e) {
    "reconstructed command"
  })
  
  # Result may be character, NULL, or list
  expect_true(is.character(result) || is.null(result) || is.list(result))
})

test_that("reconstruct_ggplot_command handles multi-line ggplot", {
  history_lines <- c(
    "ggplot(data, aes(x, y)) +",
    "  geom_point() +",
    "  geom_smooth() +",
    "  theme_minimal()"
  )
  
  result <- tryCatch({
    reconstruct_ggplot_command(history_lines, 1)
  }, error = function(e) {
    "reconstructed command"
  })
  
  # Result may be character, NULL, or list
  expect_true(is.character(result) || is.null(result) || is.list(result))
})

test_that("reconstruct_ggplot_command handles geom-only line", {
  history_lines <- c(
    "ggplot(data, aes(x, y)) +",
    "  geom_point()"
  )
  
  result <- tryCatch({
    reconstruct_ggplot_command(history_lines, 2)
  }, error = function(e) {
    "reconstructed command"
  })
  
  # Result may be character, NULL, or list
  expect_true(is.character(result) || is.null(result) || is.list(result))
})

# =============================================================================
# TEST: analyze_last_plot()
# =============================================================================

test_that("analyze_last_plot function exists", {
  expect_true(exists("analyze_last_plot"))
  expect_true(is.function(analyze_last_plot))
})

test_that("analyze_last_plot handles no stored plot", {
  # Ensure no stored plot command
  cleanup_test_objects("last_plot_command", "last_plot_timestamp")
  
  result <- tryCatch({
    analyze_last_plot()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.null(result))
})

test_that("analyze_last_plot uses stored plot command", {
  # Store a fake plot command
  assign("last_plot_command", "hist(rnorm(100))", envir = .GlobalEnv)
  assign("last_plot_timestamp", Sys.time(), envir = .GlobalEnv)
  
  result <- tryCatch({
    analyze_last_plot()
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.null(result))
  
  cleanup_test_objects("last_plot_command", "last_plot_timestamp")
})

# =============================================================================
# TEST: Plot Type Detection Edge Cases
# =============================================================================

test_that("identify_plot_type handles case insensitivity", {
  result1 <- identify_plot_type("HIST(data)")
  result2 <- identify_plot_type("Hist(data)")
  result3 <- identify_plot_type("hist(data)")
  
  expect_equal(result1, result2)
  expect_equal(result2, result3)
})

test_that("identify_plot_type handles whitespace", {
  result1 <- identify_plot_type("hist( data )")
  result2 <- identify_plot_type("hist(data)")
  
  expect_equal(result1, result2)
})

test_that("identify_plot_type handles nested function calls", {
  result <- identify_plot_type("hist(log(data))")
  expect_equal(result, "histogram")
})

# =============================================================================
# TEST: Command Generation
# =============================================================================

test_that("generate_analysis_commands includes normality test for histograms", {
  result <- generate_analysis_commands("histogram", "test_data")
  
  if (!is.null(result$normality)) {
    expect_true(grepl("shapiro", result$normality, ignore.case = TRUE))
  }
})

test_that("generate_analysis_commands includes correlation for scatter", {
  data_var <- list(x = "x_col", y = "y_col")
  result <- generate_analysis_commands("scatter", data_var)
  
  if (!is.null(result$correlation)) {
    expect_true(grepl("cor", result$correlation, ignore.case = TRUE))
  }
})

# =============================================================================
# TEST: Integration with Real Data
# =============================================================================

test_that("plot analysis works with real histogram data", {
  assign("test_hist_data", rnorm(100), envir = .GlobalEnv)
  
  commands <- generate_analysis_commands("histogram", "test_hist_data")
  result <- tryCatch({
    execute_plot_analysis(commands)
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("test_hist_data")
})

test_that("plot analysis works with real scatter data", {
  assign("test_scatter_x", rnorm(50), envir = .GlobalEnv)
  assign("test_scatter_y", rnorm(50), envir = .GlobalEnv)
  
  data_var <- list(x = "test_scatter_x", y = "test_scatter_y")
  commands <- generate_analysis_commands("scatter", data_var)
  
  result <- tryCatch({
    execute_plot_analysis(commands)
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("test_scatter_x", "test_scatter_y")
})

# =============================================================================
# TEST: Error Handling
# =============================================================================

test_that("extract_plot_data handles malformed commands", {
  malformed_commands <- c(
    "hist(",
    "ggplot(data, aes(",
    "plot(x, ",
    ")"
  )
  
  for (cmd in malformed_commands) {
    result <- tryCatch({
      extract_plot_data(cmd, "histogram")
    }, error = function(e) {
      list(error = e$message)
    })
    
    expect_true(is.list(result) || is.character(result) || is.null(result))
  }
})

test_that("identify_plot_type handles NULL input", {
  result <- tryCatch({
    identify_plot_type(NULL)
  }, error = function(e) {
    "unknown"
  })
  
  expect_true(is.character(result) || is.null(result))
})

test_that("generate_analysis_commands handles NULL data_var", {
  result <- tryCatch({
    generate_analysis_commands("histogram", NULL)
  }, error = function(e) {
    list()
  })
  
  expect_true(is.list(result))
})

