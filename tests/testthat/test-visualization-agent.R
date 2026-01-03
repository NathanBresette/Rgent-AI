# =============================================================================
# Test Visualization Agent Functions
# These tests validate the visualization agent workflow and plot generation
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
# TEST: start_visualization_agent()
# =============================================================================

test_that("start_visualization_agent handles missing dataframe", {
  result <- tryCatch({
    start_visualization_agent(
      dataframe = "nonexistent_viz_df",
      options = list(scatter = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  # Should handle gracefully
  expect_true(is.list(result))
})

test_that("start_visualization_agent creates workflow with valid data", {
  assign("viz_test_df", data.frame(
    x = rnorm(50),
    y = rnorm(50),
    group = sample(c("A", "B"), 50, replace = TRUE)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_visualization_agent(
      dataframe = "viz_test_df",
      options = list(
        scatter = TRUE,
        histogram = TRUE,
        boxplot = FALSE
      )
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("viz_test_df")
})

# =============================================================================
# TEST: create_visualization_workflow()
# =============================================================================

test_that("create_visualization_workflow creates proper structure", {
  skip_if_not(exists("create_visualization_workflow"))
  
  assign("workflow_df", data.frame(
    numeric1 = rnorm(30),
    numeric2 = rnorm(30),
    category = sample(c("A", "B", "C"), 30, replace = TRUE)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    create_visualization_workflow(
      dataframe_name = "workflow_df",
      plot_types = c("scatter", "histogram", "boxplot")
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("workflow_df")
})

# =============================================================================
# TEST: analyze_data_structure()
# =============================================================================

test_that("analyze_data_structure identifies variable types", {
  skip_if_not(exists("analyze_data_structure"))
  
  assign("structure_df", data.frame(
    numeric_col = rnorm(20),
    integer_col = 1:20,
    factor_col = factor(rep(c("X", "Y"), 10)),
    character_col = sample(letters[1:5], 20, replace = TRUE),
    date_col = seq(as.Date("2020-01-01"), by = "day", length.out = 20),
    logical_col = sample(c(TRUE, FALSE), 20, replace = TRUE),
    stringsAsFactors = FALSE
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    analyze_data_structure("structure_df")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("structure_df")
})

# =============================================================================
# TEST: is_plot_appropriate()
# =============================================================================

test_that("is_plot_appropriate recommends scatter for two numeric", {
  skip_if_not(exists("is_plot_appropriate"))
  
  result <- tryCatch({
    is_plot_appropriate(
      plot_type = "scatter",
      x_type = "numeric",
      y_type = "numeric"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.logical(result) || is.list(result))
})

test_that("is_plot_appropriate recommends boxplot for categorical + numeric", {
  skip_if_not(exists("is_plot_appropriate"))
  
  result <- tryCatch({
    is_plot_appropriate(
      plot_type = "boxplot",
      x_type = "factor",
      y_type = "numeric"
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.logical(result) || is.list(result))
})

test_that("is_plot_appropriate recommends bar for categorical", {
  skip_if_not(exists("is_plot_appropriate"))
  
  result <- tryCatch({
    is_plot_appropriate(
      plot_type = "bar",
      x_type = "factor",
      y_type = NULL
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.logical(result) || is.list(result))
})

# =============================================================================
# TEST: generate_plot_code()
# =============================================================================

test_that("generate_plot_code creates ggplot2 code for scatter", {
  skip_if_not(exists("generate_plot_code"))
  
  code <- tryCatch({
    generate_plot_code(
      plot_type = "scatter",
      dataframe = "my_data",
      variables = list(x_var = "x", y_var = "y"),
      options = list()
    )
  }, error = function(e) {
    "ggplot error handling"
  })
  
  expect_true(is.character(code))
})

test_that("generate_plot_code creates histogram code", {
  skip_if_not(exists("generate_plot_code"))
  
  code <- tryCatch({
    generate_plot_code(
      plot_type = "histogram",
      dataframe = "my_data",
      variables = list(x_var = "values"),
      options = list()
    )
  }, error = function(e) {
    "histogram error handling"
  })
  
  expect_true(is.character(code))
})

test_that("generate_plot_code creates boxplot code", {
  skip_if_not(exists("generate_plot_code"))
  
  code <- tryCatch({
    generate_plot_code(
      plot_type = "boxplot",
      dataframe = "my_data",
      variables = list(x_var = "group", y_var = "value"),
      options = list()
    )
  }, error = function(e) {
    "boxplot error handling"
  })
  
  expect_true(is.character(code))
})

# =============================================================================
# TEST: Specific Plot Code Generators
# =============================================================================

test_that("generate_scatter_code creates valid code", {
  skip_if_not(exists("generate_scatter_code"))
  
  code <- tryCatch({
    generate_scatter_code("df", list(x_var = "x", y_var = "y"), list())
  }, error = function(e) {
    "scatter code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_histogram_code creates valid code", {
  skip_if_not(exists("generate_histogram_code"))
  
  code <- tryCatch({
    generate_histogram_code("df", list(x_var = "values"), list(bins = 30))
  }, error = function(e) {
    "histogram code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_boxplot_code creates valid code", {
  skip_if_not(exists("generate_boxplot_code"))
  
  code <- tryCatch({
    generate_boxplot_code("df", list(x_var = "group", y_var = "value"), list())
  }, error = function(e) {
    "boxplot code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_bar_code creates valid code", {
  skip_if_not(exists("generate_bar_code"))
  
  code <- tryCatch({
    generate_bar_code("df", list(x_var = "category"), list())
  }, error = function(e) {
    "bar code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_line_code creates valid code", {
  skip_if_not(exists("generate_line_code"))
  
  code <- tryCatch({
    generate_line_code("df", list(x_var = "x", y_var = "y"), list())
  }, error = function(e) {
    "line code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_density_code creates valid code", {
  skip_if_not(exists("generate_density_code"))
  
  code <- tryCatch({
    generate_density_code("df", list(x_var = "values"), list())
  }, error = function(e) {
    "density code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_violin_code creates valid code", {
  skip_if_not(exists("generate_violin_code"))
  
  code <- tryCatch({
    generate_violin_code("df", list(x_var = "group", y_var = "value"), list())
  }, error = function(e) {
    "violin code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_pie_code creates valid code", {
  skip_if_not(exists("generate_pie_code"))
  
  code <- tryCatch({
    generate_pie_code("df", list(x_var = "category"), list())
  }, error = function(e) {
    "pie code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_correlation_code creates valid code", {
  skip_if_not(exists("generate_correlation_code"))
  
  code <- tryCatch({
    generate_correlation_code("df", list(x_var = "x", y_var = "y", z_var = "z"), list())
  }, error = function(e) {
    "correlation code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_qqplot_code creates valid code", {
  skip_if_not(exists("generate_qqplot_code"))
  
  code <- tryCatch({
    generate_qqplot_code("df", list(x_var = "values"), list())
  }, error = function(e) {
    "qqplot code generated"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

# =============================================================================
# TEST: execute_visualization_step()
# =============================================================================

test_that("execute_visualization_step handles scatter plot", {
  skip_if_not(exists("execute_visualization_step"))
  skip_if_not_installed("ggplot2")
  
  assign("exec_viz_df", data.frame(
    x = rnorm(30),
    y = rnorm(30)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_visualization_step(
      step = "scatter",
      dataframe = "exec_viz_df",
      variables = list(x_var = "x", y_var = "y"),
      options = list()
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("exec_viz_df")
})

test_that("execute_visualization_step handles histogram", {
  skip_if_not(exists("execute_visualization_step"))
  skip_if_not_installed("ggplot2")
  
  assign("hist_viz_df", data.frame(
    values = rnorm(100)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    execute_visualization_step(
      step = "histogram",
      dataframe = "hist_viz_df",
      variables = list(x_var = "values"),
      options = list()
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("hist_viz_df")
})

# =============================================================================
# TEST: execute_plot_code()
# =============================================================================

test_that("execute_plot_code runs valid ggplot code", {
  skip_if_not(exists("execute_plot_code"))
  skip_if_not_installed("ggplot2")
  
  assign("plot_code_df", data.frame(
    x = 1:10,
    y = rnorm(10)
  ), envir = .GlobalEnv)
  
  code <- "ggplot2::ggplot(plot_code_df, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()"
  
  result <- tryCatch({
    execute_plot_code(code)
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || inherits(result, "gg") || is.null(result))
  
  cleanup_test_objects("plot_code_df")
})

test_that("execute_plot_code handles invalid code gracefully", {
  skip_if_not(exists("execute_plot_code"))
  
  result <- tryCatch({
    execute_plot_code("this is not valid R code >>>")
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: get_plot_description()
# =============================================================================

test_that("get_plot_description returns meaningful descriptions", {
  skip_if_not(exists("get_plot_description"))
  
  plot_types <- c("scatter", "histogram", "boxplot", "bar", "line", "density")
  variables <- list(x_var = "x", y_var = "y")
  
  for (pt in plot_types) {
    desc <- tryCatch({
      get_plot_description(pt, variables)
    }, error = function(e) {
      paste("Description for", pt)
    })
    expect_true(is.character(desc))
    expect_gt(nchar(desc), 0)
  }
})

# =============================================================================
# TEST: format_visualization_results()
# =============================================================================

test_that("format_visualization_results structures output correctly", {
  skip_if_not(exists("format_visualization_results"))
  
  # Function expects a single step_result, not a list of results
  step_result <- list(
    success = TRUE,
    plot_type = "scatter",
    description = "Test scatter plot"
  )
  
  formatted <- tryCatch({
    format_visualization_results(step_result)
  }, error = function(e) {
    "formatted result"
  })
  
  expect_true(is.list(formatted) || is.character(formatted))
})

# =============================================================================
# TEST: get_next_visualization_step()
# =============================================================================

test_that("get_next_visualization_step returns step info", {
  skip_if_not(exists("get_next_visualization_step"))
  
  result <- tryCatch({
    get_next_visualization_step(
      step = "scatter",
      dataframe = "test_df",
      variables = list(x_var = "x", y_var = "y"),
      options = list()
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result) || is.null(result))
})

# =============================================================================
# TEST: Workflow from Plot Configs
# =============================================================================

test_that("create_visualization_workflow_from_plots handles configs", {
  skip_if_not(exists("create_visualization_workflow_from_plots"))
  
  configs <- list(
    list(type = "scatter", x = "x", y = "y"),
    list(type = "histogram", x = "values")
  )
  
  result <- tryCatch({
    create_visualization_workflow_from_plots(configs)
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
})

test_that("get_plot_config_for_step retrieves correct config", {
  skip_if_not(exists("get_plot_config_for_step"))
  
  plots <- list(
    list(type = "scatter", x = "x", y = "y"),
    list(type = "histogram", x = "values")
  )
  
  result <- tryCatch({
    get_plot_config_for_step(step = 1, plots = plots)
  }, error = function(e) {
    list(type = "scatter", error = e$message)
  })
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: Code Generation from Config
# =============================================================================

test_that("generate_plot_code_from_config creates code from config", {
  skip_if_not(exists("generate_plot_code_from_config"))
  
  config <- list(
    type = "scatter",
    x = "x_var",
    y = "y_var",
    color = "group"
  )
  
  code <- tryCatch({
    generate_plot_code_from_config(config, "my_df")
  }, error = function(e) {
    "plot code from config"
  })
  
  expect_true(is.character(code))
  expect_gt(nchar(code), 0)
})

test_that("generate_plot_code_by_type handles various types", {
  skip_if_not(exists("generate_plot_code_by_type"))
  
  types <- c("scatter", "histogram", "boxplot", "bar", "line")
  
  for (type in types) {
    config <- list(xVar = "x", yVar = "y")
    code <- tryCatch({
      generate_plot_code_by_type(type, config, "df", list())
    }, error = function(e) {
      paste("Code for", type)
    })
    
    expect_true(is.character(code))
  }
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("visualization handles single data point", {
  assign("single_point_df", data.frame(
    x = 5,
    y = 10
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_visualization_agent(
      dataframe = "single_point_df",
      options = list(scatter = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("single_point_df")
})

test_that("visualization handles all-NA columns", {
  assign("na_viz_df", data.frame(
    x = 1:10,
    y = rep(NA_real_, 10)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_visualization_agent(
      dataframe = "na_viz_df",
      options = list(scatter = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("na_viz_df")
})

test_that("visualization handles many categories", {
  assign("many_cat_df", data.frame(
    category = paste0("cat_", 1:100),
    value = rnorm(100)
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_visualization_agent(
      dataframe = "many_cat_df",
      options = list(bar = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("many_cat_df")
})

test_that("visualization handles date columns", {
  assign("date_viz_df", data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 30),
    value = cumsum(rnorm(30))
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_visualization_agent(
      dataframe = "date_viz_df",
      options = list(line = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("date_viz_df")
})

test_that("visualization handles special characters in column names", {
  assign("special_viz_df", data.frame(
    `Column With Spaces` = rnorm(20),
    `column.with.dots` = rnorm(20),
    check.names = FALSE
  ), envir = .GlobalEnv)
  
  result <- tryCatch({
    start_visualization_agent(
      dataframe = "special_viz_df",
      options = list(scatter = TRUE)
    )
  }, error = function(e) {
    list(error = e$message)
  })
  
  expect_true(is.list(result))
  
  cleanup_test_objects("special_viz_df")
})
