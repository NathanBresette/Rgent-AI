#' Visualization Agent for RStudio AI
#' 
#' This agent generates intelligent visualizations based on data characteristics
#' and user preferences, with plots displayed directly in the chat interface.

# Helper function to replace %||% operator
`%||%` <- function(x, y) if (is.null(x) || (is.character(x) && x == "")) y else x

#' Start the visualization agent workflow
#' @param dataframe Name of the dataframe to visualize
#' @param options List of visualization options including configured plots
#' @return List containing workflow configuration
#' @export
start_visualization_agent <- function(dataframe, options) {
  # Validate inputs
  if (is.null(dataframe) || dataframe == "") {
    return(list(
      success = FALSE,
      error = "No dataframe selected"
    ))
  }
  
  if (is.null(options$plots) || length(options$plots) == 0) {
    return(list(
      success = FALSE,
      error = "No plots configured"
    ))
  }
  
  # Get dataframe info
  tryCatch({
    df <- get(dataframe, envir = .GlobalEnv)
    
    # Analyze data structure
    data_info <- analyze_data_structure(df)
    
    # Create workflow steps based on configured plots
    workflow_steps <- create_visualization_workflow_from_plots(options$plots, data_info)
    
    return(list(
      success = TRUE,
      dataframe = dataframe,
      options = options,
      data_info = data_info,
      workflow_steps = workflow_steps,
      total_steps = length(workflow_steps),
      current_step = 1,
      message = "Visualization agent started successfully"
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error accessing dataframe:", e$message)
    ))
  })
}

#' Get the next visualization step
#' @param step Current step number
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables (legacy parameter)
#' @param options List of visualization options including configured plots
#' @return List containing step information and code
#' @export
get_next_visualization_step <- function(step, dataframe, variables, options) {
  
  tryCatch({
    # Get the plot configuration for this step
    plot_config <- get_plot_config_for_step(step, options$plots)
    
    if (is.null(plot_config)) {
      return(list(
        success = FALSE,
        error = "No more visualization steps"
      ))
    }
    
    # Generate code for this plot configuration
    plot_code <- generate_plot_code_from_config(plot_config, dataframe, options)
    
    # Get plot type (handle both old and new formats)
    plot_type <- if (!is.null(plot_config$plotType)) {
      plot_config$plotType
    } else if (!is.null(plot_config$types)) {
      plot_config$types[1] # Use first type if multiple
    } else {
      "unknown"
    }
    
    return(list(
      success = TRUE,
      step = step,
      plot_type = plot_type,
      plot_config = plot_config,
      code = plot_code,
      description = get_plot_description_from_config(plot_config)
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error generating visualization step:", e$message)
    ))
  })
}

#' Execute a visualization step
#' @param step Step number
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables (legacy parameter)
#' @param options List of visualization options including configured plots
#' @return List containing execution results
#' @export
execute_visualization_step <- function(step, dataframe, variables, options) {
  cat("Executing visualization step:", step, "\n")
  
  tryCatch({
    # Get the plot configuration for this step
    plot_config <- get_plot_config_for_step(step, options$plots)
    
    if (is.null(plot_config)) {
      return(list(
        success = FALSE,
        error = "Invalid step number"
      ))
    }
    
    # Generate and execute plot code
    plot_code <- generate_plot_code_from_config(plot_config, dataframe, options)
    
    # Create variables object from plot config for execute_plot_code
    variables <- list(
      x_var = if (!is.null(plot_config$xVar) && plot_config$xVar != "") plot_config$xVar else "",
      y_var = if (!is.null(plot_config$yVar) && plot_config$yVar != "") plot_config$yVar else "",
      color_var = if (!is.null(plot_config$colorVar) && plot_config$colorVar != "") plot_config$colorVar else ""
    )
    
    # Execute the code
    result <- execute_plot_code(plot_code, dataframe, variables)
    
    if (result$success) {
      # Handle both old format (types) and new format (plotType)
      plot_type <- if (!is.null(plot_config$plotType)) plot_config$plotType else plot_config$types[1]
      
      return(list(
        success = TRUE,
        step = step,
        plot_type = plot_type,
        plot_config = plot_config,
        plot_data = result$plot_data,
        description = result$description,
        code = plot_code,
        message = paste("Generated", plot_type, "successfully")
      ))
    } else {
      return(list(
        success = FALSE,
        error = result$error
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error executing visualization step:", e$message)
    ))
  })
}

#' Analyze data structure for visualization planning
#' @param df Dataframe to analyze
#' @return List containing data structure information
analyze_data_structure <- function(df) {
  cat("Analyzing data structure for visualization planning\n")
  
  # Get basic info
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  
  # Analyze variable types
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  
  # Get variable details
  var_details <- list()
  for (var in names(df)) {
    var_details[[var]] <- list(
      type = class(df[[var]])[1],
      unique_values = length(unique(df[[var]])),
      missing_values = sum(is.na(df[[var]])),
      is_numeric = is.numeric(df[[var]]),
      is_categorical = is.factor(df[[var]]) || is.character(df[[var]])
    )
  }
  
  return(list(
    n_rows = n_rows,
    n_cols = n_cols,
    numeric_vars = numeric_vars,
    categorical_vars = categorical_vars,
    var_details = var_details
  ))
}

#' Check if a plot type is appropriate for the given variables and data
#' @param plot_type Type of plot to check
#' @param variables List of selected variables
#' @param data_info Data structure information
#' @return Logical indicating if plot is appropriate
is_plot_appropriate <- function(plot_type, variables, data_info) {
  # For now, assume all plot types are appropriate
  # This could be enhanced with more sophisticated logic
  return(TRUE)
}

#' Create visualization workflow steps
#' @param plot_types List of selected plot types
#' @param variables List of selected variables
#' @param data_info Data structure information
#' @return List of workflow steps
create_visualization_workflow <- function(plot_types, variables, data_info) {
  cat("📋 Creating visualization workflow\n")
  
  steps <- list()
  step_num <- 1
  
  for (plot_type in plot_types) {
    # Determine if this plot type is appropriate for the data
    if (is_plot_appropriate(plot_type, variables, data_info)) {
      steps[[step_num]] <- list(
        step = step_num,
        plot_type = plot_type,
        description = get_plot_description(plot_type, variables)
      )
      step_num <- step_num + 1
    }
  }
  
  return(steps)
}

#' Check if a plot type is appropriate for the given data
#' @param plot_type Type of plot to check
#' @param variables Selected variables
#' @param data_info Data structure information
#' @return Boolean indicating if plot is appropriate
is_plot_appropriate <- function(plot_type, variables, data_info) {
  # Basic appropriateness checks
  if (plot_type %in% c("scatter", "line") && 
      (!is.null(variables$x_var) && !is.null(variables$y_var))) {
    return(TRUE)
  }
  
  if (plot_type %in% c("histogram", "density") && 
      (!is.null(variables$x_var))) {
    return(TRUE)
  }
  
  if (plot_type %in% c("boxplot", "violin") && 
      (!is.null(variables$x_var) && !is.null(variables$y_var))) {
    return(TRUE)
  }
  
  if (plot_type %in% c("bar", "pie") && 
      (!is.null(variables$x_var))) {
    return(TRUE)
  }
  
  return(TRUE) # Default to true for now
}

#' Get plot type for a specific step
#' @param step Step number
#' @param plot_types List of selected plot types
#' @return Plot type for the step
get_plot_type_for_step <- function(step, plot_types) {
  if (step <= length(plot_types)) {
    return(plot_types[[step]])
  }
  return(NULL)
}

#' Generate R code for a specific plot type
#' @param plot_type Type of plot to generate
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for the plot
generate_plot_code <- function(plot_type, dataframe, variables, options) {
  cat("Generating code for", plot_type, "plot\n")
  
  # Load required libraries
  code <- "library(ggplot2)\nlibrary(base64enc)\n\n"
  
  # Generate plot-specific code
  switch(plot_type,
    "scatter" = {
      code <- paste0(code, generate_scatter_code(dataframe, variables, options))
    },
    "histogram" = {
      code <- paste0(code, generate_histogram_code(dataframe, variables, options))
    },
    "boxplot" = {
      code <- paste0(code, generate_boxplot_code(dataframe, variables, options))
    },
    "bar" = {
      code <- paste0(code, generate_bar_code(dataframe, variables, options))
    },
    "line" = {
      code <- paste0(code, generate_line_code(dataframe, variables, options))
    },
    "density" = {
      code <- paste0(code, generate_density_code(dataframe, variables, options))
    },
    "violin" = {
      code <- paste0(code, generate_violin_code(dataframe, variables, options))
    },
    "pie" = {
      code <- paste0(code, generate_pie_code(dataframe, variables, options))
    },
    "trend" = {
      code <- paste0(code, generate_trend_code(dataframe, variables, options))
    },
    "correlation" = {
      code <- paste0(code, generate_correlation_code(dataframe, variables, options))
    },
    "qqplot" = {
      code <- paste0(code, generate_qqplot_code(dataframe, variables, options))
    },
    "residual" = {
      code <- paste0(code, generate_residual_code(dataframe, variables, options))
    },
    {
      # Default to scatter plot
      code <- paste0(code, generate_scatter_code(dataframe, variables, options))
    }
  )
  
  # Add plot capture code
  capture_code <- "
# Capture plot as base64 without displaying in plot pane
temp_file <- tempfile(fileext = '.png')

# Create plot without displaying it
grDevices::png(filename = temp_file, width = 800, height = 600)
# Use invisible to suppress plot display
invisible(print(p))
grDevices::dev.off()

plot_base64 <- base64enc::base64encode(temp_file)
unlink(temp_file)

# Return plot data
cat('PLOT_DATA:', plot_base64)
cat('\nANALYZE_PLOT:TRUE')
"
  
  return(paste0(code, capture_code))
}

#' Generate scatter plot code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for scatter plot
generate_scatter_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Scatter Plot:", variables$x_var, "vs", variables$y_var)
  
  paste0("
# Create scatter plot
p <- ggplot(", dataframe, ", aes(x = ", variables$x_var, ", y = ", variables$y_var, ")) +
  geom_point(size = 2, alpha = 0.7, color = 'steelblue') +
  theme_minimal() +
  labs(title = '", title_value, "',
       x = '", variables$x_var, "',
       y = '", variables$y_var, "')

# Display plot
print(p)
")
}

#' Generate histogram code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for histogram
generate_histogram_code <- function(dataframe, variables, options) {
  bins_value <- if (!is.null(options$bins)) options$bins else 30
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Distribution of", variables$x_var)
  
  paste0("
# Check if variable is numeric or categorical
var_data <- ", dataframe, "[['", variables$x_var, "']]
is_numeric_var <- is.numeric(var_data)

if (is_numeric_var) {
  # Create histogram for numeric variable
  p <- ggplot(", dataframe, ", aes(x = ", variables$x_var, ")) +
    geom_histogram(bins = ", bins_value, ", fill = 'skyblue', alpha = 0.7, color = 'white') +
    theme_minimal() +
    labs(title = '", title_value, "',
         x = '", variables$x_var, "',
         y = 'Count')
} else {
  # Create bar chart for categorical variable
  p <- ggplot(", dataframe, ", aes(x = ", variables$x_var, ")) +
    geom_bar(fill = 'skyblue', alpha = 0.7, color = 'white') +
    theme_minimal() +
    labs(title = '", title_value, "',
         x = '", variables$x_var, "',
         y = 'Count') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Display plot
print(p)
")
}

#' Generate boxplot code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for boxplot
generate_boxplot_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Boxplot:", variables$y_var, "by", variables$x_var)
  
  paste0("
# Create boxplot
p <- ggplot(", dataframe, ", aes(x = ", variables$x_var, ", y = ", variables$y_var, ")) +
  geom_boxplot(fill = 'lightblue', alpha = 0.7) +
  theme_minimal() +
  labs(title = '", title_value, "',
       x = '", variables$x_var, "',
       y = '", variables$y_var, "')

# Display plot
print(p)
")
}

#' Generate bar chart code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for bar chart
generate_bar_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Bar Chart of", variables$x_var)
  
  paste0("
# Create bar chart
p <- ggplot(", dataframe, ", aes(x = ", variables$x_var, ")) +
  geom_bar(fill = 'steelblue', alpha = 0.7) +
  theme_minimal() +
  labs(title = '", title_value, "',
       x = '", variables$x_var, "',
       y = 'Count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display plot
print(p)
")
}

#' Generate line plot code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for line plot
generate_line_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Line Plot:", variables$y_var, "vs", variables$x_var)
  
  paste0("
# Create line plot
p <- ggplot(", dataframe, ", aes(x = ", variables$x_var, ", y = ", variables$y_var, ")) +
  geom_line(linewidth = 1, color = 'steelblue') +
  geom_point(size = 2, color = 'steelblue') +
  theme_minimal() +
  labs(title = '", title_value, "',
       x = '", variables$x_var, "',
       y = '", variables$y_var, "')

# Display plot
print(p)
")
}

#' Generate density plot code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for density plot
generate_density_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Density Plot of", variables$x_var)
  
  paste0("
# Create density plot
p <- ggplot(", dataframe, ", aes(x = ", variables$x_var, ")) +
  geom_density(fill = 'skyblue', alpha = 0.7) +
  theme_minimal() +
  labs(title = '", title_value, "',
       x = '", variables$x_var, "',
       y = 'Density')

# Display plot
print(p)
")
}

#' Generate violin plot code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for violin plot
generate_violin_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Violin Plot:", variables$y_var, "by", variables$x_var)
  
  paste0("
# Create violin plot
p <- ggplot(", dataframe, ", aes(x = ", variables$x_var, ", y = ", variables$y_var, ")) +
  geom_violin(fill = 'lightblue', alpha = 0.7) +
  theme_minimal() +
  labs(title = '", title_value, "',
       x = '", variables$x_var, "',
       y = '", variables$y_var, "')

# Display plot
print(p)
")
}

#' Generate pie chart code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for pie chart
generate_pie_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Pie Chart of", variables$x_var)
  
  paste0("
# Load required libraries
library(dplyr)

# Create pie chart
# First, count the values for the pie chart
pie_data <- ", dataframe, " %>%
  count(", variables$x_var, ") %>%
  mutate(percentage = n / sum(n) * 100)

# Create pie chart using ggplot2
p <- ggplot(pie_data, aes(x = '', y = n, fill = ", variables$x_var, ")) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar(theta = 'y') +
  theme_void() +
  labs(title = '", title_value, "') +
  theme(legend.position = 'right')

# Display plot
print(p)
")
}

#' Generate trend analysis code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for trend analysis
generate_trend_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Trend Analysis:", variables$y_var, "vs", variables$x_var)
  
  paste0("
# Load required libraries
library(dplyr)

# Create trend analysis plot
# First, ensure data is sorted by x variable
trend_data <- ", dataframe, " %>%
  arrange(", variables$x_var, ")

# Create trend plot with smoothed line
p <- ggplot(trend_data, aes(x = ", variables$x_var, ", y = ", variables$y_var, ")) +
  geom_point(size = 2, alpha = 0.6, color = 'steelblue') +
  geom_line(linewidth = 1, color = 'steelblue', alpha = 0.8) +
  geom_smooth(method = 'loess', se = TRUE, color = 'red', linewidth = 1.5) +
  theme_minimal() +
  labs(title = '", title_value, "',
       x = '", variables$x_var, "',
       y = '", variables$y_var, "',
       subtitle = 'Red line shows trend (loess smoothing)')

# Display plot
print(p)
")
}

#' Generate correlation plot code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for correlation plot
generate_correlation_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Correlation Plot of", variables$x_var)
  
  paste0("
# Load required libraries
library(corrplot)

# Create correlation plot
# First, get numeric variables for correlation
numeric_vars <- ", dataframe, "[, sapply(", dataframe, ", is.numeric)]
cor_matrix <- cor(numeric_vars, use = 'complete.obs')

# Create correlation plot
p <- corrplot(cor_matrix, 
              method = 'color', 
              type = 'upper', 
              order = 'hclust',
              tl.cex = 0.8,
              tl.col = 'black',
              tl.srt = 45,
              addCoef.col = 'black',
              number.cex = 0.7)

# Display plot
print(p)
")
}

#' Generate Q-Q plot code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for Q-Q plot
generate_qqplot_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Q-Q Plot of", variables$x_var)
  
  paste0("
# Create Q-Q plot for normality testing
p <- ggplot(", dataframe, ", aes(sample = ", variables$x_var, ")) +
  stat_qq() +
  stat_qq_line(color = 'red', linewidth = 1) +
  theme_minimal() +
  labs(title = '", title_value, "',
       x = 'Theoretical Quantiles',
       y = 'Sample Quantiles',
       subtitle = 'Red line indicates perfect normality')

# Display plot
print(p)
")
}

#' Generate residual plot code
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @param options List of visualization options
#' @return R code string for residual plot
generate_residual_code <- function(dataframe, variables, options) {
  title_value <- if (!is.null(options$title) && options$title != "") options$title else paste("Residual Plot of", variables$y_var, "vs", variables$x_var)
  
  paste0("
# Load required libraries
library(dplyr)

# Create residual plot
# Fit a linear model
model <- lm(", variables$y_var, " ~ ", variables$x_var, ", data = ", dataframe, ")

# Create residual plot
residual_data <- ", dataframe, " %>%
  mutate(
    fitted = fitted(model),
    residuals = residuals(model)
  )

p <- ggplot(residual_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.7, color = 'steelblue') +
  geom_hline(yintercept = 0, color = 'red', linewidth = 1) +
  geom_smooth(se = TRUE, color = 'blue', linewidth = 1) +
  theme_minimal() +
  labs(title = '", title_value, "',
       x = 'Fitted Values',
       y = 'Residuals',
       subtitle = 'Red line at y=0, blue line shows trend')

# Display plot
print(p)
")
}

#' Execute plot code and capture results
#' @param plot_code R code to execute
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables
#' @return List containing execution results
execute_plot_code <- function(plot_code, dataframe, variables) {
  cat("Executing plot code\n")
  
  tryCatch({
    # Execute the code with plot suppression (same as execution engine)
    output <- utils::capture.output({
      # Execute the code
      eval(parse(text = plot_code))
    })
    
    # The code should have created a plot_base64 variable
    if (exists("plot_base64")) {
      return(list(
        success = TRUE,
        plot_data = plot_base64,
        description = paste("Generated plot for", dataframe)
      ))
    } else {
      return(list(
        success = FALSE,
        error = "Plot data not captured - plot_base64 variable not found"
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error executing plot code:", e$message)
    ))
  })
}

#' Get description for a plot type
#' @param plot_type Type of plot
#' @param variables List of selected variables
#' @return Description string
get_plot_description <- function(plot_type, variables) {
  switch(plot_type,
    "scatter" = paste("Scatter plot of", variables$x_var, "vs", variables$y_var),
    "histogram" = paste("Histogram of", variables$x_var),
    "boxplot" = paste("Boxplot of", variables$y_var, "by", variables$x_var),
    "bar" = paste("Bar chart of", variables$x_var),
    "line" = paste("Line plot of", variables$y_var, "vs", variables$x_var),
    "density" = paste("Density plot of", variables$x_var),
    "violin" = paste("Violin plot of", variables$y_var, "by", variables$x_var),
    "pie" = paste("Pie chart of", variables$x_var),
    "trend" = paste("Trend analysis of", variables$y_var, "vs", variables$x_var),
    "correlation" = paste("Correlation plot of", variables$x_var),
    "qqplot" = paste("Q-Q plot of", variables$x_var),
    "residual" = paste("Residual plot of", variables$y_var, "vs", variables$x_var),
    paste("Plot of", variables$x_var)
  )
}

#' Format visualization results for display
#' @param step_result Result from visualization step
#' @return Formatted result string
format_visualization_results <- function(step_result) {
  if (step_result$success) {
    return(paste0(
      "Generated ", step_result$plot_type, " plot successfully.\n",
      "Description: ", step_result$description
    ))
  } else {
    return(paste0("Error: ", step_result$error))
  }
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create visualization workflow from individual plot configurations
#' @param plots List of configured plots
#' @param data_info Data structure information
#' @return List of workflow steps
create_visualization_workflow_from_plots <- function(plots, data_info) {
  steps <- list()
  step_num <- 1
  
  # Handle data.frame vs list
  if (is.data.frame(plots)) {
    plot_indices <- seq_len(nrow(plots))
  } else {
    plot_indices <- seq_along(plots)
  }
  
  for (i in plot_indices) {
    if (is.data.frame(plots)) {
      plot_config <- as.list(plots[i, ])
    } else {
      plot_config <- plots[[i]]
    }
    
    # Handle both old format (with $types) and new format (individual plots)
    if (!is.null(plot_config$types)) {
      # Old format: multiple types in one config
      for (plot_type in plot_config$types) {
        steps[[step_num]] <- list(
          step = step_num,
          plot_type = plot_type,
          plot_config = plot_config,
          description = get_plot_description_from_config(plot_config)
        )
        step_num <- step_num + 1
      }
    } else if (!is.null(plot_config$plotType)) {
      # New format: individual plot with single plotType
      tryCatch({
        steps[[step_num]] <- list(
          step = step_num,
          plot_type = plot_config$plotType,
          plot_config = plot_config,
          description = get_plot_description_from_config(plot_config)
        )
        step_num <- step_num + 1
      }, error = function(e) {
        # Silently handle errors
      })
    }
  }
  
  return(steps)
}

#' Get plot configuration for a specific step
#' @param step Step number
#' @param plots List or data.frame of configured plots
#' @return Plot configuration for the step
get_plot_config_for_step <- function(step, plots) {
  # Handle data.frame vs list
  if (is.data.frame(plots)) {
    if (step <= nrow(plots)) {
      return(as.list(plots[step, ]))
    }
  } else {
    if (step <= length(plots)) {
      return(plots[[step]])
    }
  }
  return(NULL)
}

#' Generate R code from plot configuration
#' @param plot_config Plot configuration object
#' @param dataframe Name of the dataframe
#' @param options List of visualization options
#' @return R code string for the plot
generate_plot_code_from_config <- function(plot_config, dataframe, options) {
  cat("Generating code from plot configuration\n")
  
  # Load required libraries
  code <- "library(ggplot2)\nlibrary(base64enc)\n\n"
  
  # Handle both old format (types) and new format (plotType)
  if (!is.null(plot_config$types)) {
    # Old format: multiple types
    plot_types <- plot_config$types
  } else if (!is.null(plot_config$plotType)) {
    # New format: single plot type
    plot_types <- plot_config$plotType
  } else {
    plot_types <- "histogram" # Default fallback
  }
  
  # Generate code for each plot type in this configuration
  plot_codes <- c()
  for (plot_type in plot_types) {
    plot_codes <- c(plot_codes, generate_plot_code_by_type(plot_type, plot_config, dataframe, options))
  }
  
  # Combine all plot codes
  code <- paste0(code, paste(plot_codes, collapse = "\n\n"))
  
  # Add plot capture code
  capture_code <- "
# Capture plot as base64 without displaying in plot pane
temp_file <- tempfile(fileext = '.png')

# Create plot without displaying it
grDevices::png(filename = temp_file, width = 800, height = 600)
# Use invisible to suppress plot display
invisible(print(p))
grDevices::dev.off()

plot_base64 <- base64enc::base64encode(temp_file)
unlink(temp_file)

# Store plot information for analysis
last_plot_command <- deparse(substitute(p))
last_plot_timestamp <- Sys.time()
assign('last_plot_command', last_plot_command, envir = .GlobalEnv)
assign('last_plot_timestamp', last_plot_timestamp, envir = .GlobalEnv)

# Return plot data
cat('PLOT_DATA:', plot_base64)
cat('\nANALYZE_PLOT:TRUE')
"
  
  return(paste0(code, capture_code))
}

#' Generate plot code by type with individual configuration
#' @param plot_type Type of plot to generate
#' @param plot_config Plot configuration object
#' @param dataframe Name of the dataframe
#' @param options List of visualization options
#' @return R code string for the plot
generate_plot_code_by_type <- function(plot_type, plot_config, dataframe, options) {
  
  # Create variables object from plot config
  variables <- list(
    x_var = if (!is.null(plot_config$xVar) && plot_config$xVar != "") plot_config$xVar else "",
    y_var = if (!is.null(plot_config$yVar) && plot_config$yVar != "") plot_config$yVar else "",
    color_var = if (!is.null(plot_config$colorVar) && plot_config$colorVar != "") plot_config$colorVar else ""
  )
  
  # Create options object from plot config
  plot_options <- list(
    title = if (!is.null(plot_config$title) && plot_config$title != "") plot_config$title else "",
    bins = if (!is.null(plot_config$bins)) plot_config$bins else 30
  )
  
  # Generate plot-specific code
  switch(plot_type,
    "scatter" = {
      generate_scatter_code(dataframe, variables, plot_options)
    },
    "histogram" = {
      generate_histogram_code(dataframe, variables, plot_options)
    },
    "boxplot" = {
      generate_boxplot_code(dataframe, variables, plot_options)
    },
    "bar" = {
      generate_bar_code(dataframe, variables, plot_options)
    },
    "line" = {
      generate_line_code(dataframe, variables, plot_options)
    },
    "density" = {
      generate_density_code(dataframe, variables, plot_options)
    },
    "violin" = {
      generate_violin_code(dataframe, variables, plot_options)
    },
    "pie" = {
      generate_pie_code(dataframe, variables, plot_options)
    },
    "trend" = {
      generate_trend_code(dataframe, variables, plot_options)
    },
    "correlation" = {
      generate_correlation_code(dataframe, variables, plot_options)
    },
    "qqplot" = {
      generate_qqplot_code(dataframe, variables, plot_options)
    },
    "residual" = {
      generate_residual_code(dataframe, variables, plot_options)
    },
    {
      # Default to scatter plot
      generate_scatter_code(dataframe, variables, plot_options)
    }
  )
}

#' Get plot description from configuration
#' @param plot_config Plot configuration object
#' @return Description string
get_plot_description_from_config <- function(plot_config) {
  # Handle both old format (with $types) and new format (individual plots)
  if (!is.null(plot_config$types)) {
    # Old format: multiple types
    plot_types_text <- paste(plot_config$types, collapse = ", ")
  } else if (!is.null(plot_config$plotType)) {
    # New format: single plot type
    plot_types_text <- plot_config$plotType
  } else {
    plot_types_text <- "plot"
  }
  
  # Safely check for xVar and yVar fields
  xVar <- if (is.list(plot_config) && "xVar" %in% names(plot_config)) plot_config$xVar else NULL
  yVar <- if (is.list(plot_config) && "yVar" %in% names(plot_config)) plot_config$yVar else NULL
  
  if (!is.null(xVar) && xVar != "") {
    if (!is.null(yVar) && yVar != "") {
      return(paste(plot_types_text, "of", yVar, "vs", xVar))
    } else {
      return(paste(plot_types_text, "of", xVar))
    }
  }
  return(paste("Generated", plot_types_text))
}

#' Generate visualization step code (for execution engine)
#' @param step Step number
#' @param dataframe Name of the dataframe
#' @param variables List of selected variables (legacy parameter)
#' @param options List of visualization options including configured plots
#' @return R code string for the visualization step
#' @export
generate_visualization_step_code <- function(step, dataframe, variables, options) {
  cat("Generating visualization step code for step:", step, "\n")
  
  tryCatch({
    # Get the plot configuration for this step
    plot_config <- get_plot_config_for_step(step, options$plots)
    
    if (is.null(plot_config)) {
      return("# Error: Invalid step number")
    }
    
    # Generate plot code
    plot_code <- generate_plot_code_from_config(plot_config, dataframe, options)
    
    return(plot_code)
    
  }, error = function(e) {
    return(paste("# Error generating visualization code:", e$message))
  })
}
