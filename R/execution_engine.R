#' R Code Execution Engine
#' 
#' This module handles all R code execution using proven, reliable methods
#' adapted from ClaudeR's execution approach.

#' Execute R code in the active RStudio session
#'
#' This function executes the provided R code in the global environment
#' and captures both the result and any output using proven methods.
#'
#' @param code The R code to execute
#' @param settings The current settings for execution
#' @return A list containing the execution result and metadata
execute_code_in_session <- function(code, settings = NULL) {
  # Default settings if not provided
  if (is.null(settings)) {
    settings <- list(
      print_to_console = TRUE,
      log_to_file = FALSE,
      log_file_path = file.path(path.expand("~"), "rstudioai_logs.R")
    )
  }

  # Print code to console if enabled
  if (settings$print_to_console) {
    cat("\n### AI executing the following code ###\n")
    cat(code, "\n")
    cat("### End of AI code ###\n\n")
  }

  # Log code to file if enabled
  if (settings$log_to_file && !is.null(settings$log_file_path) && settings$log_file_path != "") {
    log_code_to_file(code, settings$log_file_path)
  }

  # Suppress RStudio viewer for all outputs during auto-execute
  # Save current viewer option and set to NULL to prevent viewer from opening
  original_viewer <- getOption("viewer")
  on.exit({
    # Restore original viewer option after execution
    options(viewer = original_viewer)
  }, add = TRUE)
  options(viewer = NULL)

  # Create a temporary environment for evaluation
  env <- .GlobalEnv

  # Set up plot capture if needed
  plot_file <- NULL
  has_plot <- FALSE

  tryCatch({
    # Create a temporary file for plot capture
    plot_file <- tempfile(fileext = ".png")

    # Check if this is a plot command and store it for analysis
    is_plot_command <- any(grepl("(ggplot|plot|hist|boxplot|barplot|scatterplot|qplot|plotly|leaflet|density|pairs|ggpairs|qqnorm|qqplot|corrplot|ggcorrplot|heatmap|geom_|stat_|coord_|facet_|theme_)", code, ignore.case = TRUE))
    
    if (is_plot_command) {
      # Store the plot command for analysis
      tryCatch({
        assign("last_plot_command", code, envir = .GlobalEnv)
        assign("last_plot_timestamp", Sys.time(), envir = .GlobalEnv)
      }, error = function(e) {
        # Silently fail if storage doesn't work
      })
    }

    # Check for base R plot calls (may be mixed with ggplot)
    base_plot_functions <- c("plot", "boxplot", "hist", "barplot", "pie", "qqnorm", "qqplot", 
                             "pairs", "image", "contour", "persp", "matplot", "curve")
    # Drawing commands that add to plots (should be grouped with the plot)
    plot_drawing_commands <- c("abline", "lines", "points", "legend", "text", "title", "axis", 
                               "grid", "rug", "segments", "arrows", "rect", "polygon", "curve")
    
    # Count plot function calls in the code
    plot_pattern <- paste0("\\b(", paste(base_plot_functions, collapse = "|"), ")\\s*\\(")
    num_base_plots <- length(gregexpr(plot_pattern, code, ignore.case = TRUE)[[1]])
    if (num_base_plots > 0 && gregexpr(plot_pattern, code, ignore.case = TRUE)[[1]][1] == -1) {
      num_base_plots <- 0
    }
    
    # Count ggplot expressions in the code
    ggplot_pattern <- "\\bggplot\\s*\\("
    num_ggplot_plots <- length(gregexpr(ggplot_pattern, code, ignore.case = TRUE)[[1]])
    if (num_ggplot_plots > 0 && gregexpr(ggplot_pattern, code, ignore.case = TRUE)[[1]][1] == -1) {
      num_ggplot_plots <- 0
    }
    
    # If we have base R plots OR multiple ggplot plots, parse and execute them separately
    # This ensures we capture all plots, not just the last one
    if (num_base_plots > 0 || (num_ggplot_plots > 0 && grepl("ggplot", code, ignore.case = TRUE))) {
      # Parse code into expressions and execute each plot separately
      plot_files_multi <- c()
      all_output <- c()
      result <- NULL
      
      # Parse the code into a list of expressions
      parsed_code <- tryCatch({
        parse(text = code)
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(parsed_code)) {
        # Execute expressions, grouping plot expressions with their drawing commands
        i <- 1
        while (i <= length(parsed_code)) {
          expr <- parsed_code[[i]]
          expr_str <- paste(deparse(expr), collapse = " ")
          is_base_plot_expr <- any(sapply(base_plot_functions, function(f) {
            grepl(paste0("\\b", f, "\\s*\\("), expr_str, ignore.case = TRUE)
          }))
          is_ggplot_expr <- grepl("\\bggplot\\s*\\(", expr_str, ignore.case = TRUE)
          
          if (is_base_plot_expr) {
            # This is a plot expression - collect it and any following drawing commands
            plot_exprs <- list(expr)
            # Look ahead for drawing commands that should be grouped with this plot
            j <- i + 1
            while (j <= length(parsed_code)) {
              next_expr <- parsed_code[[j]]
              next_expr_str <- paste(deparse(next_expr), collapse = " ")
              # Check if this is a drawing command
              is_drawing_cmd <- any(sapply(plot_drawing_commands, function(f) {
                grepl(paste0("\\b", f, "\\s*\\("), next_expr_str, ignore.case = TRUE)
              }))
              # Also check if it's another plot expression (if so, stop grouping)
              is_next_plot <- any(sapply(base_plot_functions, function(f) {
                grepl(paste0("\\b", f, "\\s*\\("), next_expr_str, ignore.case = TRUE)
              }))
              
              if (is_drawing_cmd && !is_next_plot) {
                plot_exprs <- c(plot_exprs, list(next_expr))
                j <- j + 1
              } else {
                break
              }
            }
            
            # Execute all plot expressions and drawing commands together
            plot_file_single <- tempfile(fileext = ".png")
            tryCatch({
              # Use png device to capture plot (suppress RStudio graphics pane)
              grDevices::png(filename = plot_file_single, width = 800, height = 600)
              # Suppress any messages/output from plot execution
              suppressMessages({
                for (plot_expr in plot_exprs) {
                  eval(plot_expr, envir = env)
                }
              })
              # Ensure device is properly closed
              dev_num <- grDevices::dev.off()
              # If there are any open devices, close them to prevent RStudio tracking
              while (grDevices::dev.cur() != 1) {
                grDevices::dev.off()
              }
              
              # Check if plot was created
              if (file.exists(plot_file_single)) {
                file_size <- file.info(plot_file_single)$size
                if (!is.null(file_size) && length(file_size) == 1 && file_size > 100) {
                  plot_files_multi <- c(plot_files_multi, plot_file_single)
                } else {
                  unlink(plot_file_single)
                }
              }
            }, error = function(e) {
              # Ensure device is closed on error
              tryCatch({
                while (grDevices::dev.cur() != 1) {
                  grDevices::dev.off()
                }
              }, error = function(e) {})
            })
            
            # Skip the expressions we just processed
            i <- j
          } else if (is_ggplot_expr) {
            # This is a ggplot expression - execute it separately and capture the plot
            plot_file_single <- tempfile(fileext = ".png")
            tryCatch({
              # Execute the ggplot expression and capture the plot object
              expr_result <- withVisible(eval(expr, envir = env))
              
              # If it's a ggplot object, print it to a graphics device
              if (!is.null(expr_result$value) && inherits(expr_result$value, "ggplot")) {
                grDevices::png(filename = plot_file_single, width = 800, height = 600)
                print(expr_result$value)
                grDevices::dev.off()
                
                # Ensure all devices are closed
                while (grDevices::dev.cur() != 1) {
                  grDevices::dev.off()
                }
                
                # Check if plot was created
                if (file.exists(plot_file_single)) {
                  file_size <- file.info(plot_file_single)$size
                  if (!is.null(file_size) && length(file_size) == 1 && file_size > 100) {
                    plot_files_multi <- c(plot_files_multi, plot_file_single)
                  } else {
                    unlink(plot_file_single)
                  }
                }
              }
              
              # Capture any output from the expression (but don't print the plot object again)
              expr_output <- utils::capture.output({
                # Don't print ggplot objects, they're already captured
                if (expr_result$visible && !is.null(expr_result$value) && !inherits(expr_result$value, "ggplot")) {
                  print(expr_result$value)
                }
              })
              all_output <- c(all_output, expr_output)
            }, error = function(e) {
              # Ensure device is closed on error
              tryCatch({
                while (grDevices::dev.cur() != 1) {
                  grDevices::dev.off()
                }
              }, error = function(e) {})
              all_output <- c(all_output, paste("Error:", e$message))
            })
            i <- i + 1
          } else {
            # Regular expression - execute normally
            tryCatch({
              expr_output <- utils::capture.output({
                result <- withVisible(eval(expr, envir = env))
                if (result$visible && !is.null(result$value)) {
                  # Suppress plotly and kable viewer display
                  is_plotly_result <- inherits(result$value, "plotly") || 
                                     (is.list(result$value) && !is.null(result$value$x) && inherits(result$value$x, "plotly")) ||
                                     (requireNamespace("htmlwidgets", quietly = TRUE) && inherits(result$value, "htmlwidget"))
                  is_kable_result <- inherits(result$value, "knitr_kable") || 
                                    (is.character(result$value) && requireNamespace("knitr", quietly = TRUE) && 
                                     !is.null(attr(result$value, "format")) && attr(result$value, "format") %in% c("html", "latex"))
                  if (!inherits(result$value, "ggplot") && !is_plotly_result && !is_kable_result) {
                    print(result$value)
                  } else if (is_plotly_result || is_kable_result) {
                    # Suppress viewer - don't print it, just capture silently
                    invisible(result$value)
                  }
                }
              })
              all_output <- c(all_output, expr_output)
            }, error = function(e) {
              all_output <- c(all_output, paste("Error:", e$message))
            })
            i <- i + 1
          }
        }
        
        # If we captured multiple plots, use those
        if (length(plot_files_multi) > 0) {
          plot_file <- plot_files_multi
          has_plot <- TRUE
          output <- all_output
        } else {
          # Fall back to regular execution
          output <- utils::capture.output({
            result <- withVisible(eval(parse(text = code), envir = env))
            if (result$visible && !is.null(result$value)) {
              # Suppress plotly and kable viewer display
              is_plotly_result <- inherits(result$value, "plotly") || 
                                 (is.list(result$value) && !is.null(result$value$x) && inherits(result$value$x, "plotly")) ||
                                 (requireNamespace("htmlwidgets", quietly = TRUE) && inherits(result$value, "htmlwidget"))
              is_kable_result <- inherits(result$value, "knitr_kable") || 
                                (is.character(result$value) && requireNamespace("knitr", quietly = TRUE) && 
                                 !is.null(attr(result$value, "format")) && attr(result$value, "format") %in% c("html", "latex"))
              if (!inherits(result$value, "ggplot") && !is_plotly_result && !is_kable_result) {
                print(result$value)
              } else if (is_plotly_result || is_kable_result) {
                # Suppress viewer - don't print it
                invisible(result$value)
              }
            }
          })
        }
      } else {
        # Parsing failed, fall back to regular execution
        output <- utils::capture.output({
          result <- withVisible(eval(parse(text = code), envir = env))
          if (result$visible && !is.null(result$value) && !inherits(result$value, "ggplot")) {
            print(result$value)
          }
        })
      }
    } else {
      # Execute code and capture plots (original logic for single plots or ggplot)
    output <- utils::capture.output({
      # Open the graphics device
      grDevices::png(filename = plot_file, width = 800, height = 600)

      # Execute the code with the graphics device open
      result <- withVisible(eval(parse(text = code), envir = env))

      # If result is a ggplot object, print it while device is open
        # Note: plotly objects are handled separately after execution
      if (result$visible && !is.null(result$value) && inherits(result$value, "ggplot")) {
        print(result$value)
      }

      # Close the graphics device
      grDevices::dev.off()

      # Check if a plot was created
      has_plot <- tryCatch({
        if (file.exists(plot_file)) {
          file_size <- file.info(plot_file)$size
          !is.null(file_size) && length(file_size) == 1 && file_size > 100
        } else {
          FALSE
        }
      }, error = function(e) {
        FALSE
      })

      # Print the result if it's visible (for non-plot objects)
        # Skip plotly and kable objects - they should not open viewer
        is_result_plotly <- !is.null(result$value) && 
                           (inherits(result$value, "plotly") || 
                            (is.list(result$value) && !is.null(result$value$x) && inherits(result$value$x, "plotly")) ||
                            (requireNamespace("htmlwidgets", quietly = TRUE) && inherits(result$value, "htmlwidget")))
        is_result_kable <- !is.null(result$value) && 
                          (inherits(result$value, "knitr_kable") || 
                           (is.character(result$value) && requireNamespace("knitr", quietly = TRUE) && 
                            !is.null(attr(result$value, "format")) && attr(result$value, "format") %in% c("html", "latex")))
        if (result$visible && (is.null(result$value) || (!inherits(result$value, "ggplot") && !is_result_plotly && !is_result_kable))) {
        print(result$value)
        } else if (is_result_plotly || is_result_kable) {
          # Suppress plotly/kable viewer - don't print it
          invisible(result$value)
        }
      })
    }
    
    # After execution, ALWAYS check for multiple ggplot objects when ggplot is used
    # Also check for plotly objects (plot_ly, ggplotly, etc.)
    # This is critical because when multiple plots are printed, only the last one is captured
    # by the single graphics device, so we need to re-print each plot separately
    plot_files <- c()
    has_plotly <- grepl("plot_ly|ggplotly|plotly::", code, ignore.case = TRUE)
    if (is_plot_command && (grepl("ggplot", code, ignore.case = TRUE) || has_plotly)) {
      # Parse code to find all ggplot/plotly expressions (assigned or not)
      # This is necessary because direct ggplot() calls aren't stored in variables
      parsed_code <- tryCatch({
        parse(text = code)
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(parsed_code)) {
        # Find all ggplot/plotly expressions in the parsed code
        ggplot_exprs <- list()
        for (expr in parsed_code) {
          expr_str <- paste(deparse(expr), collapse = " ")
          # Check if this expression contains ggplot() or plot_ly() call
          is_ggplot_expr <- grepl("\\bggplot\\s*\\(", expr_str, ignore.case = TRUE) ||
                           grepl("\\bplot_ly\\s*\\(", expr_str, ignore.case = TRUE) ||
                           grepl("\\bggplotly\\s*\\(", expr_str, ignore.case = TRUE)
          
          if (is_ggplot_expr) {
            ggplot_exprs <- c(ggplot_exprs, list(expr))
          }
        }
        
        # If we found ggplot expressions, execute each one separately to capture all plots
        if (length(ggplot_exprs) > 0) {
          for (ggplot_expr in ggplot_exprs) {
            tryCatch({
              # Execute the expression in a temporary environment to capture the plot object
              expr_result <- withVisible(eval(ggplot_expr, envir = env))
              
              # Check if the result is a ggplot or plotly object
              if (!is.null(expr_result$value)) {
                is_plotly_obj <- inherits(expr_result$value, "plotly") || 
                                (is.list(expr_result$value) && !is.null(expr_result$value$x) && inherits(expr_result$value$x, "plotly")) ||
                                (requireNamespace("htmlwidgets", quietly = TRUE) && inherits(expr_result$value, "htmlwidget"))
                
                if (inherits(expr_result$value, "ggplot")) {
                  # Capture ggplot object
                  plot_file_single <- tempfile(fileext = ".png")
                  grDevices::png(filename = plot_file_single, width = 800, height = 600)
                  print(expr_result$value)
                  grDevices::dev.off()
                  
                  if (file.exists(plot_file_single)) {
                    file_size <- tryCatch(file.info(plot_file_single)$size, error = function(e) 0)
                    if (length(file_size) == 1 && file_size > 100) {
                      plot_files <- c(plot_files, plot_file_single)
                    } else {
                      unlink(plot_file_single)
                    }
                  } else {
                    unlink(plot_file_single)
                  }
                } else if (is_plotly_obj) {
                  # Capture plotly object
                  plot_file_single <- tempfile(fileext = ".png")
                  tryCatch({
                    if (requireNamespace("htmlwidgets", quietly = TRUE) && 
                        requireNamespace("webshot", quietly = TRUE)) {
                      html_file <- tempfile(fileext = ".html")
                      lib_dir <- tempfile("lib")
                      dir.create(lib_dir)
                      htmlwidgets::saveWidget(expr_result$value, html_file, selfcontained = TRUE, libdir = lib_dir)
                      webshot::webshot(html_file, plot_file_single, vwidth = 800, vheight = 600)
                      unlink(html_file)
                      unlink(lib_dir, recursive = TRUE)
                      
                      if (file.exists(plot_file_single)) {
                        file_size <- tryCatch(file.info(plot_file_single)$size, error = function(e) 0)
                        if (length(file_size) == 1 && file_size > 100) {
                          plot_files <- c(plot_files, plot_file_single)
                        } else {
                          unlink(plot_file_single)
                        }
                      }
                    }
                  }, error = function(e) {
                    if (file.exists(plot_file_single)) unlink(plot_file_single)
                  })
                }
              }
            }, error = function(e) {
              # Skip if we can't capture this plot
            })
          }
        }
      }
      
      # Also check for assigned plot variables (backup method)
      plot_prints <- character(0)
      
      # Pattern 1: Look for assignments with ggplot/plot_ly/ggplotly
      assignment_pattern <- "([a-zA-Z_][a-zA-Z0-9_]*)\\s*(<-|=)\\s*(ggplot|plot_ly|ggplotly)"
      assignment_matches <- regmatches(code, gregexpr(assignment_pattern, code, ignore.case = TRUE, perl = TRUE))[[1]]
      for (match in assignment_matches) {
        var_match <- regmatches(match, regexec("([a-zA-Z_][a-zA-Z0-9_]*)\\s*(<-|=)\\s*(ggplot|plot_ly|ggplotly)", match, ignore.case = TRUE, perl = TRUE))[[1]]
        if (length(var_match) > 1) {
          plot_prints <- c(plot_prints, var_match[2])
        }
      }
      
      # Pattern 2: Look for variables that are printed (just the variable name on its own line)
      code_lines <- strsplit(code, "\n")[[1]]
      for (line in code_lines) {
        line_trimmed <- trimws(line)
        if (line_trimmed == "" || grepl("^#", line_trimmed)) next
        if (grepl("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*$", line_trimmed, perl = TRUE)) {
          var_match <- regmatches(line_trimmed, regexec("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*$", line_trimmed, perl = TRUE))[[1]]
          if (length(var_match) > 1) {
            plot_prints <- c(plot_prints, var_match[2])
          }
        } else if (grepl("print\\s*\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\)", line_trimmed, perl = TRUE)) {
          var_match <- regmatches(line_trimmed, regexec("print\\s*\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\)", line_trimmed, perl = TRUE))[[1]]
          if (length(var_match) > 1) {
            plot_prints <- c(plot_prints, var_match[2])
          }
        }
      }
      
      # Remove duplicates and check if variables exist and are plot objects (ggplot or plotly)
      plot_prints <- unique(plot_prints)
      valid_plot_vars <- character(0)
      for (var_name in plot_prints) {
        tryCatch({
          if (exists(var_name, envir = env)) {
            obj <- get(var_name, envir = env)
            # Check for ggplot or plotly objects
            is_plotly <- inherits(obj, "plotly") || 
                        (is.list(obj) && !is.null(obj$x) && inherits(obj$x, "plotly")) ||
                        (requireNamespace("htmlwidgets", quietly = TRUE) && inherits(obj, "htmlwidget"))
            if (inherits(obj, "ggplot") || is_plotly) {
              valid_plot_vars <- c(valid_plot_vars, var_name)
            }
          }
        }, error = function(e) {
          # Skip if we can't access the variable
        })
      }
      
      # If we found plot objects (even just one), capture each one separately
      # This ensures we get all plots, not just the last one
      if (length(valid_plot_vars) > 0) {
        for (plot_var in valid_plot_vars) {
          tryCatch({
            plot_obj <- get(plot_var, envir = env)
            
            # Check if this is a plotly object
            is_plotly_obj <- inherits(plot_obj, "plotly") || 
                            (is.list(plot_obj) && !is.null(plot_obj$x) && inherits(plot_obj$x, "plotly")) ||
                            (requireNamespace("htmlwidgets", quietly = TRUE) && inherits(plot_obj, "htmlwidget") && 
                             !is.null(plot_obj$x) && !is.null(plot_obj$x$config) && 
                             !is.null(plot_obj$x$config$modeBarButtonsToAdd))
            
            if (is_plotly_obj) {
              # Convert plotly to static PNG image
              # Suppress viewer display by using saveWidget with libdir to avoid viewer
              plot_file_single <- tempfile(fileext = ".png")
              tryCatch({
                # Try using webshot to convert HTML widget to PNG
                if (requireNamespace("htmlwidgets", quietly = TRUE) && 
                    requireNamespace("webshot", quietly = TRUE)) {
                  # Save as HTML first, then convert to PNG
                  # Use libdir parameter to avoid viewer display
                  html_file <- tempfile(fileext = ".html")
                  lib_dir <- tempfile("lib")
                  dir.create(lib_dir)
                  # Suppress viewer by using saveWidget with libdir
                  htmlwidgets::saveWidget(plot_obj, html_file, selfcontained = TRUE, libdir = lib_dir)
                  webshot::webshot(html_file, plot_file_single, vwidth = 800, vheight = 600)
                  unlink(html_file)
                  unlink(lib_dir, recursive = TRUE)
                } else {
                  # If webshot not available, skip plotly capture
                  plot_file_single <- NULL
                }
                
                if (!is.null(plot_file_single) && file.exists(plot_file_single)) {
                  file_size <- tryCatch(file.info(plot_file_single)$size, error = function(e) 0)
                  if (length(file_size) == 1 && file_size > 100) {
                    plot_files <- c(plot_files, plot_file_single)
                  } else {
                    unlink(plot_file_single)
                  }
                }
              }, error = function(e) {
                # If plotly export fails, skip this plot
                if (!is.null(plot_file_single) && file.exists(plot_file_single)) {
                  unlink(plot_file_single)
                }
              })
            } else if (inherits(plot_obj, "ggplot")) {
              # Handle ggplot objects
              plot_file_single <- tempfile(fileext = ".png")
              grDevices::png(filename = plot_file_single, width = 800, height = 600)
              print(plot_obj)
              grDevices::dev.off()
              
              if (file.exists(plot_file_single)) {
                file_size <- tryCatch(file.info(plot_file_single)$size, error = function(e) 0)
                if (length(file_size) == 1 && file_size > 100) {
                  plot_files <- c(plot_files, plot_file_single)
                } else {
                  unlink(plot_file_single)
                }
              } else {
                unlink(plot_file_single)
              }
            }
          }, error = function(e) {
            # Skip if we can't capture this plot
          })
        }
        
        # If we captured ggplot plots from direct expressions or assigned variables, merge with base R plots
        if (length(plot_files) > 0) {
          if (length(plot_file) > 0 && is.character(plot_file) && !is.list(plot_file)) {
            # Merge base R plots (already captured) with ggplot plots
            plot_file <- c(plot_file, plot_files)
          } else {
            # Only ggplot plots
            plot_file <- plot_files
          }
          has_plot <- TRUE
        }
      }
    }
    
    # Ensure has_plot is always a single logical value
    if (is.logical(has_plot)) {
      has_plot <- has_plot[1]
    } else {
      has_plot <- as.logical(has_plot)[1]
    }

    # Prepare the response
    response <- list(
      success = TRUE,
      output = paste(output, collapse = "\n")
    )

    # Include the result value if available
    if (exists("result") && !is.null(result$value)) {
      response$result <- if (is.data.frame(result$value)) {
        # For dataframes, convert to a readable format
        list(
          is_dataframe = TRUE,
          dimensions = dim(result$value),
          head = utils::head(result$value, 10)
        )
      } else if (inherits(result$value, "ggplot")) {
        # For ggplot objects, don't serialize - just return a simple message
        "ggplot object created (not serialized)"
      } else {
        # For other objects, check if they can be serialized to JSON
        # Complex objects like models, environments, etc. cannot be serialized
        obj_class <- class(result$value)
        
        # Check if object is serializable by attempting to convert to JSON
        is_serializable <- tryCatch({
          # Try to serialize a simple version
          jsonlite::toJSON(list(test = result$value), auto_unbox = TRUE)
          TRUE
        }, error = function(e) {
          FALSE
        })
        
        if (is_serializable && (is.atomic(result$value) || is.list(result$value))) {
          # Object is simple enough to serialize
          result$value
        } else {
          # Complex object - return a summary instead
          list(
            class = obj_class,
            type = typeof(result$value),
            summary = tryCatch({
              # Try to get a summary
              summary_output <- capture.output(print(result$value))
              if (length(summary_output) > 20) {
                # Truncate if too long
                c(summary_output[1:20], "... (truncated)")
              } else {
                summary_output
              }
            }, error = function(e) {
              paste("Complex object of class:", paste(obj_class, collapse = ", "))
            }),
            message = "Object created successfully but cannot be displayed in JSON format"
          )
        }
      }
    }

    # Include plot(s) if available
    if (is.logical(has_plot) && length(has_plot) == 1 && has_plot) {
      # Handle multiple plots or single plot
      plot_files_to_encode <- if (is.null(plot_file)) {
        NULL
      } else if (is.character(plot_file) && length(plot_file) > 1) {
        # Multiple plot files (vector)
        plot_file
      } else if (is.character(plot_file) && length(plot_file) == 1) {
        # Single plot file
        plot_file
      } else {
        NULL
      }
      
      if (!is.null(plot_files_to_encode)) {
        # Convert plots to base64 and add to output (same format as visualization agent)
        tryCatch({
          if (requireNamespace("base64enc", quietly = TRUE)) {
            # Encode each plot separately
            for (pf in plot_files_to_encode) {
              if (file.exists(pf)) {
                plot_base64 <- base64enc::base64encode(pf)
                # Add PLOT_DATA to output so frontend can detect and display it
                output <- c(output, paste0("PLOT_DATA:", plot_base64))
              }
            }
            # Update response output with plot data
            response$output <- paste(output, collapse = "\n")
          } else {
          }
        }, error = function(e) {
          # If base64enc is not available, just continue without base64 encoding
          cat("Warning: Error encoding plot -", e$message, "\n")
        })
      }
      
      response$plot <- list(
        created = TRUE,
        message = "Plot created and displayed in RStudio",
        mime_type = "image/png"
      )
    }
    
    return(response)
  }, error = function(e) {
    # Log the error if logging is enabled
    if (settings$log_to_file && !is.null(settings$log_file_path) && settings$log_file_path != "") {
      log_error_to_file(code, e$message, settings$log_file_path)
    }

    return(list(
      success = FALSE,
      error = e$message
    ))
  }, finally = {
    # Clean up temporary files
    # Use unlink() instead of file.remove() as it's more robust on Windows
    # Suppress warnings about file removal failures (file may be locked or already deleted)
    if (!is.null(plot_file)) {
      # Handle both single file and vector of files
      files_to_cleanup <- if (length(plot_file) == 1) {
        if (file.exists(plot_file)) plot_file else NULL
      } else {
        plot_file[sapply(plot_file, file.exists)]
      }
      
      if (!is.null(files_to_cleanup) && length(files_to_cleanup) > 0) {
        tryCatch({
          # Ensure graphics device is fully closed
          while (grDevices::dev.cur() != 1) {
            grDevices::dev.off()
          }
          # Use unlink with force=TRUE to handle locked files on Windows
          suppressWarnings(unlink(files_to_cleanup, force = TRUE))
        }, error = function(e) {
          # Silently ignore cleanup errors
        })
      }
    }
  })
}

#' Log code to file
#'
#' @param code The R code to log
#' @param log_path The path to the log file
#' @return Invisible NULL
log_code_to_file <- function(code, log_path) {
  # Create timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Format the log entry
  log_entry <- sprintf("# --- [%s] ---\n# Code executed by AI:\n%s\n\n", timestamp, code)

  # Create directory if it doesn't exist
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Append to the log file
  cat(log_entry, file = log_path, append = TRUE)

  invisible(NULL)
}

#' Log error to file
#'
#' @param code The R code that caused the error
#' @param error_message The error message
#' @param log_path The path to the log file
#' @return Invisible NULL
log_error_to_file <- function(code, error_message, log_path) {
  # Create timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Format the log entry
  log_entry <- sprintf("# --- [%s] ---\n# Code executed by AI (ERROR):\n%s\n# Error: %s\n\n",
                      timestamp, code, error_message)

  # Create directory if it doesn't exist
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Append to the log file
  cat(log_entry, file = log_path, append = TRUE)

  invisible(NULL)
}

#' Load execution settings
#'
#' @return A list containing execution settings
load_execution_settings <- function() {
  # Default settings
  default_settings <- list(
    print_to_console = TRUE,
    log_to_file = FALSE,
    log_file_path = file.path(path.expand("~"), "rstudioai_logs.R")
  )

  # Try to load settings from a settings file
  settings_file <- file.path(path.expand("~"), ".rstudioai_settings.rds")

  if (file.exists(settings_file)) {
    tryCatch({
      settings <- readRDS(settings_file)
      # Merge with defaults to ensure all fields exist
      settings <- modifyList(default_settings, settings)
      return(settings)
    }, error = function(e) {
      return(default_settings)
    })
  } else {
    return(default_settings)
  }
}

#' Save execution settings
#'
#' @param settings A list containing execution settings
#' @return Invisible NULL
save_execution_settings <- function(settings) {
  # Save settings to a settings file
  settings_file <- file.path(path.expand("~"), ".rstudioai_settings.rds")
  saveRDS(settings, settings_file)
  invisible(NULL)
}
