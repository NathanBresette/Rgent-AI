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
#' @param start_at Index of the top-level expression to start from. Used to
#'   resume after a question or an automatic fix, so lines that already ran
#'   don't run twice.
#' @param on_expr_start Optional function(i) called before each expression
#' @return A list containing the execution result and metadata. On failure it
#'   also has `failed_at` (the expression that failed) and `done_output` (the
#'   output, plots included, of the expressions that finished before it).
execute_code_in_session <- function(code, settings = NULL, start_at = 1L,
                                    on_expr_start = NULL) {
  start_at <- max(1L, as.integer(start_at %||% 1L))
  # Devices open before this run (RStudio's Plots pane, the user's own): the
  # chat closes only the ones it opened itself
  devs_before <- grDevices::dev.list()
  dev_cur_before <- grDevices::dev.cur()
  # Default settings if not provided
  if (is.null(settings)) {
    settings <- list(
      print_to_console = TRUE,
      log_to_file = FALSE,
      log_file_path = file.path(path.expand("~"), "rstudioai_logs.R")
    )
  }

  # Print code to console if enabled (once, not again when resuming)
  if (settings$print_to_console && start_at == 1L) {
    cat(.rgent_console_echo(code), sep = "\n")
  }

  # Log code to file if enabled
  if (start_at == 1L && settings$log_to_file && !is.null(settings$log_file_path) && settings$log_file_path != "") {
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

  # Set up plot capture
  plot_files <- c()
  all_output <- c()
  has_plot <- FALSE
  result <- NULL
  execution_had_error <- FALSE  # Track if any expression had an error
  last_plot_error <- ""
  last_error_message <- NULL    # Store the last error message

  tryCatch({
    # Store plot command for analysis if applicable
    is_plot_command <- any(grepl("(ggplot|plot|hist|boxplot|barplot|plotly|geom_)", code, ignore.case = TRUE))
    if (is_plot_command) {
      tryCatch({
        .rgent_state$last_plot_command <- code
        .rgent_state$last_plot_timestamp <- Sys.time()
      }, error = function(e) {})
    }

    # Base R plot functions and drawing commands
    base_plot_functions <- c(
      # Core base R plots
      "plot", "boxplot", "hist", "barplot", "pie", "qqnorm", "qqplot", 
      "pairs", "image", "contour", "persp", "matplot", "curve",
      # Additional base R plots
      "dotchart", "stripchart", "heatmap", "filled.contour", "mosaicplot",
      "fourfoldplot", "assocplot", "spineplot", "cdplot", "sunflowerplot",
      "symbols", "stars", "coplot", "interaction.plot", "termplot",
      # Specialized packages that draw directly
      "corrplot", "pheatmap", "Heatmap", "superheat",
      # lattice plots
      "xyplot", "bwplot", "densityplot", "histogram", "levelplot", 
      "contourplot", "wireframe", "splom", "cloud", "parallel",
      "dotplot", "barchart", "stripplot", "qq"
    )
    plot_drawing_commands <- c("abline", "lines", "points", "legend", "text", "title", "axis", 
                               "grid", "rug", "segments", "arrows", "rect", "polygon",
                               "mtext", "box", "locator", "identify")
    
    # Grid-based multi-plot functions (gridExtra, cowplot, patchwork, ggpubr, egg)
    grid_plot_functions <- c("grid.arrange", "arrangeGrob", "plot_grid", "wrap_plots",
                             "ggarrange", "marrangeGrob", "grid.draw")
    
    # Helper: Check if object is a plot type
    is_ggplot_obj <- function(x) inherits(x, "ggplot")
    is_plotly_obj <- function(x) {
      inherits(x, "plotly") || 
      (is.list(x) && !is.data.frame(x) && "x" %in% names(x) && inherits(x[["x"]], "plotly")) ||
      (requireNamespace("htmlwidgets", quietly = TRUE) && inherits(x, "htmlwidget"))
    }
    is_kable_obj <- function(x) {
      inherits(x, "knitr_kable") || 
      (is.character(x) && !is.null(attr(x, "format")) && attr(x, "format") %in% c("html", "latex"))
    }
    
    # Helper: Capture a ggplot object to PNG
    capture_ggplot <- function(plot_obj) {
      plot_file <- tempfile(fileext = ".png")
      tryCatch({
        grDevices::png(filename = plot_file, width = 800, height = 600)
        print(plot_obj)
        grDevices::dev.off()
        .rgent_close_new_devices(devs_before, dev_cur_before)
        
        if (file.exists(plot_file) && file.info(plot_file)$size > 100) {
          return(plot_file)
        }
        unlink(plot_file)
        return(NULL)
      }, error = function(e) {
        tryCatch(.rgent_close_new_devices(devs_before, dev_cur_before), error = function(e) {})
        if (file.exists(plot_file)) unlink(plot_file)
        return(NULL)
      })
    }
    
    # Helper: Capture a plotly object to PNG via webshot
    capture_plotly <- function(plot_obj) {
      # Check for required packages
      if (!requireNamespace("htmlwidgets", quietly = TRUE)) return(NULL)
      if (!requireNamespace("webshot", quietly = TRUE)) return(NULL)
      
      # Check if phantomjs is installed
      # find_phantom() is internal to webshot; is_phantomjs_installed() is its export
      phantom_ok <- tryCatch(isTRUE(webshot::is_phantomjs_installed()), error = function(e) FALSE)
      if (!phantom_ok) return(NULL)
      
      plot_file <- tempfile(fileext = ".png")
      html_file <- tempfile(fileext = ".html")
      lib_dir <- tempfile("lib")
      
      tryCatch({
        dir.create(lib_dir)
        
        # Suppress browser during saveWidget
        old_browser <- getOption("browser")
        options(browser = function(url) invisible(NULL))
        htmlwidgets::saveWidget(plot_obj, html_file, selfcontained = TRUE, libdir = lib_dir)
        options(browser = old_browser)
        
        webshot::webshot(html_file, plot_file, vwidth = 800, vheight = 600, delay = 0.5)
        
        # Cleanup temp files
        unlink(html_file)
        unlink(lib_dir, recursive = TRUE)
        
        if (file.exists(plot_file) && file.info(plot_file)$size > 100) {
          return(plot_file)
        }
        unlink(plot_file)
        return(NULL)
      }, error = function(e) {
        if (file.exists(html_file)) unlink(html_file)
        if (dir.exists(lib_dir)) unlink(lib_dir, recursive = TRUE)
        if (file.exists(plot_file)) unlink(plot_file)
        return(NULL)
      })
    }
    
    # Helper: Check if expression is a base R plot call
    is_base_plot_expr <- function(expr_str) {
      any(sapply(base_plot_functions, function(f) {
        grepl(paste0("\\b", f, "\\s*\\("), expr_str, ignore.case = TRUE)
      }))
    }
    
    # Helper: Check if expression is a drawing command
    is_drawing_expr <- function(expr_str) {
      any(sapply(plot_drawing_commands, function(f) {
        grepl(paste0("\\b", f, "\\s*\\("), expr_str, ignore.case = TRUE)
      }))
    }
    
    # Helper: Check if expression is an assignment
    # Name of the function a call uses, e.g. "print" for print(x) and
    # base::print(x). Always one string: as.character() on `pkg::fn` gives
    # three ("::", "pkg", "fn"), which broke every `pkg::fn(...)` line.
    call_name <- function(expr) {
      fn <- expr[[1]]
      if (is.symbol(fn)) return(as.character(fn))
      if (is.call(fn) && length(fn) == 3 && as.character(fn[[1]]) %in% c("::", ":::")) {
        return(as.character(fn[[3]]))
      }
      ""
    }

    is_assignment <- function(expr) {
      if (is.call(expr)) {
        op <- call_name(expr)
        return(op %in% c("<-", "=", "<<-", "assign"))
      }
      FALSE
    }
    
    # Helper: Check if expression is a print() call
    is_print_call <- function(expr) {
      if (is.call(expr)) {
        op <- call_name(expr)
        return(op == "print")
      }
      FALSE
    }
    
    # Helper: Check if expression might produce plotly (look at code text)
    is_plotly_expr <- function(expr_str) {
      grepl("plot_ly|ggplotly|plotly::", expr_str, ignore.case = TRUE)
    }
    
    # Helper: Check if expression is a grid-based multi-plot function
    is_grid_plot_expr <- function(expr_str) {
      any(sapply(grid_plot_functions, function(f) {
        grepl(paste0("(\\b", f, "\\s*\\()|(gridExtra::", f, ")|(cowplot::", f, ")|(patchwork::", f, ")"), 
              expr_str, ignore.case = TRUE)
      }))
    }
    
    # Helper: Execute expression and capture plotly WITHOUT opening browser
    execute_and_capture_plotly <- function(expr, env) {
      # Temporarily suppress all browser/viewer/htmlwidgets output
      old_browser <- getOption("browser")
      old_viewer <- getOption("viewer")
      old_htmlwidgets_viewer <- getOption("htmlwidgets.viewer")
      
      # Set all viewers to no-op functions
      options(browser = function(url) invisible(NULL))
      options(viewer = function(url, ...) invisible(NULL))
      options(htmlwidgets.viewer = function(url, ...) invisible(NULL))
      
      result <- tryCatch({
        # Evaluate without triggering print
        val <- eval(expr, envir = env)
        list(value = val, success = TRUE)
      }, error = function(e) {
        list(value = NULL, success = FALSE, error = conditionMessage(e))
      }, finally = {
        # Restore all options
        options(browser = old_browser)
        options(viewer = old_viewer)
        options(htmlwidgets.viewer = old_htmlwidgets_viewer)
      })
      
      return(result)
    }
    
    # One PNG device for the whole run, like the Console's one device
    # (2.4.5). Each new page becomes its own file (`%03d`), so par(), layout(),
    # legend() after plot(), hist(add = TRUE) in a loop and ggplots all draw
    # where they would in the Console, in order. Before, every base plot
    # statement got a fresh device: `for (...) hist(..., add = TRUE)` failed
    # with "plot.new has not been called yet" and par(mfrow = ...) was lost.
    run_dev <- NULL
    run_pattern <- NULL
    run_dev_use <- function() {
      cur <- grDevices::dev.cur()
      # The user's code opened its own device (png("out.png"), pdf(), ...): leave it
      if (cur != 1 && !(cur %in% devs_before) && !identical(cur, run_dev)) return(invisible(NULL))
      if (!is.null(run_dev) && run_dev %in% grDevices::dev.list()) {
        if (cur != run_dev) grDevices::dev.set(run_dev)
        return(invisible(NULL))
      }
      run_dev_flush()
      run_pattern <<- tempfile("rgent-plot-")
      grDevices::png(filename = paste0(run_pattern, "-%03d.png"), width = 800, height = 600)
      run_dev <<- grDevices::dev.cur()
      invisible(NULL)
    }
    # Close the run's device and add its pages to plot_files, in order
    run_dev_flush <- function() {
      if (is.null(run_pattern)) return(invisible(NULL))
      if (!is.null(run_dev) && run_dev %in% grDevices::dev.list()) try(grDevices::dev.off(run_dev), silent = TRUE)
      files <- sort(Sys.glob(paste0(run_pattern, "-*.png")))
      files <- files[!is.na(file.info(files)$size) & file.info(files)$size > 100]
      plot_files <<- c(plot_files, files)
      run_dev <<- NULL
      run_pattern <<- NULL
      invisible(NULL)
    }

    # Parse code into expressions
    parsed_code <- tryCatch(parse(text = code), error = function(e) NULL)
    
    if (!is.null(parsed_code) && length(parsed_code) > 0) {
      # Single-pass execution: handle each expression based on its type
      i <- start_at
      expr_start_i <- i
      output_mark <- length(all_output)
      plots_mark <- length(plot_files)
      # Stop at the first error, like R does, so the error reported is the
      # one that caused the failure rather than a knock-on error from a later line
      while (i <= length(parsed_code) && !execution_had_error) {
        # The Stop button (stop.R): stop before the next statement. One that
        # is already running can't be interrupted from the chat.
        if (i > start_at && isTRUE(.rgent_poll_messages())) {
          .rgent_stream_state$run_stopped <- list(before = i, of = length(parsed_code))
          if (isTRUE(settings$print_to_console)) cat("# Rgent: stopped before statement", i, "of", length(parsed_code), "\n")
          break
        }
        expr <- parsed_code[[i]]
        expr_str <- paste(deparse(expr), collapse = " ")
        # Remember where this expression starts, so a failure can be resumed here
        expr_start_i <- i
        output_mark <- length(all_output)
        plots_mark <- length(plot_files)
        if (is.function(on_expr_start)) on_expr_start(i)
        
        run_dev_use()
        if (is_plotly_expr(expr_str) && !is_assignment(expr)) {
          # plotly pictures are taken separately: keep the order of the pages before it
          run_dev_flush()
          # PLOTLY EXPRESSION: Handle specially to prevent browser opening
          tryCatch({
            # Capture any text output from plotly commands
            plotly_output <- utils::capture.output({
              plotly_result <- execute_and_capture_plotly(expr, env)
            })

            if (length(plotly_output) > 0) {
              all_output <- c(all_output, plotly_output)
            }

            if (plotly_result$success && !is.null(plotly_result$value)) {
              result <- list(value = plotly_result$value, visible = TRUE)

              if (is_plotly_obj(plotly_result$value)) {
                pf <- capture_plotly(plotly_result$value)
                if (!is.null(pf)) plot_files <- c(plot_files, pf)
              }
            } else if (!plotly_result$success) {
              if (!execution_had_error) {
                all_output <- c(all_output, paste("Error:", plotly_result$error))
                execution_had_error <- TRUE
                last_error_message <- plotly_result$error
              }
            }
          }, error = function(e) {
            if (!execution_had_error) {
              all_output <<- c(all_output, paste("Error:", conditionMessage(e)))
              execution_had_error <<- TRUE
              last_error_message <<- conditionMessage(e)
            }
          })
          i <- i + 1
          
                } else {
          # EXECUTE EXPRESSION and check result type at runtime
          # Capture ALL console output (cat, print, message, str, etc.)
          # Suppress browser, viewer, AND htmlwidgets viewer for ALL expressions
          old_browser <- getOption("browser")
          old_viewer <- getOption("viewer")
          old_htmlwidgets_viewer <- getOption("htmlwidgets.viewer")
          options(browser = function(url) invisible(NULL))
          options(viewer = function(url, ...) invisible(NULL))
          options(htmlwidgets.viewer = function(url, ...) invisible(NULL))
          
          tryCatch({
            # Wrap evaluation in capture.output to get ALL console output
            expr_output <- utils::capture.output({
              expr_result <- withVisible(eval(expr, envir = env))
              
              # Auto-print visible results (like typing a variable name)
              # But SKIP plotly objects to prevent them from trying to open viewer
              if (expr_result$visible && !is.null(expr_result$value) &&
                  !is_ggplot_obj(expr_result$value) && 
                  !is_plotly_obj(expr_result$value) &&
                  !is_kable_obj(expr_result$value)) {
                print(expr_result$value)
              }
            })
            
            result <- expr_result
            
            # Add captured output to all_output
            if (length(expr_output) > 0) {
              all_output <- c(all_output, expr_output)
            }
            
            # Handle plot objects detected at RUNTIME
            is_assign <- is_assignment(expr)
            is_print <- is_print_call(expr)
            
            if (!is.null(expr_result$value)) {
              should_capture_plot <- (expr_result$visible || is_print) && !is_assign
              
              if (is_ggplot_obj(expr_result$value) && should_capture_plot) {
                drawn <- is_print || isTRUE(tryCatch({
                  run_dev_use()
                  suppressMessages(print(expr_result$value))
                  TRUE
                }, error = function(e) { last_plot_error <<- conditionMessage(e); FALSE }))
                if (!drawn) {
                  # Plot capture failed - treat as execution error for consistency
                  if (!execution_had_error) {
                    msg <- if (nzchar(last_plot_error)) last_plot_error else "Plot rendering failed"
                    all_output <- c(all_output, paste("Error:", msg))
                    execution_had_error <- TRUE
                    last_error_message <- msg
                  }
                }
                
              } else if (is_plotly_obj(expr_result$value) && should_capture_plot) {
                # Plotly detected at runtime (e.g., typing variable name)
                # Browser/viewer already suppressed above, just try to capture
                pf <- capture_plotly(expr_result$value)
                if (!is.null(pf)) {
                  plot_files <- c(plot_files, pf)
                } else {
                  # Plot capture failed - treat as execution error for consistency
                  if (!execution_had_error) {
                    all_output <- c(all_output, "Error: Plotly rendering failed")
                    execution_had_error <- TRUE
                    last_error_message <- "Plotly rendering failed"
                  }
                }
              }
            }
          }, error = function(e) {
            all_output <<- c(all_output, paste("Error:", conditionMessage(e)))
            execution_had_error <<- TRUE
            last_error_message <<- conditionMessage(e)
          }, finally = {
            options(browser = old_browser)
            options(viewer = old_viewer)
            options(htmlwidgets.viewer = old_htmlwidgets_viewer)
          })
          i <- i + 1
        }
      }
      
      run_dev_flush()
      # Set output and plot status
      output <- all_output
      has_plot <- length(plot_files) > 0
      
      # If any expression had an error, return failure
      if (execution_had_error) {
        # What finished before the failing expression, so a resumed run can
        # show it without running those lines again
        done <- all_output[seq_len(output_mark)]
        for (pf in plot_files) {  # the run device is flushed at the end, so all of them
          if (file.exists(pf) && requireNamespace("base64enc", quietly = TRUE)) {
            done <- c(done, paste0("PLOT_DATA:", base64enc::base64encode(pf)))
          }
        }
        return(list(success = FALSE, error = last_error_message,
                    output = paste(output, collapse = "\n"),
                    failed_at = expr_start_i,
                    done_output = paste(done, collapse = "\n")))
      }
      
    } else {
      # Parsing failed - simple fallback execution
      output <- utils::capture.output({
        result <- withVisible(eval(parse(text = code), envir = env))
        if (result$visible && !is.null(result$value) && !is_ggplot_obj(result$value)) {
          print(result$value)
        }
      })
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
    if (has_plot && length(plot_files) > 0) {
      # Convert plots to base64 and add to output
        tryCatch({
          if (requireNamespace("base64enc", quietly = TRUE)) {
          for (pf in plot_files) {
              if (file.exists(pf)) {
                plot_base64 <- base64enc::base64encode(pf)
                output <- c(output, paste0("PLOT_DATA:", plot_base64))
            }
          }
          response$output <- paste(output, collapse = "\n")
          }
        }, error = function(e) {
          cat("Warning: Error encoding plot -", conditionMessage(e), "\n")
        })
      
      response$plot <- list(
        created = TRUE,
        message = "Plot created and displayed in RStudio",
        mime_type = "image/png"
      )
    }
    
    return(response)
  }, error = function(e) {
    if (settings$log_to_file && !is.null(settings$log_file_path) && settings$log_file_path != "") {
      log_error_to_file(code, conditionMessage(e), settings$log_file_path)
    }
    return(list(success = FALSE, error = conditionMessage(e)))
  }, finally = {
    # Clean up temporary plot files
    if (length(plot_files) > 0) {
        tryCatch({
        .rgent_close_new_devices(devs_before, dev_cur_before)
        # Keep the last picture for Explain plot: the plot was drawn into a file
        # and its device closed, so there's nothing left to redraw
        tryCatch(.rgent_keep_chat_plot(utils::tail(plot_files[file.exists(plot_files)], 1)), error = function(e) NULL)
        suppressWarnings(unlink(plot_files[sapply(plot_files, file.exists)], force = TRUE))
      }, error = function(e) {})
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

# The code the chat runs, shown in the Console as if it had been typed there:
# one "# Rgent ran:" line, then "> " for each statement and "+ " for its
# continuation lines (as R prints them).
.rgent_console_echo <- function(code) {
  lines <- unlist(strsplit(paste(code, collapse = "\n"), "\n", fixed = TRUE))
  while (length(lines) && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
  if (!length(lines)) return(character(0))
  starts <- tryCatch({
    exprs <- parse(text = lines, keep.source = TRUE)
    vapply(attr(exprs, "srcref"), function(s) s[1], 1L)
  }, error = function(e) seq_along(lines))
  prefix <- ifelse(seq_along(lines) %in% starts | !nzchar(trimws(lines)), "> ", "+ ")
  c("# Rgent ran:", paste0(prefix, lines))
}

# Closes graphics devices opened since `before` (dev.list() at the start of a
# chat run) and makes `cur` current again, leaving the user's own plots alone.
# It used to close every device, wiping the RStudio Plots pane.
.rgent_close_new_devices <- function(before, cur = 1L) {
  for (d in setdiff(grDevices::dev.list(), before)) try(grDevices::dev.off(d), silent = TRUE)
  if (cur > 1 && cur %in% grDevices::dev.list()) try(grDevices::dev.set(cur), silent = TRUE)
  invisible(NULL)
}
