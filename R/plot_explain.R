# Explain plot: what Claude is told about the user's last plot.
#
# "Explain plot" is an ordinary chat turn (so it streams, auto-runs the
# improved plot, follows the chat and lands in History like any other reply).
# The page sends `plot_explain = TRUE` and chat_with_ai adds .rgent_plot_note()
# to the prompt: the code that made the plot and, for ggplot2, the numbers
# that are actually drawn (from ggplot_build), so Claude quotes real values
# instead of guessing from the code.

.rgent_plot_command <- function() {
  g <- .GlobalEnv
  if (exists("last_plot_command", envir = g, inherits = FALSE) &&
      exists("last_plot_timestamp", envir = g, inherits = FALSE)) {
    ts <- get("last_plot_timestamp", envir = g)
    ok <- tryCatch(difftime(Sys.time(), ts, units = "mins") < 60, error = function(e) FALSE)
    if (isTRUE(ok)) return(paste(get("last_plot_command", envir = g), collapse = "\n"))
  }
  info <- tryCatch(find_last_plot_command(), error = function(e) list(found = FALSE))
  if (isTRUE(info$found)) paste(info$command, collapse = "\n") else NULL
}

.rgent_fmt_num <- function(v) {
  v <- suppressWarnings(as.numeric(v))
  out <- format(signif(v, 4), big.mark = ",", scientific = FALSE, trim = TRUE)
  out[is.na(v)] <- "NA"
  out
}

# Turns a built x or y position back into what the axis shows
.rgent_axis_formatter <- function(scale) {
  if (is.null(scale)) return(.rgent_fmt_num)
  if (inherits(scale, "ScaleDiscrete")) {
    lim <- tryCatch(scale$get_limits(), error = function(e) NULL)
    return(function(v) {
      i <- suppressWarnings(as.integer(round(as.numeric(v))))
      out <- if (length(lim)) as.character(lim[i]) else as.character(v)
      out[is.na(out)] <- as.character(v)[is.na(out)]
      out
    })
  }
  if (inherits(scale, "ScaleContinuousDate")) {
    return(function(v) format(as.Date(as.numeric(v), origin = "1970-01-01")))
  }
  if (inherits(scale, "ScaleContinuousDatetime")) {
    return(function(v) format(as.POSIXct(as.numeric(v), origin = "1970-01-01")))
  }
  .rgent_fmt_num
}

# Names of the groups (from the colour or fill legend), keyed by colour
.rgent_group_labels <- function(built) {
  for (aes in c("colour", "fill")) {
    sc <- tryCatch(built$plot$scales$get_scales(aes), error = function(e) NULL)
    if (is.null(sc) || !inherits(sc, "ScaleDiscrete")) next
    lim <- tryCatch(sc$get_limits(), error = function(e) NULL)
    if (!length(lim)) next
    cols <- tryCatch(sc$map(lim), error = function(e) NULL)
    if (length(cols) == length(lim)) {
      return(list(aes = aes, labels = stats::setNames(as.character(lim), as.character(cols))))
    }
  }
  NULL
}

.rgent_ggplot_note <- function(max_layers = 3, max_groups = 8, list_points = 14) {
  if (!"ggplot2" %in% loadedNamespaces()) return(NULL)
  p <- tryCatch(ggplot2::last_plot(), error = function(e) NULL)
  if (is.null(p)) return(NULL)
  b <- tryCatch(suppressMessages(suppressWarnings(ggplot2::ggplot_build(p))), error = function(e) NULL)
  if (is.null(b)) return(NULL)

  out <- character(0)
  labs <- if (length(b$plot$labels)) b$plot$labels else p$labels
  keep <- intersect(c("title", "subtitle", "x", "y", "colour", "fill", "size", "shape"), names(labs))
  if (length(keep)) {
    out <- c(out, paste0("Labels: ", paste0(keep, " = ", vapply(labs[keep], function(x) paste(as.character(x), collapse = " "), ""), collapse = "; ")))
  }
  geoms <- vapply(p$layers, function(l) sub("^Geom", "", class(l$geom)[1]), "")
  if (length(geoms)) out <- c(out, paste0("Layers: ", paste(geoms, collapse = ", ")))
  facet <- class(p$facet)[1]
  if (!identical(facet, "FacetNull")) out <- c(out, paste0("Faceted: ", sub("^Facet", "", facet)))

  sx <- tryCatch(b$layout$panel_scales_x[[1]], error = function(e) NULL)
  sy <- tryCatch(b$layout$panel_scales_y[[1]], error = function(e) NULL)
  fx <- .rgent_axis_formatter(sx)
  fy <- .rgent_axis_formatter(sy)
  groups <- .rgent_group_labels(b)

  for (i in seq_len(min(length(b$data), max_layers))) {
    d <- b$data[[i]]
    if (!is.data.frame(d) || nrow(d) == 0) next
    yv <- if ("y" %in% names(d)) "y" else if ("middle" %in% names(d)) "middle" else NULL
    if (!"x" %in% names(d) || is.null(yv)) next
    out <- c(out, sprintf("Layer %d (%s), %d drawn %s:", i, geoms[i], nrow(d),
                          if (nrow(d) == 1) "point" else "points"))
    key <- if ("group" %in% names(d)) d$group else rep(1L, nrow(d))
    if ("PANEL" %in% names(d) && length(unique(d$PANEL)) > 1) key <- paste(d$PANEL, key)
    ids <- unique(key)
    for (g in utils::head(ids, max_groups)) {
      s <- d[key == g, , drop = FALSE]
      s <- s[order(suppressWarnings(as.numeric(s$x))), , drop = FALSE]
      y <- suppressWarnings(as.numeric(s[[yv]]))
      name <- NULL
      if (!is.null(groups) && groups$aes %in% names(s)) name <- unname(groups$labels[as.character(s[[groups$aes]][1])])
      if (is.null(name) || is.na(name)) name <- if (length(ids) > 1) paste("group", g) else "all"
      if ("PANEL" %in% names(s) && length(unique(d$PANEL)) > 1) name <- paste0(name, " (panel ", s$PANEL[1], ")")
      if (nrow(s) <= list_points) {
        pts <- paste0(fx(s$x), ": ", .rgent_fmt_num(y), collapse = ", ")
        out <- c(out, sprintf("  %s: %s", name, pts))
      } else {
        lo <- which.min(y); hi <- which.max(y)
        out <- c(out, sprintf("  %s: %d points, x from %s to %s; first %s, last %s; min %s at %s; max %s at %s; mean %s",
                              name, nrow(s), fx(s$x[1]), fx(s$x[nrow(s)]),
                              .rgent_fmt_num(y[1]), .rgent_fmt_num(y[length(y)]),
                              .rgent_fmt_num(y[lo]), fx(s$x[lo]), .rgent_fmt_num(y[hi]), fx(s$x[hi]),
                              .rgent_fmt_num(mean(y, na.rm = TRUE))))
      }
    }
    if (length(ids) > max_groups) out <- c(out, sprintf("  ... and %d more groups", length(ids) - max_groups))
  }
  note <- paste(out, collapse = "\n")
  if (nchar(note) > 4000) note <- paste0(substr(note, 1, 4000), "\n...")
  note
}

#' What Claude is told when the user clicks Explain plot
#' @keywords internal
.rgent_plot_note <- function() {
  cmd <- .rgent_plot_command()
  is_gg <- is.null(cmd) || grepl("ggplot|geom_|qplot", cmd)
  drawn <- if (is_gg) tryCatch(.rgent_ggplot_note(), error = function(e) NULL) else NULL
  if (is.null(drawn) && !is.null(cmd)) {
    # Base graphics: summary statistics of the data the code plots
    res <- tryCatch(analyze_last_plot(), error = function(e) NULL)
    if (isTRUE(res$success) && length(res$analysis$analysis_results)) {
      r <- res$analysis$analysis_results
      drawn <- paste0("Summary of the plotted data:\n",
                      paste(names(r), ":", vapply(r, function(x) paste(x, collapse = "\n"), ""), collapse = "\n"))
      if (nchar(drawn) > 4000) drawn <- paste0(substr(drawn, 1, 4000), "\n...")
    }
  }
  if (is.null(cmd) && is.null(drawn)) {
    return(paste0("\n\n=== EXPLAIN PLOT ===\nThe user clicked Explain plot, but no recent plot was found. ",
                  "Say so in one sentence and suggest making a plot first. Don't write code."))
  }
  paste0(
    "\n\n=== THE PLOT TO EXPLAIN ===\n",
    if (!is.null(cmd)) paste0("Code that made it:\n```\n", cmd, "\n```\n") else "",
    if (!is.null(drawn)) paste0("What the plot draws (exact values):\n", drawn, "\n") else "",
    "\nExplain this plot to the person who made it. Use the values above for any numbers; never guess values.\n",
    "- Start with the main takeaway in one or two sentences, with numbers.\n",
    "- Then 2-3 short bullet points: notable peaks or dips, which group is highest or lowest, and anything odd ",
    "(outliers, gaps, missing values, overplotting, a misleading axis).\n",
    "- End with one improvement and the complete code for the improved plot in a single ```r block. ",
    "It may run straight away, so make it self-contained and use only objects that exist.\n",
    "No headings. Keep it short."
  )
}

# The plot as a base64 PNG for Claude to look at (NULL when there's none).
# ggplot2: the last ggplot, drawn fresh. Otherwise the plot on the current
# graphics device (RStudio's Plots pane), replayed into a PNG. 800x520 at 96 dpi
# keeps it to roughly 30-120 KB and about 550 input tokens.
# The last plot the chat ran, copied before the run's temp files go
.rgent_plot_store <- new.env(parent = emptyenv())
.rgent_keep_chat_plot <- function(file) {
  if (!length(file) || !nzchar(file) || !file.exists(file)) return(invisible(FALSE))
  dest <- file.path(tempdir(), "rgent-last-chat-plot.png")
  ok <- isTRUE(file.copy(file, dest, overwrite = TRUE))
  if (ok) .rgent_plot_store$file <- dest
  invisible(ok)
}
.rgent_kept_chat_plot <- function() {
  f <- .rgent_plot_store$file
  if (is.null(f) || !file.exists(f)) NULL else f
}

.rgent_plot_image <- function(width = 800, height = 520, max_bytes = 1e6) {
  cmd <- .rgent_plot_command()
  f <- tempfile("rgent-plot", fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  is_gg <- is.null(cmd) || grepl("ggplot|geom_|qplot", cmd)
  p <- if (is_gg && "ggplot2" %in% loadedNamespaces()) tryCatch(ggplot2::last_plot(), error = function(e) NULL) else NULL
  drew <- FALSE
  if (!is.null(p)) {
    drew <- tryCatch({
      grDevices::png(f, width = width, height = height, res = 96)
      on.exit(if (grDevices::dev.cur() > 1 && identical(names(grDevices::dev.cur()), "png")) grDevices::dev.off(), add = TRUE)
      suppressMessages(suppressWarnings(print(p)))
      grDevices::dev.off()
      TRUE
    }, error = function(e) FALSE)
  }
  if (!drew && grDevices::dev.cur() > 1) {
    rec <- tryCatch(grDevices::recordPlot(), error = function(e) NULL)
    if (!is.null(rec)) {
      drew <- tryCatch({
        cur <- grDevices::dev.cur()
        grDevices::png(f, width = width, height = height, res = 96)
        ok <- tryCatch({ suppressWarnings(grDevices::replayPlot(rec)); TRUE }, error = function(e) FALSE)
        grDevices::dev.off()
        if (cur > 1 && cur %in% grDevices::dev.list()) grDevices::dev.set(cur)
        ok
      }, error = function(e) FALSE)
    }
  }
  # The chat's own plots are drawn into a file and every device is closed after,
  # so an open device is newer than them; with none open, use the chat's picture
  if (!drew && grDevices::dev.cur() == 1) {
    kept <- .rgent_kept_chat_plot()
    if (!is.null(kept)) drew <- isTRUE(file.copy(kept, f, overwrite = TRUE))
  }
  if (!drew || !file.exists(f)) return(NULL)
  size <- file.info(f)$size
  if (is.na(size) || size < 200 || size > max_bytes) return(NULL)
  # base64_enc wraps lines every 76 characters; the API wants one unbroken string
  gsub("[\r\n]", "", jsonlite::base64_enc(readBin(f, "raw", size)))
}
