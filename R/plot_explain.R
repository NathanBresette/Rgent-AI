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

# A plot card's picture, sent by the page (Explain on that card): a PNG or
# JPEG in base64, else NULL
.rgent_card_image <- function(b64, max_chars = 1.4e6) {
  if (!is.character(b64) || length(b64) != 1 || !nzchar(b64) || nchar(b64) > max_chars) return(NULL)
  b64 <- gsub("[[:space:]]", "", b64)
  if (!grepl("^[A-Za-z0-9+/]+=*$", b64)) return(NULL)
  head <- tryCatch(jsonlite::base64_dec(substr(b64, 1, 16)), error = function(e) raw(0))
  png <- length(head) >= 4 && identical(head[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47)))
  jpg <- length(head) >= 3 && identical(head[1:3], as.raw(c(0xff, 0xd8, 0xff)))
  if (png || jpg) b64 else NULL
}

#' What Claude is told when the user clicks Explain plot
#'
#' `index`/`count`/`code` come from Explain on a plot card: which of the plots
#' the code drew the user means, and that code. The composer's Explain plot
#' sends none of them and means the last plot.
#' @keywords internal
.rgent_plot_note <- function(index = NULL, count = NULL, code = NULL) {
  index <- suppressWarnings(as.integer(index)); count <- suppressWarnings(as.integer(count))
  one_of <- length(index) == 1 && length(count) == 1 && !is.na(index) && !is.na(count) &&
    count > 1 && index >= 1 && index <= count
  cmd <- if (is.character(code) && length(code) == 1 && nzchar(trimws(code))) code else .rgent_plot_command()
  is_gg <- is.null(cmd) || grepl("ggplot|geom_|qplot", cmd)
  # ggplot2's last_plot() is the last plot the code drew, so its values only
  # describe plot N of N
  drawn <- if (is_gg && (!one_of || index == count)) tryCatch(.rgent_ggplot_note(), error = function(e) NULL) else NULL
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
    if (one_of) paste0("This code drew ", count, " plots. The user means plot ", index, " of ", count,
                       " (the ", index, .rgent_ordinal_suffix(index), " plot the code draws; it is the attached image). ",
                       "Explain only that plot.\n") else "",
    if (!is.null(cmd)) paste0(if (one_of) "Code that drew the plots:\n```\n" else "Code that made it:\n```\n", cmd, "\n```\n") else "",
    if (!is.null(drawn)) paste0("What the plot draws (exact values):\n", drawn, "\n") else "",
    if (one_of && is.null(drawn)) "\nExplain this plot to the person who made it. Read values from the image and the code; say \"about\" for numbers read off the picture.\n" else
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
  if (ok) {
    .rgent_plot_store$file <- dest
    .rgent_plot_store$device <- .rgent_device_snapshot()
  }
  invisible(ok)
}
# What the current device shows (NULL with none open), to tell later whether
# the user drew something after the chat's plot
.rgent_device_snapshot <- function() {
  if (grDevices::dev.cur() == 1) return(NULL)
  tryCatch(list(dev = grDevices::dev.cur(), plot = grDevices::recordPlot()), error = function(e) NULL)
}
.rgent_kept_chat_plot <- function() {
  f <- .rgent_plot_store$file
  if (is.null(f) || !file.exists(f)) NULL else f
}
# TRUE when nothing was drawn on the user's device since the chat's last plot
.rgent_device_unchanged <- function() {
  then <- .rgent_plot_store$device; now <- .rgent_device_snapshot()
  if (is.null(now)) return(TRUE)
  !is.null(then) && identical(then$dev, now$dev) && identical(then$plot[[1]], now$plot[[1]])
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
  # The chat draws its plots into files and closes those devices, so the user's
  # device doesn't show them: use the chat's picture unless the user has drawn
  # something since
  if (!drew && !is.null(.rgent_kept_chat_plot()) && .rgent_device_unchanged()) {
    drew <- isTRUE(file.copy(.rgent_kept_chat_plot(), f, overwrite = TRUE))
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
  if (!drew || !file.exists(f)) return(NULL)
  size <- file.info(f)$size
  if (is.na(size) || size < 200 || size > max_bytes) return(NULL)
  # base64_enc wraps lines every 76 characters; the API wants one unbroken string
  gsub("[\r\n]", "", jsonlite::base64_enc(readBin(f, "raw", size)))
}

.rgent_ordinal_suffix <- function(n) {
  if (n %% 100 %in% 11:13) return("th")
  switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
}
