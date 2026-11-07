# Data Explorer: column summaries, the filtered/sorted grid, and the code
# for the current view.
#
# One rule keeps the view and its code in step: every filter becomes a single
# R expression (`explorer_filter_expr`). The grid's rows are
# `which(<all expressions>)`, and "Convert to code" prints those same
# expressions inside dplyr::filter(). Sorting uses order(method = "radix",
# na.last = TRUE), which matches dplyr::arrange() (C-locale strings, NA last
# in both directions).

# Objects the package keeps in the global environment; never listed
.rgent_internal_globals <- c(
  ".rgent_update_checked", "auto_capture_active", "conversation_history",
  "current_access_code", "current_session_id", "currentStepCode",
  "last_context_state", "last_error_context", "last_error_message",
  "last_plot_command", "last_plot_timestamp", "operation_stopped", "websocket_server", "workspace_index"
)

.rgent_explorer_cache <- new.env(parent = emptyenv())

#' Names of the user's objects, without the package's own globals
#' @keywords internal
explorer_user_objects <- function(names) setdiff(names, .rgent_internal_globals)

# What kind of column this is, for picking filters and formatting
explorer_column_kind <- function(x) {
  if (inherits(x, "POSIXt")) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.factor(x)) return("factor")
  if (is.logical(x)) return("logical")
  if (is.integer(x)) return("integer")
  if (is.numeric(x) && !is.object(x)) return("number")
  if (is.character(x)) return("text")
  if (is.list(x)) return("list")
  if (is.numeric(x)) return("number")
  "other"
}

explorer_type_label <- function(x) {
  if (inherits(x, "POSIXct")) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.factor(x)) return(if (is.ordered(x)) "ordered factor" else "factor")
  if (is.list(x) && !is.data.frame(x)) return("list")
  cls <- class(x)[1]
  if (cls == "numeric") "double" else cls
}

# Values as they appear in the grid (NA stays NA, shown greyed out)
explorer_format_values <- function(x) {
  if (is.data.frame(x)) return(rep("<data.frame>", nrow(x)))
  out <- tryCatch({
    if (inherits(x, "POSIXt")) {
      tz <- attr(x, "tzone")
      v <- format(x, "%Y-%m-%d %H:%M:%S", tz = if (is.null(tz)) "" else tz[1])
      sub(" 00:00:00$", "", v)
    } else if (inherits(x, "Date") || is.factor(x)) {
      as.character(x)
    } else if (is.list(x)) {
      vapply(x, function(v) {
        if (is.null(v)) "<NULL>"
        else if (is.atomic(v) && length(v) == 1 && !is.object(v)) as.character(v)
        else sprintf("<%s [%d]>", class(v)[1], length(v))
      }, character(1))
    } else if (is.double(x) && !is.object(x)) {
      small <- !is.na(x) & x != 0 & (abs(x) < 1e-4 | abs(x) >= 1e15)
      v <- formatC(x, digits = 7, format = "fg", flag = "-")
      v[small] <- formatC(x[small], digits = 7, format = "g")
      v <- trimws(v)
      v[is.nan(x)] <- "NaN"
      v[is.infinite(x)] <- ifelse(x[is.infinite(x)] > 0, "Inf", "-Inf")
      v
    } else if (is.atomic(x) && !is.object(x)) {
      as.character(x)
    } else {
      format(x)
    }
  }, error = function(e) as.character(x))
  out <- as.character(out)
  if (!is.list(x)) {
    na <- is.na(x)
    if (is.double(x)) na <- na & !is.nan(x)
    out[na] <- NA
  }
  out
}

# Escape a literal for use as a regular expression
.rgent_regex_escape <- function(s) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", s)

# Turn a filter value into the column's type; errors carry a readable message
.rgent_filter_value <- function(x, kind, value, what = "value") {
  if (is.null(value) || length(value) == 0) stop("The filter needs a ", what, ".", call. = FALSE)
  value <- unlist(value)[1]
  if (kind %in% c("number", "integer")) {
    v <- suppressWarnings(as.numeric(value))
    if (is.na(v)) stop(sprintf("'%s' isn't a number.", value), call. = FALSE)
    return(v)
  }
  if (kind == "date") {
    v <- tryCatch(as.Date(as.character(value), optional = TRUE), error = function(e) NA)
    if (is.na(v)) stop(sprintf("'%s' isn't a date (use YYYY-MM-DD).", value), call. = FALSE)
    return(bquote(as.Date(.(format(v)))))
  }
  if (kind == "datetime") {
    tz <- attr(x, "tzone"); tz <- if (is.null(tz) || !nzchar(tz[1])) "" else tz[1]
    s <- sub("T", " ", as.character(value))
    v <- tryCatch(as.POSIXct(s, tz = tz, tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d")),
                  error = function(e) NA)
    if (is.na(v)) stop(sprintf("'%s' isn't a date-time (use YYYY-MM-DD HH:MM).", value), call. = FALSE)
    s <- format(v, "%Y-%m-%d %H:%M:%S", tz = tz)
    s <- sub(" 00:00:00$", "", s)
    return(if (nzchar(tz)) bquote(as.POSIXct(.(s), tz = .(tz))) else bquote(as.POSIXct(.(s))))
  }
  as.character(value)
}

#' One filter as an R expression (the same one the code shows)
#'
#' @param df the data frame
#' @param f list(column, type, value, value2)
#' @return a call
#' @keywords internal
explorer_filter_expr <- function(df, f) {
  column <- as.character(unlist(f$column))[1]
  if (is.na(column) || !column %in% names(df)) {
    stop(sprintf("Column '%s' isn't in the data any more.", column), call. = FALSE)
  }
  x <- df[[column]]
  kind <- explorer_column_kind(x)
  col <- as.name(column)
  type <- as.character(unlist(f$type))[1]
  val <- function() .rgent_filter_value(x, kind, f$value)
  val2 <- function() .rgent_filter_value(x, kind, if (!is.null(f$value2)) f$value2 else unlist(f$value)[2], "second value")
  text_col <- if (kind == "factor") bquote(as.character(.(col))) else col

  switch(type,
    is_null = , is_na = bquote(is.na(.(col))),
    is_not_null = , not_na = bquote(!is.na(.(col))),
    is_true = bquote(.(col)),
    is_false = bquote(!.(col)),
    equals = {
      if (kind == "logical") {
        v <- toupper(as.character(unlist(f$value)[1]))
        if (v == "TRUE") bquote(.(col)) else if (v == "FALSE") bquote(!.(col)) else stop("Pick TRUE or FALSE.", call. = FALSE)
      } else bquote(.(col) == .(val()))
    },
    not_equals = {
      if (kind == "logical") {
        v <- toupper(as.character(unlist(f$value)[1]))
        if (v == "TRUE") bquote(is.na(.(col)) | !.(col)) else bquote(is.na(.(col)) | .(col))
      } else bquote(is.na(.(col)) | .(col) != .(val()))
    },
    greater_than = bquote(.(col) > .(val())),
    greater_equal = bquote(.(col) >= .(val())),
    less_than = bquote(.(col) < .(val())),
    less_equal = bquote(.(col) <= .(val())),
    between = bquote(.(col) >= .(val()) & .(col) <= .(val2())),
    contains = bquote(grepl(.(.rgent_regex_escape(as.character(val()))), .(col), ignore.case = TRUE)),
    not_contains = bquote(!grepl(.(.rgent_regex_escape(as.character(val()))), .(col), ignore.case = TRUE)),
    starts_with = bquote(startsWith(.(text_col), .(as.character(val())))),
    ends_with = bquote(endsWith(.(text_col), .(as.character(val())))),
    is_empty = bquote(trimws(.(text_col)) == ""),
    is_not_empty = bquote(trimws(.(text_col)) != ""),
    stop(sprintf("Unknown filter '%s'.", type), call. = FALSE)
  )
}

# Text search across every column, case-insensitive
explorer_search_expr <- function(term, names) {
  pat <- .rgent_regex_escape(term)
  list(
    dplyr = bquote(dplyr::if_any(dplyr::everything(), ~ grepl(.(pat), .x, ignore.case = TRUE))),
    base = bquote(Reduce(`|`, lapply(.data, function(x) grepl(.(pat), x, ignore.case = TRUE)))),
    pattern = pat
  )
}

# Normalise sorts from JSON: list of list(column, direction)
.rgent_sorts <- function(sorts, df) {
  out <- list()
  for (s in sorts) {
    if (!is.list(s)) next
    col <- as.character(unlist(s$column))[1]
    dir <- as.character(unlist(s$direction))[1]
    if (is.na(col) || !col %in% names(df)) next
    out[[length(out) + 1]] <- list(column = col, desc = identical(dir, "desc"))
  }
  out
}

# Could as.character(x) contain `term`? Numbers, dates and logicals only
# use a few characters, so most terms rule them out without converting.
.rgent_search_can_match <- function(x, term) {
  t <- tolower(term)
  if (inherits(x, "POSIXt")) return(grepl("^[0-9: .-]+$", t))
  if (inherits(x, "Date")) return(grepl("^[0-9-]+$", t))
  if (is.factor(x) || is.character(x) || is.list(x) || is.object(x)) return(TRUE)
  if (is.logical(x)) return(grepl("^[truefals]+$", t))
  if (is.integer(x)) return(grepl("^-?[0-9]*$", t))
  if (is.double(x)) {
    # Substrings of "-12.5e-07", "Inf", "-Inf", "NaN"
    return(grepl("^-?[0-9]*[.]?[0-9]*(e[+-]?[0-9]*)?$", t) || grepl("^[+-]?[0-9]*$", t) ||
           any(vapply(c("-inf", "nan"), function(w) grepl(t, w, fixed = TRUE), logical(1))))
  }
  TRUE
}

# Which values of x contain the term (case-insensitive), like
# grepl(pat, x, ignore.case = TRUE) but without converting every value
.rgent_search_hits <- function(x, pat, term) {
  n <- length(x)
  if (!.rgent_search_can_match(x, term)) return(rep(FALSE, n))
  if (is.factor(x)) {
    h <- grepl(pat, levels(x), ignore.case = TRUE)[as.integer(x)]
    return(!is.na(h) & h)
  }
  by_unique <- function(v) {
    # unique()/match() on the bare numbers: on Date objects they would turn
    # every value into text first
    raw <- unclass(v)
    attributes(raw) <- NULL
    u <- unique(raw)
    ut <- u
    attributes(ut) <- attributes(v)[intersect(names(attributes(v)), c("class", "tzone", "levels"))]
    h <- grepl(pat, ut, ignore.case = TRUE)[match(raw, u)]
    !is.na(h) & h
  }
  if (inherits(x, "Date")) return(by_unique(x))
  if (inherits(x, "POSIXct") && !grepl(" ", term, fixed = TRUE)) {
    num <- as.numeric(unclass(x))
    if (all(is.na(num) | num == floor(num))) {
      # Whole seconds: as.character() gives "date" or "date HH:MM:SS". A term
      # without a space lies inside one of the two parts, so match the few
      # distinct dates and times of day instead of a string per value.
      tz <- attr(x, "tzone"); tz <- if (is.null(tz)) "" else tz[1]
      ok <- !is.na(num)
      v <- num[ok]
      # UTC offset per 15-minute bucket (offsets only change on those)
      b <- floor(v / 900)
      ub <- unique(b)
      off <- as.POSIXlt(ub * 900, origin = "1970-01-01", tz = tz)$gmtoff
      if (is.null(off) || anyNA(off)) off <- rep(0, length(ub))
      local <- v + off[match(b, ub)]
      day <- floor(local / 86400)
      sod <- local - day * 86400
      ud <- unique(day)
      dh <- grepl(pat, as.character(as.Date(ud, origin = "1970-01-01")), ignore.case = TRUE)[match(day, ud)]
      us <- unique(sod)
      txt <- sprintf("%02d:%02d:%02d", us %/% 3600, (us %% 3600) %/% 60, us %% 60)
      th <- grepl(pat, txt, ignore.case = TRUE) & us != 0
      h <- rep(FALSE, length(x))
      h[ok] <- dh | th[match(sod, us)]
      return(h)
    }
  }
  if (inherits(x, "POSIXt") && grepl("-", term, fixed = TRUE) && !grepl("[ :]", term)) {
    # Fractional seconds: a dash only occurs in the date part, which
    # as.character() always starts with, so search the distinct dates
    tz <- attr(x, "tzone"); tz <- if (is.null(tz)) "" else tz[1]
    return(by_unique(as.Date(x, tz = tz)))
  }
  if (is.numeric(x) && !is.object(x) && n > 10000) {
    u <- unique(x)
    if (length(u) < n / 2) return(by_unique(x))
  }
  grepl(pat, x, ignore.case = TRUE)
}

#' Rows of the view, in order (original row numbers)
#'
#' Cached for large data while the data, filters, sorts and search stay the same.
#' @keywords internal
explorer_view_rows <- function(df, filters = NULL, sorts = NULL, search = NULL, cache_key = NULL) {
  n <- nrow(df)
  key <- if (!is.null(cache_key)) jsonlite::toJSON(list(cache_key, filters, sorts, search), auto_unbox = TRUE) else NULL
  cache <- .rgent_explorer_cache$view
  if (!is.null(key) && !is.null(cache) && identical(cache$key, key) && identical(cache$data, df)) {
    return(cache$rows)
  }
  keep <- rep(TRUE, n)
  errors <- character(0)
  for (i in seq_along(filters)) {
    e <- tryCatch(explorer_filter_expr(df, filters[[i]]), error = function(err) err)
    if (inherits(e, "error")) {
      errors[as.character(i)] <- conditionMessage(e)
      next
    }
    cond <- tryCatch(eval(e, as.list(df), baseenv()), error = function(err) err)
    if (inherits(cond, "error") || length(cond) != n) {
      errors[as.character(i)] <- if (inherits(cond, "error")) conditionMessage(cond) else "The filter didn't give one value per row."
      next
    }
    keep <- keep & !is.na(cond) & cond
  }
  rows <- which(keep)
  if (!is.null(search) && nzchar(search)) {
    # Search only the rows the filters kept. When the term grows ("0" ->
    # "0.5") and nothing else changed, only the previous matches can still
    # match, so search just those.
    cand <- rows
    prev <- .rgent_explorer_cache$search
    same_base <- !is.null(prev) && !is.null(cache_key) && identical(prev$base_key, list(cache_key, filters)) &&
      identical(prev$data, df)
    if (same_base && grepl(tolower(prev$term), tolower(search), fixed = TRUE)) cand <- prev$rows
    pat <- .rgent_regex_escape(search)
    hit <- rep(FALSE, length(cand))
    for (x in df) {
      # Columns whose text can't contain the term are skipped (exact, and
      # avoids turning a million numbers or dates into strings)
      if (!any(!hit)) break
      todo <- which(!hit)
      hit[todo] <- tryCatch(.rgent_search_hits(x[cand[todo]], pat, search), error = function(e) FALSE)
    }
    rows <- cand[hit]
    if (!is.null(cache_key) && n > 50000) {
      .rgent_explorer_cache$search <- list(base_key = list(cache_key, filters), data = df, term = search, rows = rows)
    }
  }
  s <- .rgent_sorts(sorts, df)
  if (length(s) && length(rows) > 1) {
    keys <- lapply(s, function(k) df[[k$column]][rows])
    ok <- vapply(keys, function(k) is.atomic(k), logical(1))
    if (all(ok)) {
      o <- do.call(order, c(unname(keys), list(decreasing = vapply(s, `[[`, logical(1), "desc"),
                                               method = "radix", na.last = TRUE)))
      rows <- rows[o]
    } else {
      errors["sort"] <- "List columns can't be sorted."
    }
  }
  attr(rows, "errors") <- errors
  if (!is.null(key) && n > 50000) {
    .rgent_explorer_cache$view <- list(key = key, data = df, rows = rows)
  }
  rows
}

.rgent_get_frame <- function(name) {
  if (!is.character(name) || length(name) != 1 || !exists(name, envir = globalenv())) {
    stop(sprintf("'%s' isn't in your workspace any more.", name), call. = FALSE)
  }
  obj <- get(name, envir = globalenv())
  if (is.matrix(obj)) {
    obj <- as.data.frame(obj, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(obj)) stop(sprintf("'%s' isn't a data frame.", name), call. = FALSE)
  obj
}

# Counts of the most common values without table() (which sorts every
# distinct string, slow on large data)
.rgent_top_values <- function(x, n_top = 20) {
  if (is.factor(x)) {
    counts <- tabulate(as.integer(x), nbins = nlevels(x))
    labels <- levels(x)
  } else {
    x <- as.character(x)
    ux <- unique(x)
    counts <- tabulate(match(x, ux), nbins = length(ux))
    labels <- ux
  }
  keep <- counts > 0
  counts <- counts[keep]; labels <- labels[keep]
  o <- order(-counts, labels, method = "radix")[seq_len(min(n_top, length(counts)))]
  list(values = as.character(labels[o]), counts = as.numeric(counts[o]), total = length(counts))
}

.rgent_histogram <- function(v) {
  v <- as.numeric(v)
  v <- v[is.finite(v)]
  if (!length(v)) return(NULL)
  r <- range(v)
  if (r[1] == r[2]) return(list(breaks = c(r[1] - 0.5, r[1] + 0.5), counts = length(v)))
  # Whole numbers over a small range: one bar per value
  if (r[2] - r[1] <= 40 && all(v == floor(v))) {
    counts <- tabulate(as.integer(v - r[1] + 1), nbins = as.integer(r[2] - r[1] + 1))
    return(list(breaks = seq(r[1] - 0.5, r[2] + 0.5, by = 1), counts = as.numeric(counts)))
  }
  breaks <- seq(r[1], r[2], length.out = 21)
  counts <- tabulate(findInterval(v, breaks, rightmost.closed = TRUE, all.inside = TRUE), nbins = 20)
  list(breaks = breaks, counts = as.numeric(counts))
}

# Column summary for the summary panel
explorer_column_info <- function(name, x, n) {
  kind <- explorer_column_kind(x)
  miss <- if (is.list(x) && !is.data.frame(x)) sum(vapply(x, is.null, logical(1))) else sum(is.na(x))
  info <- list(name = name, type = explorer_type_label(x), kind = kind,
               missing = miss, missing_pct = if (n) round(100 * miss / n, 2) else 0)
  ok <- if (is.list(x)) !vapply(x, is.null, logical(1)) else !is.na(x)
  vals <- x[ok]
  info$unique_values <- tryCatch(length(unique(vals)), error = function(e) NA)
  if (!length(vals)) return(info)
  if (kind %in% c("number", "integer")) {
    v <- as.numeric(vals)
    fin <- v[is.finite(v)]
    info$histogram <- .rgent_histogram(fin)
    if (length(fin)) {
      # sum()-based mean and two-pass sd: same results as mean()/sd(),
      # several times faster on large columns
      k <- length(fin)
      m <- sum(fin) / k
      sdev <- if (k > 1) sqrt(sum((fin - m)^2) / (k - 1)) else NA_real_
      info$summary_stats <- list(min = min(fin), max = max(fin), mean = m, median = stats::median(fin),
                                 sd = sdev, se = if (k > 1) sdev / sqrt(k) else NA_real_)
    }
  } else if (kind %in% c("date", "datetime")) {
    info$histogram <- .rgent_histogram(vals)
    rng <- range(vals)
    info$summary_stats <- list(min = explorer_format_values(rng[1]), max = explorer_format_values(rng[2]),
                               median = explorer_format_values(sort(vals)[ceiling(length(vals) / 2)]),
                               range_days = round(as.numeric(difftime(rng[2], rng[1], units = "days")), 2))
  } else if (kind == "logical") {
    t <- sum(vals); f <- length(vals) - t
    info$frequency <- list(values = c("TRUE", "FALSE"), counts = c(t, f), total_categories = 2)
    info$summary_stats <- list(top_values = c("TRUE", "FALSE"), top_counts = c(t, f),
                               top_percentages = round(100 * c(t, f) / length(vals), 2),
                               total_categories = 2, total_observations = length(vals))
  } else if (kind %in% c("text", "factor")) {
    top <- .rgent_top_values(vals, 20)
    info$frequency <- list(values = top$values, counts = top$counts, total_categories = top$total)
    k <- min(5, length(top$values))
    info$summary_stats <- list(top_values = top$values[seq_len(k)], top_counts = top$counts[seq_len(k)],
                               top_percentages = round(100 * top$counts[seq_len(k)] / length(vals), 2),
                               total_categories = top$total, total_observations = length(vals))
    if (kind == "text") {
      len <- nchar(vals, type = "chars", allowNA = TRUE)
      info$summary_stats$min_length <- min(len, na.rm = TRUE)
      info$summary_stats$max_length <- max(len, na.rm = TRUE)
      info$summary_stats$avg_length <- round(mean(len, na.rm = TRUE), 2)
    }
  }
  info
}

#' Summary of a data frame (or matrix) for the explorer
#' @param df_name Name of the object
#' @keywords internal
get_dataframe_explorer_data <- function(df_name) {
  tryCatch({
    raw <- get(df_name, envir = globalenv())
    prev <- .rgent_explorer_cache$summary
    if (!is.null(prev) && identical(prev$name, df_name) && identical(prev$obj, raw)) return(prev$result)
    df <- .rgent_get_frame(df_name)
    n <- nrow(df)
    col_info <- lapply(seq_along(df), function(j) {
      tryCatch(explorer_column_info(names(df)[j], df[[j]], n),
               error = function(e) list(name = names(df)[j], type = class(df[[j]])[1], kind = "other",
                                        missing = 0, missing_pct = 0))
    })
    names(col_info) <- names(df)
    result <- list(
      exists = TRUE, name = df_name,
      type = if (is.matrix(raw)) "matrix" else "dataframe",
      class = class(raw),
      nrow = n, ncol = ncol(df), columns = names(df),
      column_info = col_info,
      size_mb = round(as.numeric(utils::object.size(raw)) / 1024^2, 2)
    )
    .rgent_explorer_cache$summary <- list(name = df_name, obj = raw, result = result)
    result
  }, error = function(e) list(exists = FALSE, error = conditionMessage(e)))
}

#' One page of the filtered, sorted view
#' @keywords internal
get_dataframe_chunk_filtered <- function(df_name, page = 1, page_size = 100,
                                         filters = NULL, sorts = NULL,
                                         pinned_cols = NULL, search = NULL) {
  tryCatch({
    df <- .rgent_get_frame(df_name)
    total <- nrow(df)
    rows <- explorer_view_rows(df, filters, sorts, search, cache_key = df_name)
    errors <- attr(rows, "errors")
    shown <- length(rows)
    page_size <- max(1L, min(1000L, as.integer(page_size)))
    pages <- max(1L, as.integer(ceiling(shown / page_size)))
    page <- min(max(1L, as.integer(page)), pages)
    idx <- if (shown) rows[seq.int((page - 1L) * page_size + 1L, min(page * page_size, shown))] else integer(0)

    cols <- names(df)
    pinned <- intersect(as.character(unlist(pinned_cols)), cols)
    cols <- c(pinned, setdiff(cols, pinned))
    data <- lapply(cols, function(cn) explorer_format_values(df[[cn]][idx]))
    names(data) <- cols
    kinds <- vapply(cols, function(cn) explorer_column_kind(df[[cn]]), character(1))

    row_labels <- if (.row_names_info(df) > 0) rownames(df)[idx] else as.character(idx)

    list(
      exists = TRUE, data = data, column_order = cols, column_kinds = as.list(kinds),
      row_labels = row_labels, row_index = idx,
      page = page, page_size = page_size, total_pages = pages,
      total_rows = total, filtered_rows = shown,
      start_row = if (shown) (page - 1L) * page_size + 1L else 0L,
      end_row = if (shown) (page - 1L) * page_size + length(idx) else 0L,
      errors = if (length(errors)) as.list(errors) else NULL
    )
  }, error = function(e) list(exists = FALSE, error = conditionMessage(e)))
}

#' R code that reproduces the current view
#'
#' dplyr when it's installed, base R otherwise.
#' @keywords internal
convert_explorer_view_to_code <- function(df_name, filters = NULL, sorts = NULL, search = NULL,
                                          style = c("auto", "dplyr", "base")) {
  style <- match.arg(style)
  df <- .rgent_get_frame(df_name)
  raw <- get(df_name, envir = globalenv())
  if (style == "auto") style <- if (requireNamespace("dplyr", quietly = TRUE)) "dplyr" else "base"
  src <- if (is.matrix(raw)) sprintf("as.data.frame(%s)", deparse(as.name(df_name), backtick = TRUE))
         else deparse(as.name(df_name), backtick = TRUE)
  exprs <- list()
  for (f in filters) {
    e <- tryCatch(explorer_filter_expr(df, f), error = function(err) NULL)
    if (!is.null(e)) exprs[[length(exprs) + 1]] <- e
  }
  s <- .rgent_sorts(sorts, df)
  has_search <- !is.null(search) && nzchar(search)
  dep <- function(e) paste(deparse(e, width.cutoff = 500L, backtick = TRUE), collapse = " ")

  if (!length(exprs) && !length(s) && !has_search) return(src)

  if (style == "dplyr") {
    conds <- vapply(exprs, dep, character(1))
    if (has_search) conds <- c(conds, dep(explorer_search_expr(search, names(df))$dplyr))
    steps <- character(0)
    if (length(conds) == 1) {
      steps <- sprintf("dplyr::filter(%s)", conds)
    } else if (length(conds) > 1) {
      steps <- paste0("dplyr::filter(\n    ", paste(conds, collapse = ",\n    "), "\n  )")
    }
    if (length(s)) {
      keys <- vapply(s, function(k) {
        cn <- deparse(as.name(k$column), backtick = TRUE)
        if (k$desc) sprintf("dplyr::desc(%s)", cn) else cn
      }, character(1))
      steps <- c(steps, sprintf("dplyr::arrange(%s)", paste(keys, collapse = ", ")))
    }
    return(paste(c(src, steps), collapse = " |>\n  "))
  }

  # Base R
  conds <- vapply(exprs, dep, character(1))
  lines <- character(0)
  data_name <- if (is.matrix(raw)) "view_data" else deparse(as.name(df_name), backtick = TRUE)
  if (is.matrix(raw)) lines <- c(lines, sprintf("view_data <- %s", src))
  if (has_search) {
    lines <- c(lines, sprintf(".data <- %s", data_name))
    conds <- c(conds, dep(explorer_search_expr(search, names(df))$base))
  }
  if (length(conds)) {
    lines <- c(lines, sprintf("rows <- with(%s, which(%s))", data_name, paste(sprintf("(%s)", conds), collapse = " & ")),
               sprintf("result <- %s[rows, , drop = FALSE]", data_name))
  } else {
    lines <- c(lines, sprintf("result <- %s", data_name))
  }
  if (length(s)) {
    keys <- vapply(s, function(k) sprintf("result[[%s]]", deparse(k$column)), character(1))
    lines <- c(lines, sprintf("result <- result[order(%s, decreasing = c(%s), method = \"radix\", na.last = TRUE), , drop = FALSE]",
                              paste(keys, collapse = ", "),
                              paste(vapply(s, function(k) if (k$desc) "TRUE" else "FALSE", character(1)), collapse = ", ")))
  }
  lines <- c(lines, "result")
  paste(lines, collapse = "\n")
}

#' Has this object changed since the explorer last looked?
#'
#' Keeps a reference to the object (no copy: R copies only when the user
#' modifies it, and then the reference is replaced on the next check).
#' @keywords internal
explorer_check_changed <- function(name) {
  if (!is.character(name) || !nzchar(name) || !exists(name, envir = globalenv())) {
    return(list(name = name, exists = FALSE, changed = TRUE))
  }
  obj <- get(name, envir = globalenv())
  seen <- .rgent_explorer_cache$seen
  changed <- is.null(seen) || !identical(seen$name, name) || !identical(seen$obj, obj)
  .rgent_explorer_cache$seen <- list(name = name, obj = obj)
  list(name = name, exists = TRUE, changed = changed)
}

#' Tab-separated text of some columns of the current view (for Copy Column)
#' @keywords internal
explorer_copy_values <- function(df_name, filters = NULL, sorts = NULL, search = NULL,
                                 columns = NULL, max_rows = 100000L) {
  df <- .rgent_get_frame(df_name)
  rows <- explorer_view_rows(df, filters, sorts, search, cache_key = df_name)
  cols <- intersect(as.character(unlist(columns)), names(df))
  if (!length(cols)) stop("No such column.", call. = FALSE)
  truncated <- length(rows) > max_rows
  rows <- rows[seq_len(min(length(rows), max_rows))]
  vals <- lapply(cols, function(cn) {
    v <- explorer_format_values(df[[cn]][rows])
    v[is.na(v)] <- "NA"
    gsub("[\t\r\n]", " ", v)
  })
  body <- if (length(rows)) do.call(paste, c(vals, sep = "\t")) else character(0)
  list(text = paste(c(paste(cols, collapse = "\t"), body), collapse = "\n"),
       rows = length(rows), truncated = truncated)
}

# ---- Other objects: lists, models, vectors, functions, environments, S4 ----
#
# The page shows one object (or a part of one) at a time and drills down
# through `path`: a list of steps, each list(t = "i"|"n"|"slot"|"env", v = ).
# Only what is shown is computed, so large or deeply nested objects stay fast.

.rgent_obj_size <- function(x) {
  tryCatch(as.numeric(utils::object.size(x)), error = function(e) NA_real_)
}

.rgent_fmt_bytes <- function(b) {
  if (is.na(b)) return("")
  if (b < 1024) return(paste(round(b), "B"))
  if (b < 1024^2) return(sprintf("%.1f KB", b / 1024))
  if (b < 1024^3) return(sprintf("%.1f MB", b / 1024^2))
  sprintf("%.2f GB", b / 1024^3)
}

# Short description such as "numeric [3]", "data.frame 32 × 11", "function(x, y)"
.rgent_obj_desc <- function(x) {
  cls <- class(x)[1]
  tryCatch({
    if (is.data.frame(x)) return(sprintf("%s %s × %s", cls, format(nrow(x), big.mark = ","), ncol(x)))
    if (is.matrix(x) || is.array(x)) return(sprintf("%s %s [%s]", typeof(x), cls, paste(dim(x), collapse = " × ")))
    if (is.function(x)) {
      a <- names(formals(x))
      return(sprintf("function(%s)", if (length(a) > 6) paste(c(a[1:6], "…"), collapse = ", ") else paste(a, collapse = ", ")))
    }
    if (is.environment(x)) return(sprintf("environment [%d]", length(ls(x, all.names = TRUE))))
    if (isS4(x)) return(sprintf("S4 %s", cls))
    if (is.null(x)) return("NULL")
    if (is.list(x)) return(sprintf("%s [%s]", cls, format(length(x), big.mark = ",")))
    if (is.atomic(x)) return(sprintf("%s [%s]", explorer_type_label(x), format(length(x), big.mark = ",")))
    if (is.language(x)) return(cls)
    cls
  }, error = function(e) cls)
}

# First values as one short line
.rgent_obj_preview <- function(x, width = 80L) {
  out <- tryCatch({
    if (is.null(x)) "NULL"
    else if (is.function(x) || is.environment(x) || isS4(x)) ""
    else if (is.data.frame(x)) paste(names(x)[seq_len(min(8, ncol(x)))], collapse = ", ")
    else if (is.atomic(x) && length(x)) {
      v <- explorer_format_values(x[seq_len(min(12, length(x)))])
      v[is.na(v)] <- "NA"
      if (is.character(x) || is.factor(x)) v <- ifelse(is.na(x[seq_len(length(v))]), "NA", paste0('"', v, '"'))
      paste(v, collapse = " ")
    } else if (is.language(x)) paste(deparse(x, width.cutoff = 200L)[1], collapse = "")
    else if (is.list(x) && length(x)) {
      n <- names(x)
      if (!is.null(n)) paste(utils::head(ifelse(nzchar(n), n, "—"), 8), collapse = ", ") else ""
    } else ""
  }, error = function(e) "")
  if (nchar(out) > width) out <- paste0(substr(out, 1, width - 1), "…")
  out
}

.rgent_has_children <- function(x) {
  (is.list(x) && length(x) > 0) || (is.environment(x) && length(ls(x, all.names = TRUE)) > 0) ||
    (isS4(x) && length(methods::slotNames(x)) > 0)
}

.rgent_is_syntactic <- function(n) nzchar(n) && make.names(n) == n

# Follow `path` from the global object; returns the part and its R expression
.rgent_resolve_path <- function(name, path) {
  obj <- get(name, envir = globalenv())
  expr <- if (.rgent_is_syntactic(name)) name else paste0("`", name, "`")
  for (step in path) {
    t <- step$t; v <- step$v
    if (identical(t, "slot")) {
      obj <- methods::slot(obj, v); expr <- paste0(expr, "@", v)
    } else if (identical(t, "env")) {
      obj <- get(v, envir = obj, inherits = FALSE)
      expr <- paste0(expr, if (.rgent_is_syntactic(v)) paste0("$", v) else paste0("[[\"", v, "\"]]"))
    } else if (identical(t, "n")) {
      obj <- obj[[v]]
      expr <- paste0(expr, if (.rgent_is_syntactic(v)) paste0("$", v) else paste0("[[\"", gsub('"', '\\\\"', v), "\"]]"))
    } else {
      i <- as.integer(v)
      obj <- obj[[i]]; expr <- paste0(expr, "[[", i, "]]")
    }
  }
  list(obj = obj, expr = expr)
}

.rgent_children <- function(x, limit = 500L) {
  mk <- function(label, step, el) list(label = label, step = step, desc = .rgent_obj_desc(el),
                                       preview = .rgent_obj_preview(el), expandable = .rgent_has_children(el))
  if (isS4(x) && !is.list(x)) {
    sl <- methods::slotNames(x)
    sl <- sl[seq_len(min(limit, length(sl)))]
    return(list(total = length(methods::slotNames(x)),
                items = lapply(sl, function(s) mk(paste0("@", s), list(t = "slot", v = s), methods::slot(x, s)))))
  }
  if (is.environment(x)) {
    nm <- sort(ls(x, all.names = TRUE))
    total <- length(nm)
    nm <- nm[seq_len(min(limit, total))]
    return(list(total = total, items = lapply(nm, function(s) {
      el <- tryCatch(get(s, envir = x, inherits = FALSE), error = function(e) NULL)
      mk(s, list(t = "env", v = s), el)
    })))
  }
  n <- length(x)
  idx <- seq_len(min(limit, n))
  nms <- names(x)
  items <- lapply(idx, function(i) {
    el <- x[[i]]
    nm <- if (!is.null(nms) && !is.na(nms[i]) && nzchar(nms[i])) nms[i] else ""
    # Name steps only when the name is unique, so obj[[name]] finds this element
    if (nzchar(nm) && sum(nms == nm, na.rm = TRUE) == 1) mk(nm, list(t = "n", v = nm), el)
    else mk(paste0("[[", i, "]]"), list(t = "i", v = i), el)
  })
  list(total = n, items = items)
}

# Printed output, trimmed
.rgent_printed <- function(x, max_lines = 300L) {
  out <- tryCatch(utils::capture.output(print(x)), error = function(e) paste("# print() failed:", conditionMessage(e)))
  total <- length(out)
  list(lines = utils::head(out, max_lines), total = total)
}

# Model-like objects (lm, glm, htest, ...) print a summary worth reading
.rgent_summary_lines <- function(x, max_lines = 300L) {
  if (!inherits(x, c("lm", "glm", "aov", "nls", "lme", "lmerMod", "glmerMod", "coxph", "survreg", "polr", "multinom", "gam", "Arima")))
    return(NULL)
  out <- tryCatch(utils::capture.output(print(summary(x))), error = function(e) NULL)
  if (is.null(out)) return(NULL)
  list(lines = utils::head(out, max_lines), total = length(out))
}

.rgent_function_code <- function(f) {
  src <- attr(f, "srcref")
  lines <- if (!is.null(src)) as.character(src) else deparse(f, width.cutoff = 80L)
  if (is.primitive(f)) lines <- deparse(f)
  lines
}

#' Describe an object (or part of one) for the Data Explorer's object view
#' @keywords internal
explorer_object_view <- function(name, path = list(), offset = 0L, limit = 1000L) {
  if (!exists(name, envir = globalenv(), inherits = FALSE)) return(list(exists = FALSE, error = paste0("`", name, "` no longer exists")))
  r <- tryCatch(.rgent_resolve_path(name, path), error = function(e) e)
  if (inherits(r, "error")) return(list(exists = FALSE, error = paste("Couldn't open that part:", conditionMessage(r))))
  x <- r$obj
  size <- .rgent_obj_size(x)
  out <- list(exists = TRUE, view = "object", name = name, expr = r$expr, depth = length(path),
              class = paste(class(x), collapse = ", "), desc = .rgent_obj_desc(x),
              size = .rgent_fmt_bytes(size))
  is_frame <- is.data.frame(x) || is.matrix(x)

  if (is.function(x)) {
    out$kind <- "function"
    fm <- formals(x)
    # Arguments without a default hold the empty symbol, which can't be stored
    # in a variable, so test it in place
    out$args <- lapply(seq_along(fm), function(k) {
      list(name = names(fm)[k],
           default = if (identical(fm[[k]], quote(expr = ))) NA_character_ else paste(deparse(fm[[k]]), collapse = " "))
    })
    out$code <- .rgent_function_code(x)
    env <- environment(x)
    out$env <- if (is.null(env)) "base" else { e <- environmentName(env); if (nzchar(e)) e else format(env) }
    return(out)
  }

  if (is_frame && length(path)) {
    # A data frame inside a list: show its first rows as printed text
    out$kind <- "printed"
    out$printed <- .rgent_printed(utils::head(x, 50))
    out$note <- sprintf("First %d of %s rows. Assign it to a variable to explore it in the grid.",
                        min(50, nrow(x)), format(nrow(x), big.mark = ","))
    return(out)
  }

  if (is.atomic(x) && !is.null(x) && is.null(dim(x))) {
    out$kind <- "vector"
    n <- length(x)
    out$length <- n
    out$info <- tryCatch(explorer_column_info(name, x, n), error = function(e) NULL)
    offset <- max(0L, as.integer(offset))
    idx <- if (n) seq.int(offset + 1L, min(n, offset + limit)) else integer(0)
    idx <- idx[idx <= n]
    vals <- explorer_format_values(x[idx])
    if (is.character(x)) vals <- x[idx]
    out$values <- as.list(vals)
    out$labels <- if (!is.null(names(x))) as.list(names(x)[idx]) else NULL
    out$offset <- offset
    out$shown <- length(idx)
    out$attrs <- setdiff(names(attributes(x)), c("names", "class", "levels", "tzone"))
    return(out)
  }

  if (.rgent_has_children(x)) {
    out$kind <- "tree"
    ch <- .rgent_children(x)
    out$children <- ch$items
    out$total <- ch$total
    sm <- .rgent_summary_lines(x)
    if (!is.null(sm)) out$summary <- sm
    # Classed lists and S4 objects usually print something useful too
    if (is.null(sm) && (isS4(x) || !identical(class(x), "list"))) out$printed <- .rgent_printed(x, 120L)
    return(out)
  }

  out$kind <- "printed"
  out$printed <- .rgent_printed(x)
  out
}
