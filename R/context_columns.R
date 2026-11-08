# A short summary of each data frame column for Claude's context (2.4.12).
#
# Claude used to see only column names, and guessed: amounts stored as text,
# "N/A" or "" for missing, -99 / 9999 codes, 1/2 codes whose meaning is in a
# labels attribute, case variants of the same value. One line per column
# catches most of that without a lookup:
#   numbers and dates: min, max and the NA count
#   text and factors: the number of distinct values, and the values themselves
#     (with counts) when there are at most 12 (.rgent_col_max_values; 8 before:
#     a status column with 9 spellings made Claude look them up)
#   anything with a "labels" attribute: the labels
# Kept fast on big data: text columns over 200,000 rows are summarised from
# their first 200,000 values (said so), and only the first 40 columns.

.rgent_col_max_rows <- 200000L
.rgent_col_max_cols <- 40L
.rgent_col_max_values <- 12L

.rgent_col_fmt <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(format(x))
  trimws(format(signif(x, 6), scientific = abs(x) >= 1e9 || (abs(x) < 1e-4 && x != 0), big.mark = ""))
}

.rgent_column_summary <- function(col) {
  out <- tryCatch({
    n <- length(col)
    na <- sum(is.na(col))
    parts <- character(0)
    lab <- attr(col, "labels", exact = TRUE)
    if (!is.null(lab) && length(lab) && length(lab) <= 20)
      parts <- c(parts, paste0("labels: ", paste0(names(lab), "=", unname(lab), collapse = ", ")))
    if (is.numeric(col) || inherits(col, c("Date", "POSIXt"))) {
      ok <- col[!is.na(col)]
      if (is.numeric(ok)) ok <- ok[is.finite(ok)]
      if (length(ok)) {
        r <- range(ok)
        parts <- c(parts, paste0("range ", .rgent_col_fmt(r[1]), " to ", .rgent_col_fmt(r[2])))
        if (is.numeric(col) && !inherits(col, c("Date", "POSIXt"))) {
          u <- unique(if (length(ok) > .rgent_col_max_rows) ok[seq_len(.rgent_col_max_rows)] else ok)
          if (length(u) <= .rgent_col_max_values) {
            tab <- table(ok)
            parts <- c(parts, paste0("values ", paste0(names(tab), " (", as.integer(tab), ")", collapse = ", ")))   # e.g. 1/2 codes
          }
        }
      }
    } else if (is.character(col) || is.factor(col) || is.logical(col)) {
      part <- n > .rgent_col_max_rows
      v <- if (part) col[seq_len(.rgent_col_max_rows)] else col
      v <- as.character(v)
      tab <- table(v, useNA = "no")
      k <- length(tab)
      if (is.factor(col) && nlevels(col) > k) parts <- c(parts, paste0(nlevels(col), " levels (", nlevels(col) - k, " unused)"))
      if (k <= .rgent_col_max_values && k > 0) {
        tab <- sort(tab, decreasing = TRUE)
        vals <- names(tab); vals[nchar(vals) > 30] <- paste0(substr(vals[nchar(vals) > 30], 1, 30), "...")
        parts <- c(parts, paste0(k, " distinct: ", paste0("'", vals, "' (", as.integer(tab), ")", collapse = ", ")))
      } else {
        parts <- c(parts, paste0(k, " distinct"))
      }
      if (part) parts <- c(parts, sprintf("(from the first %s rows)", format(.rgent_col_max_rows, big.mark = ",")))
    }
    if (na) parts <- c(parts, paste0(na, " NA"))
    paste(parts, collapse = "; ")
  }, error = function(e) "")
  if (!is.character(out) || length(out) != 1) "" else out
}

# The per-column details sent with a data frame: first two values and a summary
.rgent_df_columns <- function(df) {
  k <- min(ncol(df), .rgent_col_max_cols)
  if (k == 0) return(list())
  head_data <- utils::head(df, 2)
  lapply(seq_len(k), function(i) {
    list(column = names(df)[i],
         sample_values = as.character(head_data[[i]])[1:2],
         summary = .rgent_column_summary(df[[i]]))
  })
}
