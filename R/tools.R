# Claude's tools (2.5.0, backend rgent_tools.py)
#
# Claude can look things up before it answers: run a read-only R expression on
# the user's data (inspect_data), read or search the open file, read another
# text file in the project, or read a help page. The backend streams the reply;
# when Claude calls tools the stream ends with a `tool_calls` event, and
# chat_with_ai runs them here and sends the same request again with the results
# (`tool_rounds`), up to .rgent_tool_max_rounds times per question. The reply on
# the page gets a short note per lookup (a ```lookup block, see readNote()).
#
# Everything here only reads. inspect_data parses the expression and refuses
# anything that isn't on a list of reading functions (no assignment, files,
# options, packages, eval tricks), evaluates it in a fresh environment whose
# parent is the global environment, with a time limit and capped printing.
#
# options(rgent.tools = FALSE) turns the tools off (the old ```read/```find
# lookups are used then).

.rgent_tool_max_rounds <- 2L   # backend MAX_TOOL_ROUNDS

.rgent_tools_on <- function() !isFALSE(getOption("rgent.tools", TRUE))

# ---- inspect_data ----------------------------------------------------------

# Functions an inspect_data expression may call. Only functions that read and
# return a value; anything with side effects (assign, options, setwd, file and
# connection functions, eval/parse/get/do.call, system, library, Sys.setenv...)
# is left out on purpose.
.rgent_inspect_fns <- c(
  # syntax and operators
  "(", "{", "$", "@", "[", "[[", ":", "::", "~", "+", "-", "*", "/", "^", "%%", "%/%",
  "==", "!=", "<", ">", "<=", ">=", "&", "&&", "|", "||", "!", "%in%", "%>%", "|>", "if", "function",
  # structure
  "str", "summary", "head", "tail", "class", "typeof", "mode", "storage.mode", "dim", "nrow", "ncol",
  "NROW", "NCOL", "names", "colnames", "rownames", "dimnames", "length", "lengths", "levels", "nlevels",
  "attributes", "attr", "inherits", "is.na", "anyNA", "complete.cases", "is.null", "is.numeric",
  "is.character", "is.factor", "is.logical", "is.data.frame", "is.list", "is.function", "is.element",
  "is.finite", "is.infinite", "is.nan", "object.size", "format.object.size", "exists", "identity",
  "invisible", "print", "format", "formatC", "prettyNum", "toupper", "tolower", "nchar", "trimws",
  "substr", "substring", "startsWith", "endsWith", "grepl", "sprintf", "paste", "paste0", "sQuote", "dQuote",
  # values and summaries
  "table", "unique", "duplicated", "anyDuplicated", "sort", "order", "rev", "which", "which.max",
  "which.min", "any", "all", "sum", "prod", "mean", "median", "min", "max", "range", "quantile",
  "sd", "var", "IQR", "mad", "cor", "cumsum", "diff", "tabulate", "cut", "ifelse", "match", "setdiff",
  "union", "intersect", "prop.table", "proportions", "addmargins", "xtabs", "ftable", "aggregate",
  "tapply", "sapply", "vapply", "lapply", "apply", "mapply", "colSums", "rowSums", "colMeans",
  "rowMeans", "split", "subset", "with", "unlist", "seq_len", "seq_along", "seq", "rep", "c", "list",
  "character", "numeric", "integer", "logical", "round", "signif", "abs", "sqrt", "log", "log10",
  "log2", "exp", "floor", "ceiling", "trunc", "nlevels", "droplevels", "factor", "as.character",
  "as.numeric", "as.integer", "as.double", "as.logical", "as.factor", "as.Date", "as.POSIXct",
  "as.data.frame", "as.list", "as.vector", "data.frame", "t", "rowsum", "max.col", "Negate",
  "Filter", "Reduce", "weekdays", "months", "julian", "difftime", "Sys.Date", "Sys.time",
  "isTRUE", "isFALSE", "nchar", "rev", "stopifnot", "vector", "numeric_version", "packageVersion",
  "environmentName", "deparse", "body", "formals", "args",
  # dplyr / tibble / tidyr / skimr reading verbs
  "glimpse", "count", "tally", "distinct", "n_distinct", "n", "summarise", "summarize", "group_by",
  "ungroup", "filter", "select", "arrange", "desc", "slice_head", "slice_tail", "slice_max",
  "slice_min", "slice_sample", "pull", "across", "everything", "where", "starts_with", "ends_with",
  "contains", "matches", "all_of", "any_of", "mutate", "transmute", "rename", "first", "last", "nth",
  "if_else", "case_when", "between", "near", "coalesce", "na_if", "tibble", "as_tibble", "skim",
  "count_na", "drop_na", "nest", "unnest", "pivot_longer", "pivot_wider", "left_join", "inner_join",
  "anti_join", "semi_join", "bind_rows", "row_number", "lag", "lead", "cumany", "cumall",
  "str_detect", "str_sub", "str_length", "str_to_lower", "str_to_upper", "str_trim",
  "year", "month", "day", "wday", "ymd", "ymd_hms", "dmy", "mdy",
  # text, regex and other reading helpers models reach for (each was refused in the eval)
  "gsub", "sub", "regexpr", "gregexpr", "regexec", "regmatches", "strsplit", "nzchar", "chartr",
  "casefold", "sprintf", "suppressWarnings", "suppressMessages", "requireNamespace", "unname", "setNames",
  "identical", "all.equal", "rbind", "cbind", "merge", "outer", "rank", "rle", "pmin", "pmax", "cumprod",
  "cummax", "cummin", "weighted.mean", "scale", "as.POSIXlt", "strftime", "rowsum", "mapply", "Map",
  "nchar", "rev", "head", "str_replace", "str_replace_all", "str_extract", "str_count", "str_split",
  "parse_number", "is.na", "as.Date"
)
# Packages `pkg::fn` may name (the function must be on the list as well)
.rgent_inspect_pkgs <- c("base", "utils", "stats", "methods", "dplyr", "tibble", "tidyr", "skimr",
                         "pillar", "stringr", "lubridate", "forcats", "purrr", "data.table", "janitor")
# Higher-order functions: a function they call by name ("FUN = 'rm'") is checked too
.rgent_inspect_hof <- c("sapply", "vapply", "lapply", "apply", "mapply", "Map", "tapply", "aggregate", "addmargins",
                        "Filter", "Reduce", "Negate", "across", "with", "subset", "outer")

# NULL when the expression only reads, otherwise why it isn't allowed.
# Every function the expression calls must be on .rgent_inspect_fns (and not a
# function of the user's, which could do anything). Plain names that aren't
# called are data or columns (count(df, date) is fine even though date() is a
# function), except where a function is passed to be called: the arguments of
# higher-order functions (sapply(x, rm)) and the right side of %>% (x %>% rm).
.rgent_inspect_check <- function(expr, env = globalenv()) {
  bad <- NULL
  deny <- function(why) if (is.null(bad)) bad <<- why
  ok_fun <- function(nm) nm %in% .rgent_inspect_fns && !exists(nm, envir = env, mode = "function", inherits = FALSE)
  # a name or string given where a function is expected
  check_fun_ref <- function(a) {
    if (is.name(a)) {
      nm <- as.character(a)
      is_data <- exists(nm, envir = env, inherits = FALSE) && !exists(nm, envir = env, mode = "function", inherits = FALSE)
      if (nzchar(nm) && !is_data && exists(nm, envir = env, mode = "function") && !ok_fun(nm))
        deny(sprintf("`%s` isn't one of the reading functions inspect_data allows", nm))
    } else if (is.character(a) && length(a) == 1 && !is.na(a) && nzchar(a) && exists(a, envir = env, mode = "function") && !ok_fun(a)) {
      deny(sprintf("`%s` isn't one of the reading functions inspect_data allows", a))
    }
  }
  walk <- function(e) {
    if (!is.null(bad) || !is.call(e)) return(invisible())
    fn <- e[[1]]
    # pkg::fn, called or passed: both must be on the lists
    if (is.name(fn) && as.character(fn) %in% c("::", ":::")) {
      if (identical(as.character(fn), ":::")) return(deny("`:::` isn't allowed"))
      pkg <- as.character(e[[2]]); f <- as.character(e[[3]])
      if (!(pkg %in% .rgent_inspect_pkgs) || !(f %in% .rgent_inspect_fns))
        deny(sprintf("`%s::%s` isn't one of the reading functions inspect_data allows", pkg, f))
      return(invisible())
    }
    fname <- ""
    if (is.name(fn)) {
      fname <- as.character(fn)
      # A temporary variable (x <- ...) goes into the lookup's own scratch
      # environment, never the user's; only a plain name may be assigned
      if (fname %in% c("<-", "=") && length(e) == 3) {
        if (!is.name(e[[2]])) return(deny("only a plain name can be assigned (a temporary variable)"))
        # never under an allowed function's name: `print <- system; print("ls")`
        # would pass the check for print() and then run system()
        if (as.character(e[[2]]) %in% .rgent_inspect_fns) return(deny(sprintf("`%s` is a function name; use another name for the temporary variable", as.character(e[[2]]))))
        return(walk(e[[3]]))
      }
      if (fname %in% c("<<-", "->>", "assign")) return(deny("inspect_data only reads: no assignment outside the lookup"))
      if (!ok_fun(fname)) return(deny(sprintf("`%s()` isn't one of the reading functions inspect_data allows", fname)))
    } else {
      walk(fn)   # (function(x) ...)(1) or pkg::fn(...)
    }
    if (identical(fname, "function")) {
      for (d in as.list(e[[2]])) if (!missing(d)) walk(d)   # default values run too
      return(walk(e[[3]]))
    }
    args <- as.list(e)[-1]
    if (identical(fname, "%>%") && length(args) == 2 && (is.name(args[[2]]) || is.character(args[[2]]))) check_fun_ref(args[[2]])
    for (a in args) {
      if (missing(a)) next
      if (fname %in% .rgent_inspect_hof) check_fun_ref(a)
      walk(a)
    }
    invisible()
  }
  if (is.name(expr) || !is.call(expr)) return(NULL)   # a bare name or constant just prints
  walk(expr)
  bad
}

# Run one inspect_data expression: list(content, is_error)
.rgent_inspect_data <- function(code, env = globalenv(), max_chars = 4000L, time_limit = 10) {
  if (!is.character(code) || length(code) != 1 || !nzchar(trimws(code)))
    return(list(content = "No R expression was given.", is_error = TRUE))
  if (nchar(code) > 2000) return(list(content = "That expression is too long; use one short expression.", is_error = TRUE))
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) e)
  if (inherits(exprs, "error")) return(list(content = paste("Couldn't parse it:", conditionMessage(exprs)), is_error = TRUE))
  # Several statements (; or new lines) run in order, like the Console, each
  # printed; temporary variables stay in the scratch environment
  if (length(exprs) == 0) return(list(content = "No R expression was given.", is_error = TRUE))
  if (length(exprs) > 12) return(list(content = "That's too many statements for one lookup; use at most 12.", is_error = TRUE))
  for (ex in exprs) {
    why <- .rgent_inspect_check(ex, env)
    if (!is.null(why)) return(list(content = paste0("Not run: ", why, ". Use str(), summary(), table(), head(), unique() and the like."), is_error = TRUE))
  }
  scratch <- new.env(parent = env)
  old <- options(width = 100, max.print = 400, warn = 1, digits = 6)
  on.exit(options(old), add = TRUE)
  setTimeLimit(elapsed = time_limit, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  warns <- character(0)
  out <- tryCatch(withCallingHandlers(
    utils::capture.output({
      for (ex in exprs) {
        v <- withVisible(eval(ex, scratch))
        if (v$visible) print(v$value)
      }
    }),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) structure(conditionMessage(e), class = "rgent_inspect_error"))
  setTimeLimit(elapsed = Inf)
  if (inherits(out, "rgent_inspect_error")) {
    msg <- unclass(out)
    if (grepl("time limit|elapsed time", msg)) msg <- paste0("It took longer than ", time_limit, " seconds; look at a smaller part (head(), a sample).")
    return(list(content = paste("Error:", msg), is_error = TRUE))
  }
  txt <- paste(out, collapse = "\n")
  if (length(warns)) txt <- paste0(txt, "\nWarning: ", paste(unique(warns), collapse = "\nWarning: "))
  if (!nzchar(trimws(txt))) txt <- "(no output)"
  .rgent_clip_output(txt, max_chars)
}

.rgent_clip_output <- function(txt, max_chars) {
  if (nchar(txt) <= max_chars) return(list(content = txt, is_error = FALSE))
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  keep <- cumsum(nchar(lines) + 1L) <= max_chars
  keep[1] <- TRUE
  shown <- lines[keep]
  shown[1] <- substr(shown[1], 1, max_chars)
  list(content = paste0(paste(shown, collapse = "\n"), "\n... (", sum(!keep), " more lines not shown; look at a smaller part)"),
       is_error = FALSE)
}

# ---- read_project_file -----------------------------------------------------

.rgent_project_root <- function() {
  p <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  if (is.character(p) && length(p) == 1 && nzchar(p) && dir.exists(p)) p else getwd()
}

.rgent_project_text_ext <- "\\.(R|r|Rmd|rmd|qmd|Rnw|md|markdown|txt|csv|tsv|yml|yaml|json|sql|py|Rproj|css|html|tex|bib)$"
# Never read: hidden files and anything that may hold credentials or session data
.rgent_project_denied <- "(?i)(^\\.|renviron|rprofile|rhistory|\\.rdata$|\\.rds$|secret|credential|password|passwd|token|\\.pem$|\\.key$|id_rsa|\\.env$)"

# Files named `name` (any case) under root, at most 4 folders deep and 5,000
# entries looked at, so a home folder as the working directory stays quick
.rgent_find_project_file <- function(root, name, max_depth = 4L, max_seen = 5000L) {
  hits <- character(0); seen <- 0L; dirs <- ""
  for (depth in 0:max_depth) {
    nxt <- character(0)
    for (d in dirs) {
      items <- list.files(if (nzchar(d)) file.path(root, d) else root, no.. = TRUE)
      items <- items[!grepl(.rgent_project_denied, items, perl = TRUE)]
      seen <- seen + length(items)
      rel <- if (nzchar(d)) file.path(d, items) else items
      isdir <- dir.exists(file.path(root, rel))
      hits <- c(hits, rel[!isdir & tolower(items) == tolower(name)])
      nxt <- c(nxt, rel[isdir])
      if (seen > max_seen) return(hits)
    }
    dirs <- nxt
    if (!length(dirs)) break
  }
  hits
}

.rgent_read_project_file <- function(path = "", start = NULL, end = NULL, root = .rgent_project_root(),
                                     max_lines = 300L, max_chars = 12000L) {
  root_n <- normalizePath(root, winslash = "/", mustWork = FALSE)
  path <- if (is.character(path) && length(path) == 1) trimws(path) else ""
  if (grepl("^(/|~|[A-Za-z]:)", path)) {
    # an absolute path is fine only inside the project
    target <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
  } else {
    target <- normalizePath(file.path(root_n, path), winslash = "/", mustWork = FALSE)
  }
  inside <- identical(target, root_n) || startsWith(target, paste0(sub("/$", "", root_n), "/"))
  if (!inside) return(list(content = "Only files inside the project folder can be read.", is_error = TRUE))
  rel <- if (identical(target, root_n)) "" else substring(target, nchar(sub("/$", "", root_n)) + 2L)
  parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
  if (any(grepl(.rgent_project_denied, parts, perl = TRUE)))
    return(list(content = "That file can't be read (hidden, or may hold credentials).", is_error = TRUE))
  if (!file.exists(target) && nzchar(rel) && !grepl("/", rel, fixed = TRUE)) {
    # "helpers.R" for R/helpers.R: find it by name (models often leave out the folder)
    hits <- .rgent_find_project_file(root_n, rel)
    hits <- utils::head(hits[!vapply(strsplit(hits, "/", fixed = TRUE), function(p) any(grepl(.rgent_project_denied, p, perl = TRUE)), logical(1))], 20)
    if (length(hits) == 1) return(.rgent_read_project_file(hits, start, end, root, max_lines, max_chars))
    if (length(hits) > 1) return(list(content = paste0("Several files are named `", rel, "`; give the folder:\n", paste(hits, collapse = "\n")), is_error = TRUE))
  }
  if (!file.exists(target)) return(list(content = paste0("No file `", rel, "` in the project folder."), is_error = TRUE))
  if (dir.exists(target)) {
    items <- list.files(target, all.files = FALSE, no.. = TRUE)
    items <- items[!grepl(.rgent_project_denied, items, perl = TRUE)]
    if (!length(items)) return(list(content = "(empty folder)", is_error = FALSE))
    full <- file.path(target, items)
    isdir <- dir.exists(full)
    size <- vapply(seq_along(full), function(i) if (isdir[i]) "" else
      paste0("  ", format(structure(file.size(full[i]), class = "object_size"), units = "auto")), character(1))
    rows <- paste0(items, ifelse(isdir, "/", ""), size)
    more <- length(rows) > 150L
    return(list(content = paste0("Files in ", if (nzchar(rel)) rel else "the project folder", ":\n",
                                 paste(utils::head(rows, 150L), collapse = "\n"), if (more) "\n..." else ""),
                is_error = FALSE))
  }
  base <- basename(target)
  if (!grepl(.rgent_project_text_ext, base) && !(base %in% c("DESCRIPTION", "NAMESPACE", "README", "Makefile")))
    return(list(content = "Only text files (R, R Markdown, Quarto, CSV, Markdown, YAML, JSON, plain text) can be read.", is_error = TRUE))
  start <- suppressWarnings(as.integer(start %||% 1L)); if (length(start) != 1 || is.na(start) || start < 1) start <- 1L
  end <- suppressWarnings(as.integer(end %||% (start + max_lines - 1L)))
  if (length(end) != 1 || is.na(end) || end < start) end <- start + max_lines - 1L
  end <- min(end, start + max_lines - 1L)
  con <- file(target, "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  lines <- tryCatch(readLines(con, n = end, warn = FALSE), error = function(e) NULL)
  if (is.null(lines)) return(list(content = "That file couldn't be read.", is_error = TRUE))
  total_known <- length(lines) < end
  if (start > length(lines)) return(list(content = sprintf("`%s` has only %d lines.", rel, length(lines)), is_error = TRUE))
  idx <- start:length(lines)
  txt <- lines[idx]
  long <- nchar(txt, allowNA = TRUE, keepNA = FALSE) > 300L
  txt[long] <- paste0(substr(txt[long], 1L, 300L), " \u2026")
  body <- paste(sprintf("%5d| %s", idx, txt), collapse = "\n")
  head <- sprintf("%s, lines %d-%d%s:\n", rel, start, max(idx), if (total_known) " (end of file)" else "")
  .rgent_clip_output(paste0(head, body), max_chars)
}

# ---- r_help ------------------------------------------------------------------

.rgent_r_help <- function(topic, package = NULL, max_chars = 4000L) {
  if (!is.character(topic) || length(topic) != 1 || !nzchar(trimws(topic)))
    return(list(content = "No topic was given.", is_error = TRUE))
  topic <- trimws(topic)
  if (grepl("::", topic, fixed = TRUE)) { package <- sub("::.*$", "", topic); topic <- sub("^.*::", "", topic) }
  pkg <- if (is.character(package) && length(package) == 1 && nzchar(package)) package else NULL
  if (!is.null(pkg) && !requireNamespace(pkg, quietly = TRUE))
    return(list(content = sprintf("Package %s isn't installed.", pkg), is_error = TRUE))
  h <- tryCatch(do.call(utils::help, c(list(topic), if (!is.null(pkg)) list(package = pkg), list(help_type = "text"))),
                error = function(e) character(0))
  paths <- as.character(h)
  if (!length(paths)) {
    fn <- tryCatch(if (is.null(pkg)) get(topic, mode = "function") else getExportedValue(pkg, topic), error = function(e) NULL)
    if (is.function(fn)) {
      sig <- paste(utils::capture.output(print(args(fn)))[-length(utils::capture.output(print(args(fn))))], collapse = "\n")
      return(list(content = paste0("No help page found; its arguments:\n", sig), is_error = FALSE))
    }
    return(list(content = sprintf("No help page for `%s`%s.", topic, if (!is.null(pkg)) paste0(" in ", pkg) else ""), is_error = TRUE))
  }
  get_rd <- utils::getFromNamespace(".getHelpFile", "utils")
  rd <- tryCatch(get_rd(paths[1]), error = function(e) NULL)
  if (is.null(rd)) return(list(content = "That help page couldn't be read.", is_error = TRUE))
  txt <- tryCatch(utils::capture.output(tools::Rd2txt(rd, options = list(underline_titles = FALSE, width = 90))),
                  error = function(e) character(0))
  txt <- gsub("_\b", "", txt)
  pkg_name <- basename(dirname(dirname(paths[1])))
  # Keep the title, Usage and Arguments (and the start of Value)
  heads <- grep("^[A-Z][A-Za-z ]+:$", txt)
  keep_sec <- function(name, max_lines = Inf) {
    i <- heads[txt[heads] == paste0(name, ":")]
    if (!length(i)) return(character(0))
    nxt <- heads[heads > i[1]]
    j <- if (length(nxt)) nxt[1] - 1L else length(txt)
    sec <- txt[i[1]:j]
    utils::head(sec, max_lines)
  }
  title <- trimws(txt[nzchar(trimws(txt))][1])
  out <- c(sprintf("%s {%s}, version %s", title, pkg_name,
                   tryCatch(as.character(utils::packageVersion(pkg_name)), error = function(e) "?")),
           "", keep_sec("Usage"), keep_sec("Arguments"), keep_sec("Value", 12L))
  if (length(paths) > 1) out <- c(out, "", paste("Other help pages with this name:", paste(basename(dirname(dirname(paths[-1]))), collapse = ", ")))
  .rgent_clip_output(paste(out, collapse = "\n"), max_chars)
}

# ---- running a call ------------------------------------------------------------

# The open file's lines as Claude saw them (the edit target when there is one)
.rgent_tool_file <- function(edit_target) {
  if (!is.null(edit_target) && length(edit_target$lines)) return(edit_target)
  doc <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (is.null(doc) || is.null(doc$contents) || !length(doc$contents)) return(NULL)
  path <- doc$path %||% ""
  list(name = if (nzchar(path)) basename(path) else "Untitled document", lines = as.character(doc$contents))
}

# One line for the page's note: "```lookup <kind>|<what>|<ok or failed>" (no
# backticks or line breaks in a fence's info string)
.rgent_tool_note <- function(kind, what, failed = FALSE) {
  what <- gsub("[`\r\n]+", " ", what)
  what <- gsub("\\|", "\u2223", what)
  what <- gsub("\\s+", " ", trimws(what))
  if (nchar(what) > 90) what <- paste0(substr(what, 1, 87), "...")
  paste0("lookup ", kind, "|", what, "|", if (failed) "failed" else "ok")
}

# Run one tool call from Claude: list(content, is_error, note)
.rgent_run_tool <- function(call, edit_target = NULL, env = globalenv()) {
  name <- call$name %||% ""
  inp <- call$input
  if (!is.list(inp) || !is.null(inp[["_invalid"]])) {
    return(list(content = "The tool input wasn't valid JSON.", is_error = TRUE,
                note = .rgent_tool_note("data", name, TRUE)))
  }
  int <- function(x) { v <- suppressWarnings(as.integer(x)); if (length(v) == 1 && !is.na(v)) v else NULL }
  r <- tryCatch(switch(name,
    inspect_data = {
      x <- .rgent_inspect_data(inp$code, env = env)
      x$note <- .rgent_tool_note("data", inp$code %||% "", x$is_error); x
    },
    read_file_lines = {
      f <- .rgent_tool_file(edit_target)
      a <- int(inp$start); b <- int(inp$end)
      if (is.null(f)) list(content = "No file is open in the editor.", is_error = TRUE, note = .rgent_tool_note("read", "the open file", TRUE))
      else if (is.null(a) || is.null(b)) list(content = "Give start and end line numbers.", is_error = TRUE, note = .rgent_tool_note("read", "the open file", TRUE))
      else {
        b <- min(b, a + 399L)
        got <- .rgent_read_lines(f$lines, a, b)
        if (is.null(got)) list(content = sprintf("%s has %d lines; lines %d-%d don't exist.", f$name, length(f$lines), a, b),
                               is_error = TRUE, note = .rgent_tool_note("read", paste0(a, "-", b), TRUE))
        else list(content = paste0("Lines ", got$from, "-", got$to, " of ", f$name, " (", length(f$lines), " lines):\n", got$text),
                  is_error = FALSE, note = .rgent_tool_note("read", paste0(got$from, "-", got$to)))
      }
    },
    search_file = {
      f <- .rgent_tool_file(edit_target)
      p <- if (is.character(inp$pattern) && length(inp$pattern) == 1) inp$pattern else ""
      if (is.null(f)) list(content = "No file is open in the editor.", is_error = TRUE, note = .rgent_tool_note("find", p, TRUE))
      else if (!nzchar(p)) list(content = "Give a pattern.", is_error = TRUE, note = .rgent_tool_note("find", p, TRUE))
      else {
        got <- .rgent_find_lines(f$lines, p)
        list(content = paste0(got$count, " matching lines in ", f$name, ":\n", got$text), is_error = FALSE,
             note = .rgent_tool_note("find", p))
      }
    },
    read_project_file = {
      x <- .rgent_read_project_file(inp$path %||% "", int(inp$start), int(inp$end))
      x$note <- .rgent_tool_note("file", if (nzchar(inp$path %||% "")) inp$path else "the project folder", x$is_error); x
    },
    r_help = {
      x <- .rgent_r_help(inp$topic %||% "", inp$package)
      x$note <- .rgent_tool_note("help", paste0(if (is.character(inp$package) && nzchar(inp$package %||% "")) paste0(inp$package, "::") else "",
                                                inp$topic %||% ""), x$is_error); x
    },
    list(content = paste0("There is no tool called ", name, "."), is_error = TRUE, note = .rgent_tool_note("data", name, TRUE))
  ), error = function(e) list(content = paste("The lookup failed:", conditionMessage(e)), is_error = TRUE,
                              note = .rgent_tool_note("data", name, TRUE)))
  r$content <- enc2utf8(as.character(r$content %||% ""))
  r
}

# The notes shown in the reply for a round of lookups
.rgent_tool_notes_text <- function(notes, after) {
  lead <- if (!nzchar(after)) "" else if (grepl("\n\n$", after)) "" else if (grepl("\n$", after)) "\n" else "\n\n"
  paste0(lead, paste0("```", notes, "\n```", collapse = "\n"), "\n\n")
}
