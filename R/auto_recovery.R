#' Smart recovery for auto-executed code
#'
#' Many failures of AI-written code are not bugs in the code: the R session is
#' just missing something. An object is created by a chunk the user hasn't run,
#' a package from the document's setup chunk isn't attached, or a relative path
#' only works from the document's folder. Asking Claude to rewrite the code
#' can't fix those. This module fixes the session instead and re-runs the code.
#' Only errors that really are in the code go back to Claude.
#'
#' Works on whatever document is open in the RStudio source editor:
#' R Markdown / Quarto (units are code chunks) and R scripts (units are
#' top-level expressions).

# ---------------------------------------------------------------------------
# Error classification
# ---------------------------------------------------------------------------

# Match an error message against one of R's message templates. The template is
# also run through gettext() so non-English R sessions are recognised.
.rgent_match_template <- function(msg, template, domain = "R") {
  translated <- tryCatch(gettext(template, domain = domain, trim = FALSE),
                         error = function(e) template)
  for (tpl in unique(c(template, translated))) {
    rx <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", tpl)
    # A %s at the very end runs to the end of the line; others match lazily
    rx <- sub("%s$", "(.+)", rx)
    rx <- gsub("%s", "(.+?)", rx, fixed = TRUE)
    m <- regmatches(msg, regexec(rx, msg, perl = TRUE))[[1]]
    if (length(m) >= 2) return(m[2])
    if (length(m) == 1) return(m[1])  # template without %s
  }
  NULL
}

.rgent_strip_quotes <- function(x) {
  gsub("^[\"'‘’“”]+|[\"'‘’“”]+$", "", trimws(x))
}

.rgent_strip_ansi <- function(x) {
  if (is.null(x)) return(x)
  gsub("\033\\[[0-9;]*[A-Za-z]", "", x)
}

#' Work out what kind of error an execution error message is
#' @param msg The error message
#' @return A list with `type` and, when relevant, `name`
#' @keywords internal
classify_execution_error <- function(msg) {
  if (is.null(msg) || length(msg) == 0 || !nzchar(msg[1])) return(list(type = "other"))
  msg <- .rgent_strip_ansi(paste(msg, collapse = "\n"))

  m <- .rgent_match_template(msg, "object '%s' not found")
  if (!is.null(m)) return(list(type = "missing_object", name = m))

  m <- .rgent_match_template(msg, "could not find function \"%s\"")
  if (!is.null(m)) return(list(type = "missing_function", name = m))

  m <- .rgent_match_template(msg, "there is no package called %s", domain = "R-base")
  if (!is.null(m)) return(list(type = "missing_package", name = .rgent_strip_quotes(m)))

  # Files: base R and readr/vroom wordings
  m <- .rgent_match_template(msg, "cannot open file '%s': %s")
  if (is.null(m)) {
    m2 <- regmatches(msg, regexec("[\"'‘]([^\"'’]+)[\"'’] does not exist", msg, perl = TRUE))[[1]]
    if (length(m2) >= 2) m <- m2[2]
  }
  if (!is.null(m)) return(list(type = "missing_file", name = m))
  # read.csv() and friends: the file name is only in a warning
  if (!is.null(.rgent_match_template(msg, "cannot open the connection"))) {
    return(list(type = "missing_file", name = NULL))
  }

  list(type = "other")
}

# ---------------------------------------------------------------------------
# Reading the open document
# ---------------------------------------------------------------------------

#' Describe the document open in the RStudio source editor
#' @return A list with `units`, `path`, `dir`, `is_md` and `cursor`, or NULL
#' @keywords internal
get_document_info <- function() {
  ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (is.null(ctx) || length(ctx$contents) == 0) return(NULL)
  lines <- ctx$contents
  path <- ctx$path %||% ""
  ext <- if (grepl("\\.", basename(path))) tolower(sub(".*\\.", "", basename(path))) else ""
  has_chunks <- any(grepl("^\\s*`{3,}\\s*\\{\\s*[rR](?![A-Za-z0-9_])", lines, perl = TRUE))
  is_md <- ext %in% c("rmd", "qmd", "rmarkdown", "md", "rnw") || (ext == "" && has_chunks)
  units <- if (is_md) parse_chunk_units(lines) else if (ext %in% c("r", "")) parse_script_units(lines) else list()
  cursor <- tryCatch(ctx$selection[[1]]$range$start[["row"]], error = function(e) NA_integer_)
  list(
    units = units,
    path = path,
    dir = if (nzchar(path)) dirname(normalizePath(path, mustWork = FALSE)) else NULL,
    is_md = is_md,
    cursor = cursor
  )
}

#' Split R Markdown / Quarto lines into R code chunks
#' @keywords internal
parse_chunk_units <- function(lines) {
  start_rx <- "^\\s*(`{3,})\\s*\\{\\s*[rR](?![A-Za-z0-9_])(.*)\\}\\s*$"
  units <- list()
  n <- length(lines)
  i <- 1
  while (i <= n) {
    m <- regmatches(lines[i], regexec(start_rx, lines[i], perl = TRUE))[[1]]
    if (length(m) >= 3) {
      fence <- m[2]
      j <- i + 1
      while (j <= n && !grepl(paste0("^\\s*", fence, "`*\\s*$"), lines[j])) j <- j + 1
      body <- if (j > i + 1) lines[(i + 1):(min(j, n + 1) - 1)] else character(0)
      units[[length(units) + 1]] <- make_chunk_unit(m[3], body, i, length(units) + 1)
      i <- j + 1
    } else {
      i <- i + 1
    }
  }
  units
}

make_chunk_unit <- function(header, body, start_line, index) {
  header <- trimws(sub("^\\s*,", "", header))
  label <- NULL
  rest <- header
  if (nzchar(header)) {
    first <- trimws(strsplit(header, ",", fixed = TRUE)[[1]][1])
    if (!grepl("=", first, fixed = TRUE)) {
      label <- first
      rest <- trimws(sub("^[^,]*,?", "", header))
    }
  }
  opts <- if (nzchar(rest)) {
    tryCatch(eval(parse(text = paste0("alist(", rest, ")"))), error = function(e) list())
  } else list()

  eval_opt <- opts[["eval"]]

  # Quarto / knitr comment options: "#| label: x", "#| eval: false"
  hash_opts <- grep("^\\s*#\\|", body, value = TRUE)
  for (h in hash_opts) {
    kv <- regmatches(h, regexec("^\\s*#\\|\\s*([A-Za-z.\\-]+)\\s*:\\s*(.*?)\\s*$", h, perl = TRUE))[[1]]
    if (length(kv) < 3) next
    if (kv[2] == "label" && is.null(label)) label <- .rgent_strip_quotes(kv[3])
    if (kv[2] == "eval") {
      v <- kv[3]
      if (tolower(v) %in% c("false", "no")) eval_opt <- FALSE
      else if (tolower(v) %in% c("true", "yes")) eval_opt <- TRUE
      else if (grepl("^!expr\\s+", v)) {
        eval_opt <- tryCatch(parse(text = sub("^!expr\\s+", "", v))[[1]], error = function(e) NULL)
      }
    }
  }

  list(
    kind = "chunk",
    label = label,
    display = if (!is.null(label) && nzchar(label)) sprintf("chunk `%s` (line %d)", label, start_line)
              else sprintf("the chunk at line %d", start_line),
    start = start_line,
    code = paste(body, collapse = "\n"),
    eval_opt = eval_opt
  )
}

#' Split an R script into top-level expressions
#' @keywords internal
parse_script_units <- function(lines) {
  exprs <- tryCatch(parse(text = lines, keep.source = TRUE), error = function(e) NULL)
  if (is.null(exprs) || length(exprs) == 0) return(list())
  srcrefs <- attr(exprs, "srcref")
  lapply(seq_along(exprs), function(k) {
    sr <- srcrefs[[k]]
    first <- sr[1]
    last <- sr[3]
    list(
      kind = "line",
      label = NULL,
      display = if (first == last) sprintf("line %d", first) else sprintf("lines %d-%d", first, last),
      start = first,
      code = paste(lines[first:last], collapse = "\n"),
      eval_opt = NULL
    )
  })
}

# Arguments of a call, without empty ones like the gap in x[, 1] (touching an
# empty argument errors with "argument is missing")
.call_args <- function(e) {
  args <- as.list(e)[-1]
  keep <- vapply(seq_along(args), function(k) !identical(args[[k]], quote(expr = )), logical(1))
  args[keep]
}

#' Names a piece of code assigns at top level (not inside functions)
#' @keywords internal
code_defines <- function(code) {
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character(0))
  out <- character(0)
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1]]
    fname <- if (is.symbol(fn)) as.character(fn) else ""
    if (fname %in% c("function", "local", "quote", "bquote", "~", "expression")) return(invisible())
    args <- .call_args(e)
    if (fname %in% c("<-", "=", "<<-") && length(args) >= 2) {
      tgt <- args[[1]]
      if (is.symbol(tgt)) out <<- c(out, as.character(tgt))
      else if (is.character(tgt) && length(tgt) == 1) out <<- c(out, tgt)
    }
    if (fname == "assign" && length(args) >= 1 && is.character(args[[1]])) out <<- c(out, args[[1]])
    if (fname == "data") {
      for (a in args) {
        if (is.symbol(a) && nzchar(as.character(a))) out <<- c(out, as.character(a))
        else if (is.character(a)) out <<- c(out, a)
      }
    }
    for (a in args) {
      if (is.call(a)) walk(a)
    }
    invisible()
  }
  for (e in exprs) walk(e)
  unique(out)
}

#' Packages a document loads with library(), require() or pacman::p_load()
#' @keywords internal
document_packages <- function(doc) {
  if (is.null(doc) || length(doc$units) == 0) return(character(0))
  code <- paste(vapply(doc$units, function(u) u$code, character(1)), collapse = "\n")
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  pkgs <- character(0)
  if (is.null(exprs)) {
    m <- regmatches(code, gregexpr("(?:library|require)\\(\\s*[\"']?([A-Za-z0-9.]+)", code, perl = TRUE))[[1]]
    return(unique(sub(".*\\(\\s*[\"']?", "", m)))
  }
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1]]
    fname <- if (is.symbol(fn)) as.character(fn)
             else if (is.call(fn) && identical(fn[[1]], as.name("::"))) as.character(fn[[3]])
             else ""
    args <- .call_args(e)
    if (fname %in% c("library", "require", "requireNamespace") && length(args) >= 1) {
      a <- args[[1]]
      if (is.symbol(a) || is.character(a)) pkgs <<- c(pkgs, as.character(a))
    }
    if (fname == "p_load") {
      nm <- names(args) %||% rep("", length(args))
      for (k in seq_along(args)) {
        if (nzchar(nm[k])) next
        a <- args[[k]]
        if (is.symbol(a) || is.character(a)) pkgs <<- c(pkgs, as.character(a))
      }
    }
    for (a in args) if (is.call(a)) walk(a)
    invisible()
  }
  for (e in exprs) walk(e)
  unique(pkgs[nzchar(pkgs)])
}

# ---------------------------------------------------------------------------
# Running document units
# ---------------------------------------------------------------------------

# Code that we never run without the user asking: it installs or removes
# things, deletes files, or ends the session.
.rgent_unsafe_patterns <- c(
  "install\\.packages\\s*\\(" = "installs packages",
  "remove\\.packages\\s*\\(" = "removes packages",
  "BiocManager::install" = "installs packages",
  "(remotes|devtools|pak)::\\w*install" = "installs packages",
  "(^|[^.\\w])unlink\\s*\\(" = "deletes files",
  "file\\.remove\\s*\\(" = "deletes files",
  "(^|[^.\\w])rm\\s*\\(\\s*list\\s*=" = "clears the environment",
  "(^|[^.\\w])system2?\\s*\\(" = "runs shell commands",
  "(^|[^.\\w])shell\\s*\\(" = "runs shell commands",
  "(^|[^.\\w])(q|quit)\\s*\\(" = "ends the R session",
  "restartSession|\\.rs\\.restartR" = "restarts R",
  "rmarkdown::render|quarto::quarto_render|knitr::knit\\s*\\(" = "renders a document"
)

unit_unsafe_reason <- function(code) {
  code_nc <- gsub("#[^\n]*", "", code)
  for (k in seq_along(.rgent_unsafe_patterns)) {
    if (grepl(names(.rgent_unsafe_patterns)[k], code_nc, perl = TRUE)) return(unname(.rgent_unsafe_patterns[k]))
  }
  NULL
}

with_dir <- function(dir, expr) {
  if (is.null(dir) || !dir.exists(dir)) return(force(expr))
  old <- setwd(dir)
  on.exit({
    # Respect code that changed the working directory on purpose
    if (identical(normalizePath(getwd(), mustWork = FALSE), normalizePath(dir, mustWork = FALSE))) setwd(old)
  }, add = TRUE)
  force(expr)
}

#' Run one document unit in the global environment
#' @return list(ok, error)
#' @keywords internal
run_document_unit <- function(unit, doc, reason) {
  exprs <- tryCatch(parse(text = unit$code, keep.source = FALSE), error = function(e) e)
  if (inherits(exprs, "error")) return(list(ok = FALSE, error = conditionMessage(exprs)))
  cat(sprintf("\n### Rgent: running %s %s ###\n", unit$display, reason))
  # R Markdown chunks run from the document's folder in RStudio, so do the same
  run_dir <- if (isTRUE(doc$is_md)) doc$dir else NULL
  with_dir(run_dir, {
    for (e in exprs) {
      err <- tryCatch({
        eval(e, envir = .GlobalEnv)
        NULL
      }, error = function(err) .rgent_strip_ansi(conditionMessage(err)))
      if (!is.null(err)) return(list(ok = FALSE, error = err))
    }
    list(ok = TRUE, error = NULL)
  })
}

# Pick the unit that creates `name`: the last one above the cursor, otherwise
# the first one in the document.
find_defining_unit <- function(name, doc) {
  if (is.null(doc) || length(doc$units) == 0) return(NULL)
  if (is.null(doc$defs)) doc$defs <- lapply(doc$units, function(u) code_defines(u$code))
  hits <- which(vapply(doc$defs, function(d) name %in% d, logical(1)))
  if (length(hits) == 0) return(NULL)
  starts <- vapply(doc$units[hits], function(u) u$start, numeric(1))
  above <- hits[!is.na(doc$cursor) & starts < doc$cursor]
  doc$units[[if (length(above)) max(above) else min(hits)]]
}

# ---------------------------------------------------------------------------
# Recovery strategies
# ---------------------------------------------------------------------------

new_recovery_state <- function() {
  state <- new.env(parent = emptyenv())
  state$notes <- character(0)
  state$units_run <- 0
  state$max_units <- 15
  state$tried <- character(0)
  state
}

result_fixed <- function(...) list(fixed = TRUE, ...)
result_user <- function(msg) list(fixed = FALSE, user_message = msg)
result_claude <- function() list(fixed = FALSE, user_message = NULL)

#' Try to fix the session for an error, without changing the code
#' @return list(fixed = TRUE/FALSE, user_message = <why the user must act> or NULL,
#'   use_docdir = TRUE to re-run from the document's folder)
#' @keywords internal
try_recover <- function(error, doc, state, depth = 0) {
  info <- classify_execution_error(error)
  key <- paste(info$type, info$name %||% "")
  if (info$type == "other") return(result_claude())
  if (key %in% state$tried) return(result_claude())
  state$tried <- c(state$tried, key)
  if (depth > 6) return(result_claude())

  switch(info$type,
    missing_object = recover_missing_object(info$name, doc, state, depth),
    missing_function = recover_missing_function(info$name, doc, state, depth),
    missing_package = result_user(sprintf(
      "Package `%s` isn't installed. Install it with `install.packages(\"%s\")` (or `BiocManager::install(\"%s\")` if it's a Bioconductor package), then try again.",
      info$name, info$name, info$name)),
    missing_file = recover_missing_file(info$name, doc, state$code),
    result_claude()
  )
}

recover_missing_object <- function(name, doc, state, depth) {
  unit <- find_defining_unit(name, doc)
  # Not created anywhere in the document: probably a wrong name in the code,
  # which Claude can fix with the list of objects that do exist.
  if (is.null(unit)) return(result_claude())
  run_unit_with_recovery(unit, doc, state, depth, sprintf("to create `%s`", name), name)
}

fn_label <- function(fname) if (grepl("^%.*%$", fname)) sprintf("`%s`", fname) else sprintf("`%s()`", fname)

recover_missing_function <- function(fname, doc, state, depth) {
  # 1. A function the document defines itself
  unit <- find_defining_unit(fname, doc)
  if (!is.null(unit)) {
    return(run_unit_with_recovery(unit, doc, state, depth, paste("to define", fn_label(fname)), fname))
  }

  # Check exports by reading NAMESPACE files, so we don't load (and run the
  # startup code of) every package that happens to have a matching help alias
  exports_fn <- function(p) {
    if (p %in% loadedNamespaces()) {
      return(isTRUE(tryCatch(fname %in% getNamespaceExports(p), error = function(e) FALSE)))
    }
    nsi <- read_namespace_info(p)
    if (is.null(nsi)) return(FALSE)
    fname %in% nsi$exports || any(vapply(nsi$exportPatterns, function(rx) grepl(rx, fname), logical(1)))
  }
  attach_pkg <- function(p, why) {
    ok <- tryCatch({
      suppressPackageStartupMessages(library(p, character.only = TRUE))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) return(NULL)
    state$notes <- c(state$notes, sprintf("Loaded `%s` %s.", p, why))
    result_fixed()
  }

  # 2. A package the document loads (usually a setup chunk that wasn't run)
  doc_pkgs <- document_packages(doc)
  installed <- doc_pkgs[vapply(doc_pkgs, function(p) nzchar(system.file(package = p)), logical(1))]
  for (p in installed) {
    if (exports_fn(p)) {
      r <- attach_pkg(p, paste("(your document uses it) for", fn_label(fname)))
      if (!is.null(r)) return(r)
    }
  }

  # 3. Any installed package that exports it
  esc <- gsub("([][{}()+*^$|\\\\?.%])", "\\\\\\1", fname)
  hs <- tryCatch(
    suppressWarnings(utils::help.search(paste0("^", esc, "$"), fields = "alias",
                                        agrep = FALSE, ignore.case = FALSE, types = "help")),
    error = function(e) NULL)
  cands <- if (!is.null(hs) && NROW(hs$matches)) unique(as.character(hs$matches[, "Package"])) else character(0)
  cands <- cands[vapply(cands, exports_fn, logical(1))]
  if (length(cands) == 0) return(result_claude())

  pick <- NULL
  if (length(cands) == 1) {
    pick <- cands
  } else {
    # Re-exports (e.g. %>% in dplyr and tidyr) import it from its home package
    # (magrittr). Drop the re-exporters and see if one home is left.
    reexports <- vapply(cands, function(p) {
      nsi <- read_namespace_info(p)
      any(vapply(nsi$imports %||% list(), function(imp) {
        is.list(imp) && length(imp) >= 2 && imp[[1]] %in% cands && fname %in% imp[[2]]
      }, logical(1)))
    }, logical(1))
    homes <- cands[!reexports]
    loaded <- intersect(cands, loadedNamespaces())
    if (length(homes) == 1) pick <- homes
    else if (length(loaded) >= 1) pick <- loaded[1]
  }
  if (is.null(pick)) {
    return(result_user(sprintf(
      "%s is in more than one installed package (%s). Load the one you mean with `library()`, then try again.",
      fn_label(fname), paste0("`", cands, "`", collapse = ", "))))
  }
  attach_pkg(pick, paste("for", fn_label(fname))) %||% result_claude()
}

read_namespace_info <- function(p) {
  lib <- dirname(system.file(package = p))
  if (!nzchar(lib) || lib == ".") return(NULL)
  tryCatch(parseNamespaceFile(p, lib, mustExist = FALSE), error = function(e) NULL)
}

recover_missing_file <- function(path, doc, code = NULL) {
  # Without a file name in the error, look at the file paths the code mentions
  candidates <- if (!is.null(path)) path else {
    lits <- regmatches(code %||% "", gregexpr("[\"']([^\"'\n]+\\.[A-Za-z0-9]{1,8})[\"']", code %||% "", perl = TRUE))[[1]]
    unique(gsub("^[\"']|[\"']$", "", lits))
  }
  # R Markdown chunks run from the document's folder, the console doesn't
  if (!is.null(doc) && isTRUE(doc$is_md) && !is.null(doc$dir)) {
    for (p in candidates) {
      if (!file.exists(p) && file.exists(file.path(doc$dir, p))) {
        return(result_fixed(use_docdir = TRUE, note = sprintf(
          "Ran the code from your document's folder, where `%s` is.", p)))
      }
    }
  }
  missing <- candidates[!file.exists(candidates)]
  if (length(missing) == 0) return(result_claude())
  result_user(sprintf(
    "R can't find %s (the working directory is `%s`). Check the path, or use the full path to the file.",
    paste0("`", missing, "`", collapse = ", "), getwd()))
}

run_unit_with_recovery <- function(unit, doc, state, depth, reason, name) {
  if (state$units_run >= state$max_units) {
    return(result_user(sprintf(
      "`%s` comes from %s, but getting there needs more than %d earlier steps. Run the document up to that point (Run All Chunks Above), then try again.",
      name, unit$display, state$max_units)))
  }

  # Respect chunks that are switched off, e.g. {r, eval = FALSE} or eval = RUN_X
  if (!is.null(unit$eval_opt)) {
    val <- tryCatch(eval(unit$eval_opt, envir = .GlobalEnv), error = function(e) e)
    if (inherits(val, "error")) {
      inner <- try_recover(conditionMessage(val), doc, state, depth + 1)
      if (!isTRUE(inner$fixed)) {
        return(result_user(sprintf(
          "`%s` comes from %s, but I couldn't tell whether that chunk is turned on (`eval = %s`): %s",
          name, unit$display, paste(deparse(unit$eval_opt), collapse = ""), conditionMessage(val))))
      }
      val <- tryCatch(eval(unit$eval_opt, envir = .GlobalEnv), error = function(e) NA)
    }
    if (is.logical(val) && length(val) >= 1 && !isTRUE(any(val))) {
      return(result_user(sprintf(
        "`%s` comes from %s, but that chunk is turned off (`eval = %s`). Turn it on, or run it yourself, then try again.",
        name, unit$display, paste(deparse(unit$eval_opt), collapse = ""))))
    }
  }

  unsafe <- unit_unsafe_reason(unit$code)
  if (!is.null(unsafe)) {
    return(result_user(sprintf(
      "`%s` comes from %s, but that code %s, so I didn't run it automatically. Run it yourself, then try again.",
      name, unit$display, unsafe)))
  }

  for (attempt in 1:6) {
    state$units_run <- state$units_run + 1
    res <- run_document_unit(unit, doc, reason)
    if (isTRUE(res$ok)) {
      state$notes <- c(state$notes, sprintf("Ran %s %s.", unit$display, reason))
      return(result_fixed())
    }
    # The chunk needs something from further up: fix that, then run it again
    inner <- try_recover(res$error, doc, state, depth + 1)
    if (!isTRUE(inner$fixed)) {
      if (!is.null(inner$user_message)) return(inner)
      return(result_user(sprintf(
        "`%s` comes from %s, but running that code failed: %s",
        name, unit$display, res$error)))
    }
    if (state$units_run >= state$max_units) break
  }
  result_user(sprintf("`%s` comes from %s, but I couldn't get that code to run. Run it yourself, then try again.",
                      name, unit$display))
}

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

#' Short description of the objects in the global environment, for Claude
#' @keywords internal
summarize_global_env <- function(max_objects = 60) {
  nms <- ls(envir = .GlobalEnv)
  if (length(nms) == 0) return("(the global environment is empty)")
  shown <- utils::head(nms, max_objects)
  desc <- vapply(shown, function(n) {
    obj <- tryCatch(get(n, envir = .GlobalEnv), error = function(e) NULL)
    cls <- class(obj)[1]
    d <- dim(obj)
    if (!is.null(d)) sprintf("%s (%s %s)", n, cls, paste(d, collapse = "x"))
    else if (is.function(obj)) sprintf("%s (function)", n)
    else if (is.list(obj) && !is.null(names(obj))) sprintf("%s (%s of %d: %s)", n, cls, length(obj),
                                                           paste(utils::head(names(obj), 5), collapse = ", "))
    else sprintf("%s (%s, length %d)", n, cls, length(obj))
  }, character(1))
  out <- paste(desc, collapse = "; ")
  if (length(nms) > max_objects) out <- paste0(out, sprintf("; ... and %d more", length(nms) - max_objects))
  out
}

#' Execute AI code, fixing the session and re-running when the failure isn't
#' a bug in the code
#'
#' @param code The R code to execute
#' @param settings Execution settings passed to execute_code_in_session()
#' @param max_rounds How many times to fix the session and re-run
#' @return The execute_code_in_session() result plus `recovery` (notes on what
#'   was fixed), and on failure `needs_user` / `user_message` (the user has to
#'   act, so don't ask Claude) and `env_objects` (for Claude's fix prompt)
#' @keywords internal
execute_with_recovery <- function(code, settings = NULL, max_rounds = 4) {
  state <- new_recovery_state()
  state$code <- code
  doc <- NULL
  doc_loaded <- FALSE
  in_docdir <- FALSE
  result <- NULL

  for (round in seq_len(max_rounds + 1)) {
    result <- if (in_docdir) with_dir(doc$dir, execute_code_in_session(code, settings))
              else execute_code_in_session(code, settings)
    result$error <- .rgent_strip_ansi(result$error)
    if (isTRUE(result$success) || round > max_rounds) break

    if (!doc_loaded) {
      doc <- tryCatch(get_document_info(), error = function(e) NULL)
      if (!is.null(doc)) {
        doc <- list2env(doc)  # lets find_defining_unit() cache parsed definitions
      }
      doc_loaded <- TRUE
    }

    fix <- tryCatch(try_recover(result$error, doc, state),
                    error = function(e) result_claude())
    if (isTRUE(fix$fixed)) {
      if (isTRUE(fix$use_docdir)) {
        in_docdir <- TRUE
        state$notes <- c(state$notes, fix$note)
      }
      next
    }
    if (!is.null(fix$user_message)) {
      result$needs_user <- TRUE
      result$user_message <- fix$user_message
    }
    break
  }

  result$recovery <- state$notes
  if (!isTRUE(result$success)) {
    result$env_objects <- tryCatch(summarize_global_env(), error = function(e) NULL)
  }
  result
}
