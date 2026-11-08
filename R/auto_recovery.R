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
    lines = lines,
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
  "fs::(file|dir)_delete|file\\.rename\\s*\\(" = "deletes or moves files",
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

#' Folder R Markdown chunks run from, following RStudio's "Evaluate chunks in
#' directory" setting (document by default). NULL for scripts, or when chunks
#' run in the current directory.
#' @keywords internal
chunk_working_dir <- function(doc) {
  if (is.null(doc) || !isTRUE(doc$is_md) || is.null(doc$dir)) return(NULL)
  pref <- tryCatch(rstudioapi::readRStudioPreference("knit_working_dir", "default"),
                   error = function(e) "default")
  if (identical(pref, "current")) return(NULL)
  if (identical(pref, "project")) {
    proj <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
    if (!is.null(proj)) return(proj)
  }
  doc$dir
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
  run_dir <- chunk_working_dir(doc)
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

# Pick the unit that creates `name`.
# - `before` set (a chunk needs it): the last unit above that chunk. A chunk
#   like `meta <- meta %>% filter(...)` updates `meta`, so the original has to
#   come from further up.
# - Otherwise (the user's code needs it): the last unit above the cursor,
#   or the first one in the document.
find_defining_unit <- function(name, doc, before = NULL) {
  if (is.null(doc) || length(doc$units) == 0) return(NULL)
  if (is.null(doc$defs)) doc$defs <- lapply(doc$units, function(u) code_defines(u$code))
  hits <- which(vapply(doc$defs, function(d) name %in% d, logical(1)))
  if (length(hits) == 0) return(NULL)
  starts <- vapply(doc$units[hits], function(u) u$start, numeric(1))
  if (!is.null(before)) {
    above <- hits[starts < before]
    return(if (length(above)) doc$units[[max(above)]] else NULL)
  }
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
  state$progress <- NULL
  state
}

# Tell the user what's happening (shown live in the chat while R works)
report_progress <- function(state, msg) {
  if (is.function(state$progress)) try(state$progress(msg), silent = TRUE)
  invisible()
}

result_fixed <- function(...) list(fixed = TRUE, ...)
result_user <- function(msg) list(fixed = FALSE, user_message = msg)
result_claude <- function() list(fixed = FALSE, user_message = NULL)

#' Try to fix the session for an error, without changing the code
#' @return list(fixed = TRUE/FALSE, user_message = <why the user must act> or NULL,
#'   use_docdir = TRUE to re-run from the document's folder)
#' @keywords internal
try_recover <- function(error, doc, state, depth = 0, before = NULL) {
  info <- classify_execution_error(error)
  key <- paste(info$type, info$name %||% "", before %||% "")
  if (info$type == "other") return(result_claude())
  if (key %in% state$tried) return(result_claude())
  state$tried <- c(state$tried, key)
  if (depth > 6) return(result_claude())

  switch(info$type,
    missing_object = recover_missing_object(info$name, doc, state, depth, before),
    missing_function = recover_missing_function(info$name, doc, state, depth, before),
    missing_package = result_user(sprintf(
      "Package `%s` isn't installed. Install it with `install.packages(\"%s\")` (or `BiocManager::install(\"%s\")` if it's a Bioconductor package), then try again.",
      info$name, info$name, info$name)),
    missing_file = recover_missing_file(info$name, doc, state$code, state$run_dir),
    result_claude()
  )
}

recover_missing_object <- function(name, doc, state, depth, before = NULL) {
  unit <- find_defining_unit(name, doc, before)
  # Not created anywhere in the document: probably a wrong name in the code,
  # which Claude can fix with the list of objects that do exist.
  if (is.null(unit)) return(result_claude())
  run_unit_with_recovery(unit, doc, state, depth, sprintf("to create `%s`", name), name)
}

fn_label <- function(fname) if (grepl("^%.*%$", fname)) sprintf("`%s`", fname) else sprintf("`%s()`", fname)

recover_missing_function <- function(fname, doc, state, depth, before = NULL) {
  # 1. A function the document defines itself
  unit <- find_defining_unit(fname, doc, before)
  if (!is.null(unit)) {
    return(run_unit_with_recovery(unit, doc, state, depth, paste("to define", fn_label(fname)), fname))
  }

  # Check exports by reading NAMESPACE files, so we don't load (and run the
  # startup code of) every package that happens to have a matching help alias
  # "named": the NAMESPACE exports it by name. "pattern": only an
  # exportPattern() matches it (e.g. picante's "^[^.]"), which says nothing
  # about whether the package has it. NA: not exported.
  export_kind <- function(p) {
    if (p %in% loadedNamespaces()) {
      has <- isTRUE(tryCatch(fname %in% getNamespaceExports(p), error = function(e) FALSE))
      return(if (has) "named" else NA_character_)
    }
    nsi <- read_namespace_info(p)
    if (is.null(nsi)) return(NA_character_)
    if (fname %in% nsi$exports) return("named")
    if (any(vapply(nsi$exportPatterns, function(rx) grepl(rx, fname), logical(1)))) return("pattern")
    NA_character_
  }
  # Attach a package, and keep it only if it really provides the function
  attach_pkg <- function(p, why) {
    was_attached <- paste0("package:", p) %in% search()
    report_progress(state, sprintf("Loading `%s` for %s", p, fn_label(fname)))
    ok <- tryCatch({
      suppressPackageStartupMessages(library(p, character.only = TRUE))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) return(NULL)
    if (!exists(fname, mode = "function")) {
      if (!was_attached) try(detach(paste0("package:", p), character.only = TRUE), silent = TRUE)
      return(NULL)
    }
    state$notes <- c(state$notes, sprintf("Loaded `%s` %s.", p, why))
    result_fixed()
  }

  # 2. A package the document loads (usually a setup chunk that wasn't run).
  #    Packages that export it by name first.
  doc_pkgs <- document_packages(doc)
  installed <- doc_pkgs[vapply(doc_pkgs, function(p) nzchar(system.file(package = p)), logical(1))]
  kinds <- vapply(installed, export_kind, character(1))
  for (p in c(installed[kinds %in% "named"], installed[kinds %in% "pattern"])) {
    r <- attach_pkg(p, paste("(your document uses it) for", fn_label(fname)))
    if (!is.null(r)) return(r)
  }

  # 3. Any installed package that exports it
  report_progress(state, sprintf("Looking for the package that has %s", fn_label(fname)))
  esc <- gsub("([][{}()+*^$|\\\\?.%])", "\\\\\\1", fname)
  hs <- tryCatch(
    suppressWarnings(utils::help.search(paste0("^", esc, "$"), fields = "alias",
                                        agrep = FALSE, ignore.case = FALSE, types = "help")),
    error = function(e) NULL)
  cands <- if (!is.null(hs) && NROW(hs$matches)) unique(as.character(hs$matches[, "Package"])) else character(0)
  cand_kinds <- vapply(cands, export_kind, character(1))
  cands <- if (any(cand_kinds %in% "named")) cands[cand_kinds %in% "named"] else cands[!is.na(cand_kinds)]
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

recover_missing_file <- function(path, doc, code = NULL, run_dir = NULL) {
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
    "R can't find %s (looked in `%s`). Check the path, or use the full path to the file.",
    paste0("`", missing, "`", collapse = ", "), run_dir %||% getwd()))
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
      inner <- try_recover(conditionMessage(val), doc, state, depth + 1, before = unit$start)
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
    report_progress(state, sprintf("Running %s %s", unit$display, reason))
    res <- run_document_unit(unit, doc, reason)
    if (isTRUE(res$ok)) {
      state$notes <- c(state$notes, sprintf("Ran %s %s.", unit$display, reason))
      return(result_fixed())
    }
    # The chunk needs something from further up: fix that, then run it again
    inner <- try_recover(res$error, doc, state, depth + 1, before = unit$start)
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
# Questions from the code
# ---------------------------------------------------------------------------
#
# Code sometimes asks a question in the Console: ggsave() asks whether to
# create a folder, installers ask Yes/No, scripts call readline(). Nobody is
# watching the Console while Rgent runs code, so R would wait forever. While
# Rgent runs code, the functions packages use to ask (menu, readline,
# askYesNo, scan from the console) are swapped for versions that stop and pass
# the question to the chat. The chat shows it with buttons or a text box, and
# the code runs again with the answer.

# Replace a function in a namespace (and its attached package) until the
# returned restore function is called
.rgent_swap_function <- function(name, ns, fn) {
  old <- get(name, envir = asNamespace(ns))
  pkg <- paste0("package:", ns)

  # Every place a package can see the function: the namespace, the attached
  # package, and the import environments of packages that copied it in with
  # importFrom() (e.g. importFrom(utils, menu) in caret or DESeq2)
  places <- function() {
    envs <- list(asNamespace(ns))
    if (pkg %in% search()) envs <- c(envs, list(as.environment(pkg)))
    for (n in loadedNamespaces()) {
      imp <- tryCatch(parent.env(asNamespace(n)), error = function(e) NULL)
      if (!is.null(imp) && exists(name, envir = imp, inherits = FALSE)) envs <- c(envs, list(imp))
    }
    envs
  }
  # Replace `from` with `to` wherever the binding currently holds `from`
  replace_binding <- function(from, to) {
    for (e in places()) {
      cur <- tryCatch(get(name, envir = e, inherits = FALSE), error = function(err) NULL)
      if (!identical(cur, from)) next
      locked <- bindingIsLocked(name, e)
      if (locked) unlockBinding(name, e)
      assign(name, to, envir = e)
      if (locked) lockBinding(name, e)
    }
  }
  replace_binding(old, fn)
  # Restoring also fixes copies made by packages loaded while it was swapped
  function() replace_binding(fn, old)
}

# Lines printed to the captured output just before a question. Some packages
# print the question with cat() (rlang::check_installed), not message().
# The engine captures output with capture.output(), which writes into a text
# connection in its own frame; finish the current line and read it from there.
.rgent_recent_output <- function(n = 3) {
  if (sink.number() == 0) return(character(0))
  cat("\n")
  for (fr in rev(sys.frames())) {
    if (!exists("file", envir = fr, inherits = FALSE) || !exists("rval", envir = fr, inherits = FALSE)) next
    con <- get("file", envir = fr, inherits = FALSE)
    if (!inherits(con, "textConnection")) next
    out <- tryCatch(textConnectionValue(con), error = function(e) character(0))
    out <- out[nzchar(trimws(out))]
    return(utils::tail(out, n))
  }
  character(0)
}

.rgent_input_needed <- function(state, kind, prompt, choices = NULL) {
  # Drop cli's bullet symbols ("ℹ Would you like ...") for the chat
  tidy <- function(x) gsub("^\\s*[\u2139\u2716\u2714\u2022\u2192!*>-]+\\s+", "", x, perl = TRUE)
  recent <- c(utils::tail(state$recent_messages %||% character(0), 3),
              tryCatch(.rgent_recent_output(), error = function(e) character(0)))
  recent <- tidy(trimws(.rgent_strip_ansi(recent)))
  recent <- utils::tail(unique(recent[nzchar(recent)]), 3)
  state$pending_input <- list(
    kind = kind,
    prompt = tidy(trimws(.rgent_strip_ansi(paste(prompt %||% "", collapse = " ")))),
    choices = choices,
    context = recent,
    answers_given = length(state$answers_used)
  )
  cat("\n### Rgent: the code is asking a question. Answer it in the Rgent chat. ###\n")
  stop(structure(class = c("rgent_input_needed", "error", "condition"),
                 list(message = "Waiting for your answer in the Rgent chat", call = NULL)))
}

.rgent_next_answer <- function(state) {
  if (length(state$answers) == 0) return(NULL)
  a <- state$answers[[1]]
  state$answers <- state$answers[-1]
  state$answers_used <- c(state$answers_used, a)
  a
}

#' Swap in the question-catching versions of menu(), readline(), askYesNo()
#' and console scan(); returns a function that restores the originals
#' @keywords internal
install_prompt_handlers <- function(state) {
  restores <- list()
  safe_swap <- function(name, ns, fn) {
    r <- tryCatch(.rgent_swap_function(name, ns, fn), error = function(e) NULL)
    if (!is.null(r)) restores[[length(restores) + 1]] <<- r
  }

  safe_swap("menu", "utils", function(choices, graphics = FALSE, title = NULL) {
    a <- .rgent_next_answer(state)
    if (!is.null(a)) {
      # The chat sends the 1-based position; accept the choice text too
      n <- match(a, choices)
      if (is.na(n)) n <- suppressWarnings(as.integer(a))
      return(if (is.na(n)) 0L else n)
    }
    .rgent_input_needed(state, "choice", title, as.character(choices))
  })

  safe_swap("readline", "base", function(prompt = "") {
    a <- .rgent_next_answer(state)
    if (!is.null(a)) return(as.character(a))
    .rgent_input_needed(state, "text", prompt)
  })

  safe_swap("askYesNo", "utils", function(msg, default = TRUE, prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))), ...) {
    a <- .rgent_next_answer(state)
    if (!is.null(a)) return(switch(tolower(a), "1" = , yes = TRUE, "2" = , no = FALSE, NA))
    .rgent_input_needed(state, "choice", msg, c("Yes", "No", "Cancel"))
  })

  # select.list() would open a Tcl/Tk or OS dialog in some setups (and hang
  # on macOS without XQuartz), so ask in the chat no matter which GUI is around
  safe_swap("select.list", "utils", function(choices, preselect = NULL, multiple = FALSE, title = NULL,
                                             graphics = getOption("menu.graphics")) {
    labels <- as.character(choices)
    a <- .rgent_next_answer(state)
    if (!is.null(a)) {
      if (isTRUE(multiple)) {
        idx <- suppressWarnings(as.integer(strsplit(trimws(a), "[ ,]+")[[1]]))
        idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(labels)]
        return(labels[unique(idx)])
      }
      n <- match(a, labels)
      if (is.na(n)) n <- suppressWarnings(as.integer(a))
      return(if (is.na(n) || n < 1 || n > length(labels)) "" else labels[n])
    }
    if (isTRUE(multiple)) {
      .rgent_input_needed(state, "text", paste0(
        if (is.null(title)) "Choose one or more" else title,
        ". Type the numbers, separated by spaces: ",
        paste0(seq_along(labels), ") ", labels, collapse = "   ")))
    }
    .rgent_input_needed(state, "choice", title, labels)
  })

  orig_scan <- base::scan
  scan_fn <- function() {
    mc <- match.call()
    # Only reading from the Console counts; scan(text = ...) also has file = ""
    if (identical(file, "") && missing(text)) {
      a <- .rgent_next_answer(state)
      if (is.null(a)) .rgent_input_needed(state, "text", "Enter values (separated by spaces)")
      return(orig_scan(text = a, what = what, quiet = TRUE))
    }
    mc[[1]] <- orig_scan
    eval(mc, parent.frame())
  }
  formals(scan_fn) <- formals(orig_scan)
  safe_swap("scan", "base", scan_fn)

  function() for (r in rev(restores)) try(r(), silent = TRUE)
}

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

#' One-line description of an object, e.g. "melsi_tbl: tibble 4x5; columns:
#' Time <chr>, n <int>, ..."
#' @keywords internal
describe_object_brief <- function(name, env = .GlobalEnv) {
  obj <- tryCatch(get(name, envir = env), error = function(e) NULL)
  cls <- class(obj)[1]
  short <- function(x, n = 40) if (nchar(x) > n) paste0(substr(x, 1, n - 3), "...") else x
  if (is.data.frame(obj)) {
    cols <- names(obj)
    types <- vapply(obj, function(col) {
      t <- class(col)[1]
      switch(t, character = "chr", integer = "int", numeric = "dbl", logical = "lgl",
             factor = "fct", Date = "date", POSIXct = "dttm", list = "list", t)
    }, character(1))
    shown <- utils::head(seq_along(cols), 25)
    more <- if (length(cols) > 25) sprintf(", +%d more", length(cols) - 25) else ""
    sprintf("%s: %s %dx%d; columns: %s%s", name, cls, nrow(obj), ncol(obj),
            paste0(cols[shown], " <", types[shown], ">", collapse = ", "), more)
  } else if (is.function(obj)) {
    sig <- tryCatch(paste(utils::head(deparse(args(obj)), -1), collapse = " "), error = function(e) "function")
    sprintf("%s: %s", name, short(gsub("\\s+", " ", sig), 120))
  } else if (is.list(obj)) {
    nms <- names(obj)
    sprintf("%s: %s of %d%s", name, cls, length(obj),
            if (length(nms)) paste0(" (", paste(utils::head(nms, 10), collapse = ", "),
                                    if (length(nms) > 10) ", ..." else "", ")") else "")
  } else if (is.atomic(obj) && length(obj) == 1) {
    sprintf("%s: %s %s", name, cls, short(paste(format(obj), collapse = "")))
  } else if (!is.null(dim(obj))) {
    sprintf("%s: %s %s", name, cls, paste(dim(obj), collapse = "x"))
  } else {
    sprintf("%s: %s, length %d", name, cls, length(obj))
  }
}

# Lines of the open document that assign `name`, e.g. "line 845: melsi_rows <- list()"
grep_definitions <- function(name, doc, max_hits = 3) {
  if (is.null(doc) || is.null(doc$lines)) return(character(0))
  esc <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", name)
  # name <- ..., name <<- ..., ... -> name, a line starting with name = ..., assign("name", ...)
  rx <- paste0("(^|[^.\\w])", esc, "\\s*<<?-|->>?\\s*", esc, "(?![.\\w])|^\\s*", esc,
               "\\s*=(?!=)|assign\\(\\s*[\"']", esc, "[\"']")
  hits <- grep(rx, doc$lines, perl = TRUE)
  hits <- hits[!grepl("^\\s*#", doc$lines[hits])]
  if (length(hits) == 0) return(character(0))
  # Where it's first created, plus the latest changes
  hits <- unique(c(utils::head(hits, 1), utils::tail(hits, max_hits - 1)))
  vapply(hits, function(h) sprintf("line %d: %s", h, trimws(substr(doc$lines[h], 1, 160))), character(1))
}

#' Small, targeted context for asking Claude to fix failed code
#'
#' Instead of the whole workspace and file, describe only what the failing code
#' touches: the objects it uses, where a missing name is defined in the open
#' document, and similar names that do exist (typos, column names).
#' @keywords internal
build_fix_context <- function(code, error, doc = NULL, max_chars = 1800) {
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  used <- if (is.null(exprs)) character(0) else unique(all.names(exprs))
  used <- used[grepl("^[.A-Za-z][.A-Za-z0-9_]*$", used)]
  used <- setdiff(used, code_defines(code))
  existing <- used[vapply(used, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)]

  out <- character(0)
  if (length(existing)) {
    out <- c(out, "Objects the code uses:",
             paste0("- ", vapply(existing, function(n) tryCatch(describe_object_brief(n), error = function(e) n), character(1))))
  }

  info <- classify_execution_error(error)
  if (info$type %in% c("missing_object", "missing_function") && !is.null(info$name)) {
    nm <- info$name
    defs <- grep_definitions(nm, doc)
    file_name <- if (!is.null(doc) && nzchar(doc$path %||% "")) basename(doc$path) else "the open file"
    if (length(defs)) {
      out <- c(out, sprintf("`%s` is assigned in %s at:", nm, file_name), paste0("- ", defs))
    } else if (!is.null(doc)) {
      out <- c(out, sprintf("`%s` isn't assigned anywhere in %s.", nm, file_name))
    }
    # Similar names: objects in the session, and columns of the data frames used
    pool <- explorer_user_objects(ls(envir = .GlobalEnv))
    for (n in existing) {
      obj <- tryCatch(get(n, envir = .GlobalEnv), error = function(e) NULL)
      if (is.data.frame(obj)) pool <- c(pool, names(obj))
    }
    similar <- unique(agrep(nm, setdiff(pool, nm), max.distance = 0.2, value = TRUE))
    if (length(similar)) out <- c(out, sprintf("Similar names that exist: %s", paste(utils::head(similar, 6), collapse = ", ")))
  }

  txt <- paste(out, collapse = "\n")
  if (nchar(txt) > max_chars) txt <- paste0(substr(txt, 1, max_chars - 4), " ...")
  txt
}

#' Execute AI code, fixing the session and re-running when the failure isn't
#' a bug in the code
#'
#' @param code The R code to execute
#' @param settings Execution settings passed to execute_code_in_session()
#' @param max_rounds How many times to fix the session and re-run
#' @param progress Optional function(message) called with a short status line
#'   at each step, so the chat can show what's happening
#' @return The execute_code_in_session() result plus `recovery` (notes on what
#'   was fixed), and on failure `needs_user` / `user_message` (the user has to
#'   act, so don't ask Claude) and `fix_context` (targeted lookup for Claude's fix prompt)
#' @keywords internal
execute_with_recovery <- function(code, settings = NULL, max_rounds = 4, progress = NULL,
                                  answers = character(0), resume = NULL) {
  state <- new_recovery_state()
  state$progress <- progress
  state$code <- code
  state$answers <- as.character(unlist(answers))
  state$answers_used <- character(0)
  state$expr_mark <- 0L
  result <- NULL

  # Resuming (after a question, or after an automatic fix): start at the line
  # that stopped, so lines that already ran don't run twice, and keep the
  # output they produced. `resume` comes back from the chat as
  # list(start_at, output).
  start_at <- suppressWarnings(as.integer(unlist(resume$start_at)))
  if (length(start_at) != 1 || is.na(start_at) || start_at < 1) start_at <- 1L
  carried_output <- unlist(resume$output)
  carried_output <- if (is.character(carried_output) && length(carried_output))
    paste(carried_output, collapse = "\n") else ""
  carry <- function(txt) {
    if (is.character(txt) && length(txt) == 1 && nzchar(txt)) {
      carried_output <<- if (nzchar(carried_output)) paste(carried_output, txt, sep = "\n") else txt
    }
  }
  # Answers given while the current line ran belong to it; answers before
  # that belong to lines that finished and won't run again
  on_expr_start <- function(i) state$expr_mark <- length(state$answers_used)

  # Questions the code asks go to the chat instead of the Console
  restore_prompts <- install_prompt_handlers(state)
  on.exit(restore_prompts(), add = TRUE)

  # The open document (an environment, so find_defining_unit() can cache)
  doc <- tryCatch(list2env(get_document_info()), error = function(e) NULL)
  # Code for an R Markdown document runs from the same folder as its chunks,
  # so relative paths like "output/figures" mean the same thing
  run_dir <- chunk_working_dir(doc)
  state$run_dir <- run_dir
  in_docdir <- FALSE

  # Remember the last few messages, so a question like "Would you like to
  # create a new directory?" can be shown with what was printed before it
  remember_message <- function(m) {
    state$recent_messages <- utils::tail(c(state$recent_messages, conditionMessage(m)), 5)
  }

  withCallingHandlers(message = remember_message, for (round in seq_len(max_rounds + 1)) {
    if (round > 1) report_progress(state, "Running your code again")
    dir_now <- if (in_docdir && !is.null(doc)) doc$dir else run_dir
    # Some code cancels with invokeRestart("abort"), e.g. rlang::check_installed
    # when the answer is No. That would unwind past everything and the chat
    # would never hear back, so catch it here.
    run <- function() execute_code_in_session(code, settings, start_at = start_at,
                                              on_expr_start = on_expr_start)
    result <- withRestarts(
      if (!is.null(dir_now)) with_dir(dir_now, run()) else run(),
      abort = function(...) list(success = FALSE, error = "The code was cancelled.", cancelled = TRUE))
    if (isTRUE(result$cancelled)) {
      result$needs_user <- TRUE
      result$user_message <- if (length(state$answers_used))
        "The code stopped after your answer, so nothing else ran." else "The code was cancelled before it finished."
      break
    }
    result$error <- .rgent_strip_ansi(result$error)
    # The code asked a question: stop here and ask the user in the chat
    if (!is.null(state$pending_input)) break
    if (isTRUE(result$success) || round > max_rounds) break

    # The next round resumes at the line that failed. Keep what ran before it,
    # and put back the answers that line used so it gets them again
    if (!is.null(result$failed_at)) {
      carry(result$done_output)
      start_at <- result$failed_at
      n_used <- length(state$answers_used)
      if (n_used > state$expr_mark) {
        state$answers <- c(state$answers_used[(state$expr_mark + 1):n_used], state$answers)
        state$answers_used <- state$answers_used[seq_len(state$expr_mark)]
      }
    }

    if (classify_execution_error(result$error)$type %in% c("missing_object", "missing_function")) {
      report_progress(state, "Your code needs something that isn't loaded yet. Finding it")
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
  })

  if (!is.null(state$pending_input)) {
    result$success <- FALSE
    result$needs_input <- state$pending_input
    # Only the answers the stopped line needs; the chat adds the new one
    used <- state$answers_used
    result$answers <- as.list(if (length(used) > state$expr_mark) used[(state$expr_mark + 1):length(used)] else character(0))
    if (!is.null(result$failed_at)) {
      carry(result$done_output)
      start_at <- result$failed_at
    }
    result$resume <- list(start_at = start_at, output = carried_output)
    result$recovery <- state$notes
    return(result)
  }

  # Put the output of lines that ran in earlier rounds first
  if (nzchar(carried_output)) {
    result$output <- if (is.character(result$output) && length(result$output) == 1 && nzchar(result$output))
      paste(carried_output, result$output, sep = "\n") else carried_output
    if (isTRUE(result$success) && is.null(result$plot) && grepl("PLOT_DATA:", carried_output, fixed = TRUE)) {
      result$plot <- list(created = TRUE, message = "Plot created and displayed in RStudio",
                          mime_type = "image/png")
    }
  }

  result$recovery <- state$notes
  if (!isTRUE(result$success)) {
    result$fix_context <- tryCatch(build_fix_context(code, result$error, doc), error = function(e) "")
  }
  result
}

#' Short description of the file open in the RStudio editor, for the greeting
#' @return list(name, folder, kind, lines, cursor_line, chunk), or NULL when no
#'   file is open
#' @keywords internal
describe_active_file <- function() {
  ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (is.null(ctx) || is.null(ctx$contents)) return(NULL)
  path <- ctx$path %||% ""
  name <- if (nzchar(path)) basename(path) else "an unsaved file"
  ext <- if (grepl("\\.", name)) tolower(sub(".*\\.", "", name)) else ""
  kind <- switch(ext, rmd = "R Markdown", qmd = "Quarto", r = "R script", rnw = "Sweave",
                 md = "Markdown", py = "Python", sql = "SQL", NULL)
  lines <- ctx$contents
  # Unsaved tabs have no extension: recognize R Markdown by its chunk fences
  if (is.null(kind) && !nzchar(path) && isTRUE(tryCatch(.rgent_is_markdown(name, lines), error = function(e) FALSE))) {
    kind <- "R Markdown"; ext <- "rmd"
  }
  # RStudio reports an empty buffer as one empty line
  n_lines <- if (length(lines) == 1 && !nzchar(lines)) 0L else length(lines)
  if (!nzchar(path) && n_lines == 0) return(NULL)  # empty Untitled tab: nothing to say
  cursor <- tryCatch(as.integer(ctx$selection[[1]]$range$start[["row"]]), error = function(e) NA_integer_)

  chunk <- NULL
  if (!is.na(cursor) && ext %in% c("rmd", "qmd", "rnw")) {
    units <- tryCatch(parse_chunk_units(lines), error = function(e) list())
    starts <- vapply(units, function(u) u$start, numeric(1))
    inside <- which(starts <= cursor)
    if (length(inside)) {
      u <- units[[max(inside)]]
      # Only count it if the cursor is before the chunk's closing fence
      after <- lines[(u$start + 1):min(cursor, length(lines))]
      if (cursor == u$start || !any(grepl("^\\s*`{3,}\\s*$", after[-length(after)]))) {
        chunk <- u$label %||% sprintf("starting at line %d", u$start)
      }
    }
  }

  folder <- if (nzchar(path)) {
    d <- dirname(path)
    home <- path.expand("~")
    if (startsWith(d, home)) paste0("~", substring(d, nchar(home) + 1)) else d
  } else NULL

  list(name = name, folder = folder, kind = kind, lines = n_lines,
       cursor_line = cursor, chunk = chunk)
}
