# Edits Claude proposes to the user's open file (```edit blocks).
#
# Claude sees the file as numbered lines (backend `_format_file_context`), so
# an edit only has to name a line range and the new code: no old text comes
# back from the model. The chat page shows each edit as a diff the user
# accepts or rejects (even with Auto Execute on), and an accepted edit can be
# undone from the chat.
#
# 1. `remember_edit_target()` stores the document as Claude saw it when the
#    request was sent, so line numbers refer to that version.
# 2. `preview_file_edits()` turns line ranges into old/new lines from it.
# 3. `apply_file_edit()` finds the old lines in the document as it is now
#    (the one nearest the original position, so earlier edits that shifted
#    lines don't matter) and replaces them. Undo is the same call with old and
#    new swapped. If the lines were changed since, nothing is touched.

.rgent_edits <- new.env(parent = emptyenv())

#' Instructions appended to a chat request when a file is open
#' @keywords internal
edit_instructions <- function(file_name, outline = "", auto_run = FALSE, run_edits = TRUE) {
  # Auto-run applies edits by itself (and runs their code with /runedits on), so
  # Claude mustn't say "once you accept" or repeat the code in a ```r block
  # (it ran twice: once from the edit, once from the ```r block)
  review <- if (isTRUE(auto_run)) paste0(
    "Auto-run is on: your edits are applied to the file as soon as your reply ends",
    if (!isFALSE(run_edits)) " and their R code runs" else "",
    ", so describe them as done (not \"once you accept\"), and never repeat an edit's code in a ```r block. ",
    "The user can Undo an edit on its card (point them there to revert). "
  ) else "The user accepts each edit and can Undo it on its card (point them there to revert). "
  paste0(
    "\n\n(To change the open file", if (nzchar(file_name)) paste0(" ", file_name) else "",
    ", use ```edit blocks, not ```r: ```edit 12-15 replaces lines 12-15 (empty block = delete); ",
    "```edit after 12 inserts after line 12 (0 = top). Line numbers from the CURRENT FILE listing",
    " or OUTLINE; one block per change. If the new lines contain ``` fences, wrap the edit in ````. ",
    review,
    "Edit when asked to change the file, or when the user asks why something in it is wrong: explain briefly ",
    "and include the fix as an edit in the same reply (they can reject it). ```r is for code to run now.)",
    outline
  )
}

# R Markdown / Quarto: by extension, or for unsaved documents (no
# extension) by having chunk fences such as ```{r}
.rgent_is_markdown <- function(name, lines) {
  if (grepl("\\.(R?md|qmd)$", name, ignore.case = TRUE)) return(TRUE)
  if (grepl("\\.[A-Za-z]+$", name)) return(FALSE)
  any(grepl("^\\s*`{3,}\\s*\\{\\s*[A-Za-z]", lines))
}

# All fenced chunks in R Markdown / Quarto: fence line, closing line (NA if
# never closed), engine and label
.rgent_chunks <- function(lines) {
  open_rx <- "^\\s*(`{3,})\\s*\\{\\s*([A-Za-z0-9_]+)[ ,]*([^}]*)\\}\\s*$"
  out <- list()
  n <- length(lines)
  i <- 1L
  while (i <= n) {
    m <- regmatches(lines[i], regexec(open_rx, lines[i], perl = TRUE))[[1]]
    if (length(m) >= 3) {
      close_rx <- paste0("^\\s*", m[2], "`*\\s*$")
      j <- i + 1L
      while (j <= n && !grepl(close_rx, lines[j])) j <- j + 1L
      opts <- trimws(m[4])
      label <- sub("^([^,= ]+)(\\s*,.*)?$", "\\1", opts)
      if (!nzchar(opts) || grepl("=", label, fixed = TRUE)) label <- ""
      out[[length(out) + 1L]] <- list(open = i, close = if (j <= n) j else NA_integer_,
                                      engine = m[3], label = label, options = opts)
      i <- if (j <= n) j + 1L else n + 1L
    } else {
      i <- i + 1L
    }
  }
  out
}

#' A short map of a long document, so Claude knows where every chunk and
#' section starts and ends even when the listing shows only part of the file
#' @keywords internal
# Mirrors the backend's _format_file_context(): it sends the whole file when the
# numbered listing fits FILE_CONTEXT_WHOLE_CHARS (lines cut at 300 characters,
# plus 8 for the "  123| " prefix). Keep the numbers in sync with backend/main.py.
.rgent_listing_chars <- function(lines) sum(pmin(nchar(lines, type = "chars", allowNA = TRUE, keepNA = FALSE), 300L) + 8L)
.rgent_listing_fits <- function(lines, budget = 30000L) .rgent_listing_chars(lines) <= budget

# Numbered lines a-b of the document as Claude saw it, for a ```read request.
# Capped at `budget` characters so one request can't blow up the prompt.
.rgent_read_lines <- function(lines, from, to, budget = 14000L) {
  n <- length(lines)
  from <- max(1L, as.integer(from)); to <- min(n, as.integer(to))
  if (is.na(from) || is.na(to) || from > to) return(NULL)
  cost <- cumsum(pmin(nchar(lines[from:to], allowNA = TRUE, keepNA = FALSE), 300L) + 8L)
  to <- from + max(1L, sum(cost <= budget)) - 1L
  txt <- lines[from:to]
  long <- nchar(txt, allowNA = TRUE, keepNA = FALSE) > 300L
  txt[long] <- paste0(substr(txt[long], 1L, 300L), " \u2026")
  list(from = from, to = to, text = paste(sprintf("%5d| %s", from:to, txt), collapse = "\n"))
}

# ```read a-b at the end of a reply: Claude asking for lines it wasn't shown
.rgent_read_request <- function(reply) {
  m <- regmatches(reply, regexec("(?m)^ {0,3}```+ *read +(\\d+) *(?:-|to) *(\\d+) *\n(?:[^`]*\n)?```+ *$", reply, perl = TRUE))[[1]]
  if (length(m) < 3) return(NULL)
  list(from = as.integer(m[2]), to = as.integer(m[3]))
}

# ```find <pattern> at the end of a reply: Claude searching the open file
.rgent_find_pattern_re <- "(?m)^ {0,3}```+ *find +(.+?) *\n(?:[^`]*\n)?```+ *$"
.rgent_find_request <- function(reply) {
  m <- regmatches(reply, regexec(.rgent_find_pattern_re, reply, perl = TRUE))[[1]]
  if (length(m) < 2) return(NULL)
  p <- trimws(m[2])
  # "ggplot", `ggplot` and /ggplot/ all mean the same search
  p <- sub("^([\"`'/])(.*)\\1$", "\\2", p)
  if (!nzchar(p)) return(NULL)
  list(pattern = p)
}

# The last lookup Claude asked for in this text (a read or a find), or NULL
.rgent_lookup_request <- function(reply) {
  r <- .rgent_read_request(reply); f <- .rgent_find_request(reply)
  last_at <- function(re) { g <- gregexpr(re, reply, perl = TRUE)[[1]]; if (g[1] < 0) -1L else max(g) }
  if (!is.null(r) && !is.null(f)) {
    if (last_at(.rgent_find_pattern_re) > last_at("(?m)^ {0,3}```+ *read +\\d")) r <- NULL else f <- NULL
  }
  if (!is.null(r)) return(c(list(type = "read"), r))
  if (!is.null(f)) return(c(list(type = "find"), f))
  NULL
}

# Lines of the file matching a pattern (a regular expression, ignoring case;
# taken literally if it isn't a valid one), numbered like the listing and
# capped so a broad search stays small
.rgent_find_lines <- function(lines, pattern, max_matches = 60L, budget = 6000L) {
  hit <- tryCatch(suppressWarnings(grepl(pattern, lines, perl = TRUE, ignore.case = TRUE)),
                  error = function(e) grepl(tolower(pattern), tolower(lines), fixed = TRUE))
  idx <- which(hit)
  if (!length(idx)) return(list(pattern = pattern, count = 0L, text = "(no matching lines)"))
  txt <- lines[idx]
  long <- nchar(txt, allowNA = TRUE, keepNA = FALSE) > 200L
  txt[long] <- paste0(substr(txt[long], 1L, 200L), " …")
  rows <- sprintf("%5d| %s", idx, txt)
  keep <- seq_along(rows) <= max_matches & cumsum(nchar(rows) + 1L) <= budget
  keep[1] <- TRUE
  out <- paste(rows[keep], collapse = "\n")
  if (any(!keep)) out <- paste0(out, "\n(", sum(!keep), " more matches not shown; search for something narrower)")
  list(pattern = pattern, count = length(idx), text = out)
}

# In the saved history a read or find request becomes a short note, so later
# requests don't carry stale requests (or a block that looks like an option)
.rgent_strip_read <- function(reply) {
  reply <- gsub("(?m)^ {0,3}```+ *read +(\\d+) *(?:-|to) *(\\d+) *\n(?:[^`]*\n)?```+ *$", "(read lines \\1-\\2 of the file)", reply, perl = TRUE)
  gsub(.rgent_find_pattern_re, "(searched the file for \\1)", reply, perl = TRUE)
}

document_outline <- function(lines, name, budget = 30000L, max_items = 80L) {
  n <- length(lines)
  if (.rgent_listing_fits(lines, budget)) return("")
  items <- character(0)
  if (.rgent_is_markdown(name, lines)) {
    if (n > 1 && grepl("^---\\s*$", lines[1])) {
      end <- which(grepl("^(---|\\.\\.\\.)\\s*$", lines[-1]))[1] + 1L
      if (!is.na(end)) items <- c(items, sprintf("%d-%d YAML header", 1L, end))
    }
    chunks <- .rgent_chunks(lines)
    inside <- rep(FALSE, n)
    for (ch in chunks) {
      last <- if (is.na(ch$close)) n else ch$close
      inside[ch$open:last] <- TRUE
      what <- sprintf("%s chunk%s%s", ch$engine,
                      if (nzchar(ch$label)) paste0(" `", ch$label, "`") else "",
                      if (nzchar(ch$options) && !identical(ch$options, ch$label)) paste0(" {", ch$options, "}") else "")
      items <- c(items, if (is.na(ch$close)) sprintf("%d-%d %s (NEVER CLOSED)", ch$open, n, what)
                        else sprintf("%d-%d %s", ch$open, ch$close, what))
    }
    heads <- which(!inside & grepl("^#{1,6} \\S", lines))
    hl <- sprintf("%d %s", heads, substr(lines[heads], 1, 80))
    items <- c(items, hl)
    items <- items[order(as.integer(sub("^(\\d+).*", "\\1", items)))]
  } else {
    # R script: top-level expressions that define things, and section comments
    exprs <- tryCatch(parse(text = lines, keep.source = TRUE), error = function(e) NULL)
    if (!is.null(exprs)) {
      sr <- attr(exprs, "srcref")
      for (k in seq_along(exprs)) {
        e <- exprs[[k]]
        a <- sr[[k]][1]; b <- sr[[k]][3]
        if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=", "<<-") && b > a) {
          lhs <- paste(deparse(e[[2]]), collapse = "")
          kind <- if (is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) "function" else "assignment"
          items <- c(items, sprintf("%d-%d %s %s", a, b, kind, substr(lhs, 1, 60)))
        } else if (b - a >= 4) {
          items <- c(items, sprintf("%d-%d %s", a, b, substr(gsub("\\s+", " ", lines[a]), 1, 60)))
        }
      }
    }
    secs <- grep("^#.*(-{4,}|={4,}|#{4,})\\s*$", lines)
    items <- c(items, sprintf("%d %s", secs, substr(trimws(lines[secs]), 1, 80)))
    items <- items[order(as.integer(sub("^(\\d+).*", "\\1", items)))]
  }
  more <- length(items) > max_items
  items <- utils::head(items, max_items)
  paste0("\n\nOUTLINE of ", name, " (", n, " lines; the CURRENT FILE listing may show only part of it):\n",
         if (length(items)) paste0("  ", items, collapse = "\n") else "  (no chunks, sections or definitions found)",
         if (more) "\n  ..." else "",
         "\nTo replace or delete a whole chunk or section, use its range from this outline, ",
         "including lines the listing doesn't show; the user sees every affected line before accepting. ",
         "Never cut a chunk's opening fence without its closing fence or the reverse. ",
         "You can look at the rest of the file: end your reply with an empty ```read 46-151 block (a line range) ",
         "or ```find ggplot|geom_ block (a regular expression, ignoring case; you get every matching line with its number), ",
         "and nothing after it; the result is sent and you continue. For questions about the whole file ",
         "(\"which plots do I have\", \"where is x defined\"), search or read first; never tell the user you can't see part of the file.")
}

.rgent_inside_chunk <- function(lines) {
  inside <- rep(FALSE, length(lines))
  for (ch in .rgent_chunks(lines)) {
    last <- if (is.na(ch$close)) length(lines) else ch$close
    inside[ch$open:last] <- TRUE
  }
  inside
}

# Would this edit move lines it doesn't touch into or out of a chunk? (For
# example, replacing a chunk's first half leaves its second half as text.)
# Compares the document before and after, so it doesn't matter what fences
# the new code has.
.rgent_chunk_warning <- function(target, start, end, new_code) {
  if (!.rgent_is_markdown(target$name, target$lines)) return(NULL)
  lines <- target$lines
  n <- length(lines)
  after <- c(lines[seq_len(start - 1L)], new_code, if (end < n) lines[(end + 1L):n])
  was <- .rgent_inside_chunk(lines)
  now <- .rgent_inside_chunk(after)
  kept_old <- c(seq_len(start - 1L), if (end < n) (end + 1L):n)
  kept_new <- c(seq_len(start - 1L), if (end < n) (end + 1L):n + (length(new_code) - (end - start + 1L)))
  flip <- kept_old[was[kept_old] != now[kept_new]]
  if (!length(flip)) return(NULL)
  # Report the first run of affected lines (in the file as it is now)
  run <- flip[c(TRUE, diff(flip) == 1L) & cumsum(c(TRUE, diff(flip) != 1L)) == 1L]
  a <- min(run); b <- max(run)
  where <- if (a == b) sprintf("Line %d", a) else sprintf("Lines %d-%d", a, b)
  if (was[a]) sprintf("%s would no longer be inside a code chunk (they'd knit as text). Check the edit covers the whole chunk.", where)
  else sprintf("%s would end up inside a code chunk (text would run as code). Check the edit's chunk fences.", where)
}

# What the user did with Claude's edits since its last reply (accepted,
# rejected, undone), so Claude doesn't rely on an out-of-date picture.
.rgent_edit_note <- function(text) {
  .rgent_edits$log <- c(.rgent_edits$log, text)
  invisible(NULL)
}

#' Note for Claude about edits handled since its last reply; clears the log
#' @keywords internal
edit_history_note <- function() {
  log <- .rgent_edits$log
  .rgent_edits$log <- NULL
  if (!length(log)) return("")
  paste0("\n\n(Since your last reply the user ", paste(utils::tail(log, 8), collapse = "; "),
         ". The CURRENT FILE listing shows the file as it is now.)")
}

#' Remember the open document as Claude will see it
#' @keywords internal
remember_edit_target <- function(doc = NULL) {
  if (is.null(doc)) {
    doc <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  }
  if (is.null(doc) || is.null(doc$contents)) {
    .rgent_edits$target <- NULL
    return(invisible(NULL))
  }
  path <- if (is.null(doc$path)) "" else doc$path
  .rgent_edits$target <- list(
    id = if (is.null(doc$id)) "" else doc$id,
    path = path,
    name = if (nzchar(path)) basename(path) else "Untitled document",
    lines = as.character(doc$contents)
  )
  invisible(.rgent_edits$target)
}

# Line ranges of R chunk bodies in an R Markdown / Quarto document
.rgent_chunk_bodies <- function(lines) {
  start_rx <- "^\\s*(`{3,})\\s*\\{\\s*[rR](?![A-Za-z0-9_])(.*)\\}\\s*$"
  out <- list()
  n <- length(lines)
  i <- 1
  while (i <= n) {
    m <- regmatches(lines[i], regexec(start_rx, lines[i], perl = TRUE))[[1]]
    if (length(m) >= 3) {
      j <- i + 1
      while (j <= n && !grepl(paste0("^\\s*", m[2], "`*\\s*$"), lines[j])) j <- j + 1
      out[[length(out) + 1]] <- c(fence = i, first = i + 1, last = j - 1)
      i <- j + 1
    } else {
      i <- i + 1
    }
  }
  out
}

# The document as it will be after the edit, and where the new lines land
.rgent_edit_after <- function(target, mode, start, end, new_code) {
  lines <- target$lines
  n <- length(lines)
  if (mode == "replace") {
    after <- c(lines[seq_len(start - 1)], new_code, if (end < n) lines[(end + 1):n])
    es <- start
  } else {
    after <- c(lines[seq_len(start)], new_code, if (start < n) lines[(start + 1):n])
    es <- start + 1L
  }
  list(lines = after, es = es, ee = es + length(new_code) - 1L)
}

# Complete R chunks (after the edit) whose opening fence is one of the new
# lines, leaving out chunks turned off with eval = FALSE. An edit that only
# moves a closing fence (and so breaks a chunk) finds none.
.rgent_edit_chunks_hit <- function(after) {
  hit <- list()
  n <- length(after$lines)
  for (b in .rgent_chunk_bodies(after$lines)) {
    if (b[["fence"]] >= after$es && b[["fence"]] <= after$ee && b[["last"]] < n &&
        !grepl("eval\\s*=\\s*(F|FALSE)\\b", after$lines[b[["fence"]]])) {
      hit[[length(hit) + 1]] <- b
    }
  }
  hit
}

# Can the new code be run as R? Always in a script. In R Markdown / Quarto
# when the edit sits inside an R chunk, or when it adds or rewrites whole
# chunks (fences included): then those chunks run. Before 2.4.5 an edit that
# inserted a new chunk, or replaced one fence and all, was never runnable, so
# "Accept & run" didn't appear for the most common edit.
.rgent_edit_runnable <- function(target, mode, start, end, new_code = NULL) {
  if (!.rgent_is_markdown(target$name, target$lines)) return(TRUE)
  for (b in .rgent_chunk_bodies(target$lines)) {
    if (mode == "replace" && start >= b[["first"]] && end <= b[["last"]]) return(TRUE)
    if (mode == "after" && start >= b[["fence"]] && start <= b[["last"]]) return(TRUE)
  }
  if (length(new_code) == 0) return(FALSE)
  length(.rgent_edit_chunks_hit(.rgent_edit_after(target, mode, start, end, new_code))) > 0
}

# What "Accept & run" runs: the whole statements the edit touches, not just
# the new lines. A changed line in the middle of a pipeline
# (`summarise(...)` on its own) fails when run alone. In R Markdown / Quarto
# it also runs the rest of that chunk, as the chunk's later lines usually
# use what the edit changed (and print the result); an edit that adds or
# rewrites whole chunks runs those chunks. Works on the document as it will
# be after the edit; falls back to the new lines when that part doesn't parse.
.rgent_edit_run_code <- function(target, mode, start, end, new_code) {
  if (length(new_code) == 0) return("")
  fallback <- paste(new_code, collapse = "\n")
  a <- .rgent_edit_after(target, mode, start, end, new_code)
  after <- a$lines; es <- a$es; ee <- a$ee
  md <- .rgent_is_markdown(target$name, target$lines)
  first <- 1L; last <- length(after)
  if (md) {
    hit <- NULL
    for (b in .rgent_chunk_bodies(after)) {
      if (es >= b[["first"]] && ee <= b[["last"]]) { hit <- b; break }
    }
    if (is.null(hit)) {
      # Whole chunks added or rewritten: run their bodies, in order
      chunks <- .rgent_edit_chunks_hit(a)
      if (length(chunks) == 0) return(fallback)
      body <- unlist(lapply(chunks, function(b) if (b[["last"]] >= b[["first"]]) after[b[["first"]]:b[["last"]]]))
      return(paste(body, collapse = "\n"))
    }
    first <- hit[["first"]]; last <- hit[["last"]]
  }
  region <- after[first:last]
  parsed <- tryCatch(parse(text = region, keep.source = TRUE), error = function(e) NULL)
  refs <- if (is.null(parsed)) NULL else attr(parsed, "srcref")
  if (length(refs) == 0) return(fallback)
  off <- first - 1L
  starts <- vapply(refs, function(r) r[[1]], integer(1)) + off
  ends <- vapply(refs, function(r) r[[3]], integer(1)) + off
  touched <- which(ends >= es & starts <= ee)
  if (length(touched) == 0) return(fallback)
  from <- starts[min(touched)]
  to <- if (md) ends[length(ends)] else ends[max(touched)]
  paste(after[from:to], collapse = "\n")
}

.rgent_as_lines <- function(x) {
  if (is.null(x)) return(character(0))
  as.character(unlist(x, use.names = FALSE))
}

#' Turn ```edit blocks into old/new lines for review
#'
#' @param edits list of list(mode = "replace"|"after", start, end, code)
#' @keywords internal
preview_file_edits <- function(edits) {
  target <- .rgent_edits$target
  lapply(edits, function(e) {
    fail <- function(msg) list(ok = FALSE, error = msg)
    if (is.null(target)) return(fail("No open file to edit."))
    lines <- target$lines
    n <- length(lines)
    mode <- if (identical(e$mode, "after")) "after" else "replace"
    start <- suppressWarnings(as.integer(e$start))
    end <- suppressWarnings(as.integer(if (is.null(e$end)) e$start else e$end))
    code <- if (is.null(e$code)) "" else paste(.rgent_as_lines(e$code), collapse = "\n")
    code <- sub("\n$", "", code)
    new_code <- if (nzchar(code)) strsplit(code, "\n", fixed = TRUE)[[1]] else character(0)
    # Newer pages send the block's exact lines: a block holding one blank line
    # inserts a blank line (`code` alone can't tell it from an empty block).
    # A replace made only of blank lines still means delete.
    if (!is.null(e$lines)) {
      exact <- .rgent_as_lines(e$lines)
      new_code <- if (mode == "replace" && !any(nzchar(trimws(exact)))) character(0) else exact
    }

    if (is.na(start)) return(fail("The edit has no line number."))
    if (mode == "replace") {
      if (is.na(end) || start < 1 || end < start || end > n) {
        return(fail(sprintf("Lines %s-%s aren't in %s (it has %d lines).", start, end, target$name, n)))
      }
      old <- lines[start:end]
      new <- new_code
      at <- start
      # A deletion leaves nothing to find when undoing it, so keep a
      # neighbouring line in both versions as an anchor
      if (length(new_code) == 0) {
        if (start > 1) {
          old <- lines[(start - 1):end]
          new <- lines[start - 1]
          at <- start - 1L
        } else if (end < n) {
          old <- lines[start:(end + 1)]
          new <- lines[end + 1]
        }
      }
    } else {
      if (start < 0 || start > n) {
        return(fail(sprintf("Line %d isn't in %s (it has %d lines).", start, target$name, n)))
      }
      if (length(new_code) == 0) return(fail("The edit inserts nothing."))
      # Inserting is a replace of the neighbouring line, so it can be found
      # again later even if the file shifted.
      if (start >= 1) {
        old <- lines[start]
        new <- c(lines[start], new_code)
        at <- start
      } else if (n >= 1) {
        old <- lines[1]
        new <- c(new_code, lines[1])
        at <- 1L
      } else {
        old <- character(0)
        new <- new_code
        at <- 1L
      }
      end <- start
    }
    if (identical(old, new)) return(fail("The edit doesn't change anything."))
    warning <- if (mode == "replace") .rgent_chunk_warning(target, start, end, new_code) else NULL
    list(
      warning = warning,
      ok = TRUE,
      file = target$name, path = target$path, id = target$id,
      mode = mode, start = start, end = end, at = at,
      old = as.list(old), new = as.list(new),
      code = paste(new_code, collapse = "\n"),
      # an edit that breaks a chunk (warning) is never run by Accept
      runnable = is.null(warning) && length(new_code) > 0 && .rgent_edit_runnable(target, mode, start, end, new_code),
      run_code = tryCatch(.rgent_edit_run_code(target, mode, start, end, new_code),
                          error = function(err) paste(new_code, collapse = "\n"))
    )
  })
}

# Find the open document: by id, then by path (opening it if it was closed)
.rgent_find_document <- function(id, path) {
  same <- function(doc) {
    dp <- if (is.null(doc$path)) "" else as.character(doc$path)
    !is.null(doc) && ((nzchar(id) && identical(doc$id, id)) ||
                      (nzchar(path) && nzchar(dp) &&
                         normalizePath(dp, mustWork = FALSE) == normalizePath(path, mustWork = FALSE)))
  }
  if (nzchar(id)) {
    doc <- tryCatch(rstudioapi::getSourceEditorContext(id), error = function(e) NULL)
    if (same(doc)) return(doc)
  }
  doc <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (same(doc)) return(doc)
  if (nzchar(path) && file.exists(path)) {
    tryCatch(rstudioapi::documentOpen(path), error = function(e) NULL)
    doc <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (same(doc)) return(doc)
  }
  NULL
}

# Where `old` occurs in `lines`, nearest to line `at`; NA if nowhere
.rgent_locate_lines <- function(lines, old, at) {
  k <- length(old)
  n <- length(lines)
  if (k == 0) return(if (n <= 1 && all(!nzchar(lines))) 1L else NA_integer_)
  if (k > n) return(NA_integer_)
  hits <- Filter(function(p) identical(lines[p:(p + k - 1)], old), seq_len(n - k + 1))
  if (length(hits) == 0) return(NA_integer_)
  hits <- unlist(hits)
  hits[which.min(abs(hits - at))]
}

#' Replace `old` lines with `new` lines in an open document
#'
#' Used both to apply an accepted edit and to undo it (old and new swapped).
#' @keywords internal
apply_file_edit <- function(id = "", path = "", at = 1L, old = list(), new = list()) {
  old <- .rgent_as_lines(old)
  new <- .rgent_as_lines(new)
  id <- if (is.null(id)) "" else as.character(id)
  path <- if (is.null(path)) "" else as.character(path)
  at <- suppressWarnings(as.integer(at))
  if (is.na(at)) at <- 1L

  doc <- .rgent_find_document(id, path)
  if (is.null(doc)) {
    return(list(ok = FALSE, error = "That file isn't open in RStudio any more."))
  }
  lines <- as.character(doc$contents)
  p <- .rgent_locate_lines(lines, old, at)
  if (is.na(p)) {
    return(list(ok = FALSE,
                error = "Those lines changed since, so nothing was touched. Edit the file by hand or ask again."))
  }
  n <- length(lines)
  q <- p + length(old) - 1
  pos <- rstudioapi::document_position
  # Column past the end of a line; RStudio clips it to the line end
  eol <- function(row) nchar(lines[row], type = "bytes") + 1

  if (length(old) == 0) {
    range <- rstudioapi::document_range(pos(1, 1), pos(1, eol(1)))
    text <- paste(new, collapse = "\n")
  } else if (q < n) {
    range <- rstudioapi::document_range(pos(p, 1), pos(q + 1, 1))
    text <- if (length(new)) paste0(paste(new, collapse = "\n"), "\n") else ""
  } else if (length(new)) {
    range <- rstudioapi::document_range(pos(p, 1), pos(n, eol(n)))
    text <- paste(new, collapse = "\n")
  } else if (p > 1) {
    # Deleting the last lines: also remove the newline before them
    range <- rstudioapi::document_range(pos(p - 1, eol(p - 1)), pos(n, eol(n)))
    text <- ""
  } else {
    range <- rstudioapi::document_range(pos(1, 1), pos(n, eol(n)))
    text <- ""
  }

  res <- tryCatch({
    rstudioapi::modifyRange(range, text, id = doc$id)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (!isTRUE(res)) return(list(ok = FALSE, error = paste("RStudio couldn't edit the file:", res)))
  tryCatch(rstudioapi::setCursorPosition(pos(p, 1), id = doc$id), error = function(e) NULL)

  list(ok = TRUE, at = p, id = doc$id,
       path = if (is.null(doc$path)) "" else doc$path)
}
