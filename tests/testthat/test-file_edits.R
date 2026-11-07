# Edits Claude proposes to the open file: preview, apply, undo against a
# fake RStudio editor (modifyRange clips columns past the line end, as RStudio does).

fake_editor <- function(lines, path = "", id = "d1") {
  st <- new.env()
  st$docs <- list()
  st$docs[[id]] <- list(id = id, path = path, contents = lines)
  st$active <- id
  ctx <- function(id = NULL) {
    if (is.null(id)) id <- st$active
    if (is.null(id)) NULL else st$docs[[id]]
  }
  modify <- function(location = NULL, text = NULL, id = NULL) {
    lines <- st$docs[[id]]$contents
    off <- function(p) {
      row <- min(p[["row"]], length(lines))
      col <- min(p[["column"]], nchar(lines[row]) + 1)
      sum(nchar(lines[seq_len(row - 1)]) + 1) + col - 1
    }
    full <- paste(lines, collapse = "\n")
    a <- off(location$start); b <- off(location$end)
    full <- paste0(substr(full, 1, a), text, substr(full, b + 1, nchar(full)))
    out <- strsplit(full, "\n", fixed = TRUE)[[1]]
    if (endsWith(full, "\n")) out <- c(out, "")
    if (!nzchar(full)) out <- ""
    st$docs[[id]]$contents <- out
    invisible(NULL)
  }
  st$mocks <- list(getSourceEditorContext = ctx, modifyRange = modify,
                   setCursorPosition = function(position, id = NULL) NULL)
  st
}

with_editor <- function(st, code) {
  api <- asNamespace("rstudioapi")
  saved <- lapply(names(st$mocks), function(n) get(n, envir = api))
  names(saved) <- names(st$mocks)
  for (n in names(st$mocks)) { unlockBinding(n, api); assign(n, st$mocks[[n]], envir = api) }
  on.exit(for (n in names(saved)) assign(n, saved[[n]], envir = api), add = TRUE)
  force(code)
}

# JSON round trip, as between R and the chat page
rt <- function(x) jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE), simplifyVector = FALSE)
apply_edit <- function(p, undo = FALSE, at = NULL) {
  rt(rstudioai:::apply_file_edit(id = p$id, path = p$path, at = if (is.null(at)) p$at else at,
                                 old = if (undo) p$new else p$old, new = if (undo) p$old else p$new))
}
base <- c("library(stats)", "x <- 1", "y <- 2", "z <- x + y", "print(z)")

test_that("replace, insert and delete apply and undo exactly", {
  cases <- list(
    list(e = list(mode = "replace", start = 2, end = 3, code = "x <- 10\ny <- 20"),
         want = c("library(stats)", "x <- 10", "y <- 20", "z <- x + y", "print(z)")),
    list(e = list(mode = "after", start = 0, code = "# top"), want = c("# top", base)),
    list(e = list(mode = "after", start = 5, code = "str(z)"), want = c(base, "str(z)")),
    list(e = list(mode = "replace", start = 2, end = 3, code = ""), want = base[c(1, 4, 5)]),
    list(e = list(mode = "replace", start = 4, end = 5, code = ""), want = base[1:3]),
    list(e = list(mode = "replace", start = 1, end = 5, code = ""), want = "")
  )
  for (cs in cases) {
    st <- fake_editor(base)
    with_editor(st, {
      rstudioai:::remember_edit_target()
      p <- rt(rstudioai:::preview_file_edits(rt(list(cs$e))))[[1]]
      expect_true(p$ok)
      a <- apply_edit(p)
      expect_true(a$ok)
      expect_identical(st$docs$d1$contents, cs$want)
      u <- apply_edit(p, undo = TRUE, at = a$at)
      expect_true(u$ok)
      expect_identical(st$docs$d1$contents, base)
    })
  }
})

test_that("edits find their lines after earlier edits or typing shifted them", {
  st <- fake_editor(base)
  with_editor(st, {
    rstudioai:::remember_edit_target()
    ps <- rt(rstudioai:::preview_file_edits(rt(list(
      list(mode = "after", start = 1, code = "library(utils)"),
      list(mode = "replace", start = 4, end = 4, code = "z <- x * y")))))
    st$docs$d1$contents <- c("# typed by the user", st$docs$d1$contents)
    expect_true(apply_edit(ps[[1]])$ok)
    expect_true(apply_edit(ps[[2]])$ok)
    expect_identical(st$docs$d1$contents, c("# typed by the user", "library(stats)", "library(utils)",
                                            "x <- 1", "y <- 2", "z <- x * y", "print(z)"))
  })
})

test_that("changed lines are left alone", {
  st <- fake_editor(base)
  with_editor(st, {
    rstudioai:::remember_edit_target()
    p <- rt(rstudioai:::preview_file_edits(rt(list(list(mode = "replace", start = 2, end = 2, code = "x <- 5")))))[[1]]
    st$docs$d1$contents[2] <- "x <- 99"
    before <- st$docs$d1$contents
    a <- apply_edit(p)
    expect_false(a$ok)
    expect_identical(st$docs$d1$contents, before)
  })
})

test_that("bad line numbers are rejected and Rmd prose isn't runnable", {
  rmd <- c("---", "title: t", "---", "Text", "```{r}", "x <- 1", "```", "More")
  st <- fake_editor(rmd, path = file.path(tempdir(), "a.Rmd"))
  with_editor(st, {
    rstudioai:::remember_edit_target()
    r <- rt(rstudioai:::preview_file_edits(rt(list(
      list(mode = "replace", start = 6, end = 6, code = "x <- 2"),
      list(mode = "replace", start = 4, end = 4, code = "Other"),
      list(mode = "replace", start = 7, end = 20, code = "x")))))
    expect_true(r[[1]]$runnable)
    expect_false(r[[2]]$runnable)
    expect_false(r[[3]]$ok)
  })
})

test_that("the prompt explains the edit format", {
  ins <- rstudioai:::edit_instructions("analysis.Rmd")
  expect_match(ins, "```edit 12-15", fixed = TRUE)
  expect_match(ins, "analysis.Rmd", fixed = TRUE)
})
