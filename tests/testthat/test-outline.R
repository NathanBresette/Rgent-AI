rmd_lines <- function() {
  body <- c("```{r pressure, echo=FALSE}", sprintf("x%d <- %d", 1:60, 1:60), "```")
  c("---", "title: t", "---", "", "## Intro", "", "```{r setup}", "library(stats)", "```", "",
    "## Plots", "", body, "", "Closing text.", sprintf("line %d", 1:20))
}

test_that("long R Markdown gets an outline with chunk ranges; short files don't", {
  l <- rmd_lines()
  o <- document_outline(l, "Untitled document", budget = 0L)
  expect_match(o, "1-3 YAML header")
  expect_match(o, "7-9 r chunk `setup`")
  expect_match(o, "13-74 r chunk `pressure` \\{pressure, echo=FALSE\\}")
  expect_match(o, "11 ## Plots")
  expect_equal(document_outline(l, "a.Rmd"), "")  # fits the size budget: the backend sends it whole
})

test_that("an edit that leaves half a chunk behind is flagged", {
  l <- rmd_lines()
  remember_edit_target(list(id = "d", path = "", contents = l))
  cut <- preview_file_edits(list(list(mode = "replace", start = 11, end = 30,
                                      code = "## Tests\n\n```{r tests}\nt.test(1:10)\n```")))[[1]]
  expect_match(cut$warning, "^Lines 31-74 would no longer be inside a code chunk")
  whole <- preview_file_edits(list(list(mode = "replace", start = 11, end = 74,
                                        code = "## Tests\n\n```{r tests}\nt.test(1:10)\n```")))[[1]]
  expect_null(whole$warning)
  prose <- preview_file_edits(list(list(mode = "replace", start = 76, end = 76, code = "New text.")))[[1]]
  expect_false(prose$runnable)
})

test_that("the outline follows the backend's size budget, not a line count", {
  short_lines <- sprintf("x%d <- %d", 1:400, 1:400)            # 400 short lines fit
  expect_true(.rgent_listing_fits(short_lines))
  expect_equal(document_outline(short_lines, "a.R"), "")
  wide <- rep(strrep("y", 120), 160)                            # 160 wide lines don't
  expect_false(.rgent_listing_fits(wide))
  big <- c(rmd_lines(), rep(paste("Some prose", strrep("z", 100)), 200))
  o <- document_outline(big, "big.Rmd")
  expect_match(o, "13-74 r chunk")
  expect_match(o, "```read 46-151", fixed = TRUE)
})

test_that("Claude can ask for lines it wasn't shown", {
  expect_equal(.rgent_read_request("Let me check.\n\n```read 46-151\n```"), list(from = 46L, to = 151L))
  expect_equal(.rgent_read_request("ok\n````read 3 to 9\n````\n"), list(from = 3L, to = 9L))
  expect_null(.rgent_read_request("```r\nread 1-2\n```"))
  expect_null(.rgent_read_request("no request here"))
  l <- sprintf("line %d", 1:200)
  got <- .rgent_read_lines(l, 190, 500)
  expect_equal(c(got$from, got$to), c(190L, 200L))
  expect_match(got$text, "^  190\\| line 190")
  capped <- .rgent_read_lines(rep(strrep("w", 290), 200), 1, 200)
  expect_lt(capped$to, 200L)                                    # capped at the budget
  expect_null(.rgent_read_lines(l, 300, 400))
})

test_that("read requests are kept out of the saved history", {
  expect_equal(.rgent_strip_read("Checking.\n\n```read 46-151\n```\n\nDone."), "Checking.\n\n(read lines 46-151 of the file)\n\nDone.")
})

test_that("an edit can insert a blank line", {
  remember_edit_target(list(id = "d", path = "", contents = c("```{r a}", "x", "```", "```{r b}", "y", "```")))
  p <- preview_file_edits(list(list(mode = "after", start = 3, end = 3, code = "", lines = list(""))))[[1]]
  expect_true(p$ok)
  expect_equal(.rgent_as_lines(p$new), c("```", ""))   # the anchor line, then the blank line
  p <- preview_file_edits(list(list(mode = "after", start = 3, end = 3, code = "")))[[1]]
  expect_false(p$ok)                                      # old pages: still "inserts nothing"
  p <- preview_file_edits(list(list(mode = "after", start = 1, code = "z", lines = list("z", ""))))[[1]]
  expect_equal(tail(.rgent_as_lines(p$new), 2), c("z", ""))
  p <- preview_file_edits(list(list(mode = "replace", start = 2, end = 2, code = "", lines = list(""))))[[1]]
  expect_equal(.rgent_as_lines(p$old), c("```{r a}", "x"))   # blank replace = delete, as before
  expect_equal(.rgent_as_lines(p$new), "```{r a}")            # (the line above stays as the undo anchor)
})

test_that("```find searches the file and stays small", {
  l <- c("```{r a}", "library(ggplot2)", "p <- ggplot(df, aes(x)) + geom_point()", "```", "text", "```{r b}", "plot(1:3)", "q <- ggplot(df2, aes(y))", "```")
  expect_equal(.rgent_find_request("Let me look.\n\n```find ggplot\n```")$pattern, "ggplot")
  expect_equal(.rgent_find_request("```find \"geom_|ggplot\\(\"\n```")$pattern, "geom_|ggplot\\(")
  expect_null(.rgent_find_request("```r\nfind(x)\n```"))
  f <- .rgent_find_lines(l, "ggplot\\(")
  expect_equal(f$count, 2L)
  expect_match(f$text, "^    3\\| p <- ggplot")
  expect_match(f$text, "    8\\| q <- ggplot")
  expect_equal(.rgent_find_lines(l, "GEOM_POINT")$count, 1L)        # ignores case
  expect_equal(.rgent_find_lines(l, "aes(x")$count, 1L)            # invalid regex: taken literally
  expect_equal(.rgent_find_lines(l, "nothing here")$count, 0L)
  big <- .rgent_find_lines(rep("x <- 1", 500), "x", max_matches = 60L)
  expect_equal(big$count, 500L)
  expect_match(big$text, "440 more matches not shown")
  expect_lt(nchar(big$text), 6200)
  # the last lookup in the text wins; history keeps a note instead of the block
  expect_equal(.rgent_lookup_request("```find ggplot\n```")$type, "find")
  expect_equal(.rgent_lookup_request("```read 3-9\n```")$type, "read")
  expect_null(.rgent_lookup_request("no lookup"))
  expect_equal(.rgent_strip_read("Searching.\n\n```find ggplot\n```"), "Searching.\n\n(searched the file for ggplot)")
})

test_that("big files without chunks still get the read/find option", {
  o <- document_outline(rep("x <- 1", 3000), "data.R")
  expect_match(o, "```find")
  expect_match(o, "no chunks, sections or definitions")
})
