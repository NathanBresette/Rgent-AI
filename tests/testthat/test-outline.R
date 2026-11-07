rmd_lines <- function() {
  body <- c("```{r pressure, echo=FALSE}", sprintf("x%d <- %d", 1:60, 1:60), "```")
  c("---", "title: t", "---", "", "## Intro", "", "```{r setup}", "library(stats)", "```", "",
    "## Plots", "", body, "", "Closing text.", sprintf("line %d", 1:20))
}

test_that("long R Markdown gets an outline with chunk ranges; short files don't", {
  l <- rmd_lines()
  o <- document_outline(l, "Untitled document", min_lines = 80L)
  expect_match(o, "1-3 YAML header")
  expect_match(o, "7-9 r chunk `setup`")
  expect_match(o, "13-74 r chunk `pressure` \\{pressure, echo=FALSE\\}")
  expect_match(o, "11 ## Plots")
  expect_equal(document_outline(l, "a.Rmd"), "")  # under 150 lines: the backend sends it whole
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
