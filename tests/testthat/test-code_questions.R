# A question from code the chat runs shows what the user needs to answer it
# (2.4.21): remotes prints its upgrade menu with cat() and asks with readline().

test_that("a numbered menu printed before readline() reaches the chat whole", {
  # all inside one call, as in remotes::install_github()
  code <- paste(
    'upgrade <- function() {',
    'cat("These packages have more recent versions available.\\nIt is recommended to update all of them.\\nWhich would you like to update?\\n\\n")',
    'cat("1: All\\n2: CRAN packages only\\n3: None\\n4: otel    (0.2.0 -> 0.3.0) [CRAN]\\n5: openssl (2.5.0 -> 2.6.0) [CRAN]\\n\\n")',
    'readline("Enter one or more numbers, or an empty line to skip updates: ")',
    '}', 'x <- upgrade()', sep = "\n")
  r <- rstudioai:::execute_with_recovery(code)
  q <- r$needs_input
  expect_equal(q$kind, "text")
  expect_match(q$prompt, "Enter one or more numbers")
  expect_true(all(c("1: All", "2: CRAN packages only", "3: None") %in% q$context))
  expect_true(any(grepl("^5: openssl", q$context)))
  expect_true("Which would you like to update?" %in% q$context)
  expect_false(any(grepl("These packages", q$context)))   # only the heading right above the list
})

test_that("without a list, only the last few lines are shown", {
  r <- rstudioai:::execute_with_recovery(paste(
    'ask <- function() { for (i in 1:10) cat("progress line", i, "\\n"); readline("Your name: ") }',
    'x <- ask()', sep = "\n"))
  ctx <- r$needs_input$context
  expect_lte(length(ctx), 3)
  expect_true("progress line 10" %in% ctx)
})

test_that("the context rule: a list ending the output is kept, capped, with its heading", {
  f <- rstudioai:::.rgent_question_context
  expect_equal(f(c("a", "b", "c", "d")), c("b", "c", "d"))
  expect_equal(f(c("noise", "Pick one:", "1: x", "2: y")), c("Pick one:", "1: x", "2: y"))
  long <- c("Head", paste0(1:50, ": item"))
  expect_length(f(long), 30)
  expect_equal(f(c("1: a", "2: b", "done", "more", "end")), c("done", "more", "end"))   # an old list further up isn't
  expect_equal(f(character(0)), character(0))
})
