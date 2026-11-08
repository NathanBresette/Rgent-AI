test_that("auto-run asks first for code that installs, deletes, runs shell commands or quits", {
  chk <- rstudioai:::.rgent_auto_run_check
  expect_equal(chk(list(auto = TRUE, code = "install.packages('x')")), "installs packages")
  expect_equal(chk(list(auto = TRUE, code = "x <- 1\nunlink('data', recursive = TRUE)")), "deletes files")
  expect_equal(chk(list(auto = TRUE, code = "rm(list = ls())")), "clears the environment")
  expect_equal(chk(list(auto = TRUE, code = "system('ls')")), "runs shell commands")
  expect_equal(chk(list(auto = TRUE, code = "fs::file_delete('a.csv')")), "deletes or moves files")
  expect_null(chk(list(auto = TRUE, code = "summary(lm(mpg ~ wt, mtcars))")))
  expect_null(chk(list(auto = TRUE, code = "# unlink('x') is only a comment\nx <- 1")))
  # A click on Run (no `auto`), or a resumed run, is the user's OK
  expect_null(chk(list(code = "install.packages('x')")))
  expect_null(chk(list(auto = TRUE, code = "install.packages('x')", resume = list(start_at = 2))))
})

test_that("Rgent's code shows in the Console like typed input", {
  echo <- rstudioai:::.rgent_console_echo
  expect_equal(echo("x <- 1\nsales %>%\n  head()\n"), c("# Rgent ran:", "> x <- 1", "> sales %>%", "+   head()"))
  expect_equal(echo("if (TRUE) {"), c("# Rgent ran:", "> if (TRUE) {"))   # doesn't parse: every line is a statement
  expect_length(echo("  \n"), 0)
})
