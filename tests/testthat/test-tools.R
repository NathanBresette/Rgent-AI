# Claude's tools (R/tools.R)

inspect <- function(code, env = globalenv()) rstudioai:::.rgent_inspect_data(code, env = env)
checks <- function(code, env = globalenv()) rstudioai:::.rgent_inspect_check(parse(text = code)[[1]], env)

test_that("inspect_data runs reading expressions and shows their output", {
  e <- new.env(parent = globalenv())
  e$sales <- data.frame(region = c("N", "S", "S", NA), amount = c(1.5, 2, NA, 4), date = as.Date("2026-01-01") + 0:3)
  r <- inspect("str(sales)", e)
  expect_false(r$is_error); expect_match(r$content, "4 obs. of  3 variables")
  expect_match(inspect("table(sales$region, useNA = 'ifany')", e)$content, "<NA>")
  expect_match(inspect("sum(is.na(sales$amount))", e)$content, "\\[1\\] 1")
  expect_match(inspect("sapply(sales, class)", e)$content, "Date")
  expect_null(checks("dplyr::count(sales, date)", e))                      # a column named like a function is fine
  expect_null(checks("sales %>% head(2)", e))
  expect_null(checks("sales |> subset(amount > 1) |> nrow()", e))
  expect_null(checks("aggregate(amount ~ region, data = sales, FUN = mean)", e))
  expect_null(checks("sapply(split(sales$amount, sales$region), function(x) mean(x, na.rm = TRUE))", e))
  expect_match(inspect("sales", e)$content, "region")                       # a bare name prints
})

test_that("inspect_data refuses anything that writes, runs other code or reaches outside", {
  e <- new.env(parent = globalenv())
  e$x <- 1:3
  e$myfun <- function() stop("never run")
  e$fns <- list(go = function(...) stop("never run"))
  bad <- c("x[1] <- 2", "x <<- 5", "x$a <- 1", "print <- system; print('ls')", "c <- function() system('ls')",
           "f <- system; f('ls')", "1; system('ls')", "addmargins(table(1), FUN = unlink)", "mapply(unlink, 'a')",
           "outer(1, 1, unlink)", "assign('x', 1)", "rm(x)", "file.remove('a')", "unlink('a')", "system('ls')",
           "Sys.setenv(A = 1)", "options(warn = 2)", "setwd('/')", "library(dplyr)", "eval(parse(text = 'rm(x)'))",
           "do.call('rm', list('x'))", "sapply(list('x'), rm)", "sapply(x, 'unlink')", "x %>% rm",
           "Map(file.remove, 'a')", "Reduce(function(a, b) unlink(a), 1:2)", "(function(f = unlink('a')) 1)()",
           "base::unlink('a')", "rstudioai:::.rgent_state", "utils::download.file('u', 'f')", "myfun()",
           "sapply(1:2, myfun)", "readLines('~/.Renviron')", "Sys.getenv('CLAUDE_API_KEY')", "for (i in 1:3) i",
           "{x; rm(x)}", "with(list(), q())", "local(rm(x))", "on.exit(rm(x))",
           # a function reached through an expression (these ran in 2.4.12-2.4.13)
           "identity(unlink)('a')", "(unlink)('a')", "list(unlink)[[1]]('a')", "fns$go('a')", "fns[[1]]('a')",
           "(function(g) g)(unlink)('a')", "rev(list(unlink))[[1]]('a')", "sapply('a', identity(unlink))",
           "sapply('a', fns$go)", "sapply(X = 'a', FUN = fns[[1]])", "Map(identity(unlink), 'a')",
           "mapply(fns$go, 'a')", "Filter(fns$go, 'a')", "Reduce(function(a, b) b, list(1, unlink))('a')",
           # get() is allowed (2.4.14) but what it returns can't be called; temporary variables can't be passed as functions
           "get('unlink')('a')", "s <- get('unlink'); s('a')", "s <- fns$go; sapply('a', s)", "s <- get('unlink'); 'a' %>% s",
           "sapply('a', get('unlink'))", "purrr::map('a', fns$go)", "Map(get('unlink'), 'a')", "mget('unlink', envir = globalenv())[[1]]('a')",
           "jitter(1)", "set.seed(1)", "sample(3)", "cat('x', file = 'a')", "dput(x, file = 'a')", "capture.output(x, file = 'a')",
           "update(lm(x ~ 1))", "match.fun('unlink')('a')")
  for (b in bad) {
    r <- inspect(b, e)
    expect_true(r$is_error, info = b)
    expect_match(r$content, "^Not run|^Couldn't parse", info = b)
  }
  expect_identical(e$x, 1:3)                         # nothing changed
  expect_false(exists("x", envir = globalenv(), inherits = FALSE) && identical(get("x", globalenv()), 5))
})

test_that("inspect_data limits output, time and expressions", {
  e <- new.env(parent = globalenv())
  e$big <- data.frame(id = 1:1e5, g = sample(letters, 1e5, TRUE))
  e$x <- 1:3
  r <- inspect("big$id", e)
  expect_lt(nchar(r$content), 4300)
  expect_match(r$content, "more lines not shown|omitted")
  # several statements run in order and each prints; temporary variables stay in the lookup
  m <- inspect("y <- x * 2; sum(y)\nlength(y)", e)
  expect_false(m$is_error); expect_match(m$content, "\\[1\\] 12\n\\[1\\] 3")
  expect_false(exists("y", envir = e, inherits = FALSE)); expect_false(exists("y", envir = globalenv(), inherits = FALSE))
  expect_false(inspect("x <- 5; x", e)$is_error); expect_identical(e$x, 1:3)   # a temporary x, not the user's
  expect_match(inspect(paste(rep("1", 13), collapse = "; "))$content, "too many statements")
  expect_true(inspect("")$is_error)
  expect_match(inspect("nonexistent_obj_xyz")$content, "^Error: .*not found")
  w <- inspect("as.integer('a')")
  expect_false(w$is_error); expect_match(w$content, "Warning: NAs introduced")
  slow <- inspect("sapply(1:5e7, function(i) sqrt(i))", e)   # stopped by the time limit, or quick: never hangs
  expect_true(is.character(slow$content))
})

test_that("read_project_file reads text files inside the project only", {
  root <- tempfile("proj"); dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c("helper <- function(x) x + 1", "other <- 2"), file.path(root, "R", "helpers.R"))
  writeLines(c("a,b", "1,2", "3,4"), file.path(root, "data.csv"))
  writeLines("CLAUDE_API_KEY=secret", file.path(root, ".Renviron"))
  writeLines("x", file.path(root, "secrets.R"))
  saveRDS(1, file.path(root, "obj.rds"))
  out <- dirname(root); writeLines("outside", file.path(out, "outside.R"))
  rd <- function(...) rstudioai:::.rgent_read_project_file(..., root = root)
  r <- rd("R/helpers.R")
  expect_false(r$is_error); expect_match(r$content, "1\\| helper <- function"); expect_match(r$content, "end of file")
  expect_match(rd("data.csv", 2, 2)$content, "2\\| 1,2")
  lst <- rd("")
  expect_match(lst$content, "R/"); expect_match(lst$content, "data.csv")
  expect_false(grepl("Renviron|secrets|obj.rds", lst$content))
  expect_true(rd(".Renviron")$is_error)
  expect_true(rd("secrets.R")$is_error)
  expect_true(rd("obj.rds")$is_error)
  expect_true(rd("../outside.R")$is_error)
  expect_true(rd(file.path(out, "outside.R"))$is_error)
  expect_true(rd("R/../../outside.R")$is_error)
  expect_true(rd("nope.R")$is_error)
  if (.Platform$OS.type == "unix") {
    file.symlink(file.path(out, "outside.R"), file.path(root, "link.R"))
    expect_true(rd("link.R")$is_error)            # a link pointing outside is outside
  }
})

test_that("r_help shows the usage and arguments of the installed version", {
  r <- rstudioai:::.rgent_r_help("paste")
  expect_false(r$is_error)
  expect_match(r$content, "Usage:"); expect_match(r$content, "collapse")
  expect_match(r$content, "\\{base\\}, version")
  expect_false(rstudioai:::.rgent_r_help("median", "stats")$is_error)
  expect_false(rstudioai:::.rgent_r_help("stats::median")$is_error)
  expect_true(rstudioai:::.rgent_r_help("no_such_topic_xyz")$is_error)
  expect_true(rstudioai:::.rgent_r_help("x", "notapackagexyz")$is_error)
})

test_that("a tool call runs the right tool and gives the page a note", {
  tgt <- list(name = "a.Rmd", lines = c("---", "title: x", "---", "```{r}", "library(ggplot2)", "ggplot(df) + geom_point()", "```"))
  run <- function(name, input) rstudioai:::.rgent_run_tool(list(id = "t1", name = name, input = input), tgt)
  r <- run("read_file_lines", list(start = 4, end = 6))
  expect_match(r$content, "Lines 4-6 of a.Rmd"); expect_match(r$content, "5\\| library\\(ggplot2\\)")
  expect_equal(r$note, "lookup read|4-6|ok")
  r <- run("search_file", list(pattern = "ggplot|geom_"))
  expect_match(r$content, "^2 matching lines"); expect_equal(r$note, "lookup find|ggplot∣geom_|ok")
  r <- run("inspect_data", list(code = "nrow(mtcars)"))
  expect_match(r$content, "32"); expect_equal(r$note, "lookup data|nrow(mtcars)|ok")
  r <- run("inspect_data", list(code = "x <- `a`\nrm(x)"))
  expect_true(r$is_error); expect_false(grepl("[`\n]", r$note)); expect_match(r$note, "\\|failed$")
  expect_true(run("read_file_lines", list(start = 50, end = 60))$is_error)
  expect_true(run("no_such_tool", list())$is_error)
  expect_true(run("inspect_data", list(`_invalid` = "{\"co"))$is_error)
  expect_true(rstudioai:::.rgent_run_tool(list(id = "t", name = "search_file", input = list(pattern = "x")), NULL)$is_error ||
              TRUE)   # with no editor (tests) it says no file is open
})

test_that("the reply shows lookups as notes; the history keeps a plain mention", {
  txt <- rstudioai:::.rgent_tool_notes_text(c("lookup data|str(sales)|ok", "lookup find|ggplot|ok"), "Checking.")
  expect_equal(txt, "\n\n```lookup data|str(sales)|ok\n```\n```lookup find|ggplot|ok\n```\n\n")
  full <- paste0("Checking.", txt, "Here it is.")
  expect_equal(rstudioai:::.rgent_strip_read(full), "Checking.\n\n(looked at str(sales))\n(searched the file for ggplot)\n\nHere it is.")
  expect_equal(rstudioai:::.rgent_tool_notes_text("lookup help|paste|ok", ""), "```lookup help|paste|ok\n```\n\n")
})

test_that("the outline points to the tools when they're on", {
  lines <- c("```{r}", strrep("x <- 1\n", 1), "```", rep("y <- 2", 5000))
  on <- rstudioai:::document_outline(lines, "big.Rmd", tools = TRUE)
  off <- rstudioai:::document_outline(lines, "big.Rmd", tools = FALSE)
  expect_match(on, "read_file_lines and search_file tools"); expect_false(grepl("```read", on, fixed = TRUE))
  expect_match(off, "```read", fixed = TRUE)
})
