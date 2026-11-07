# Data Explorer engine: the view's rows, the code for the view, formatting

ns <- asNamespace("rstudioai")
rt <- function(x) jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), simplifyVector = FALSE)
F <- function(column, type, value = NULL, value2 = NULL) list(column = column, type = type, value = value, value2 = value2)
S <- function(column, direction = "asc") list(column = column, direction = direction)

xp_data <- function() {
  data.frame(id = 1:8,
    `first name` = c("Ann", "Bob", NA, "", "a.b", "O'Neil", "ann", "Bob"),
    score = c(3.5, NA, 10, 2, 7.25, 0, -1, 100),
    when = as.Date("2024-01-01") + c(0, 40, NA, 10, 5, 300, 20, 1),
    flag = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE, FALSE),
    grp = factor(c("a", "b", "a", "c", NA, "a", "c", "b"), levels = c("c", "b", "a")),
    check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("the code for a view returns exactly the rows the view shows", {
  assign("xp_df", xp_data(), envir = globalenv())
  on.exit(rm("xp_df", envir = globalenv()), add = TRUE)
  views <- list(
    list(f = list(F("score", "greater_than", 2))),
    list(f = list(F("first name", "contains", "a.b"))),
    list(f = list(F("first name", "not_equals", "Bob")), s = list(S("score", "desc"))),
    list(f = list(F("when", "between", "2024-01-02", "2024-02-15"))),
    list(f = list(F("flag", "is_true"), F("grp", "equals", "a"))),
    list(s = list(S("grp"), S("first name", "desc"))),
    list(search = "b"),
    list(f = list(F("score", "is_null")))
  )
  for (v in views) {
    rows <- ns$explorer_view_rows(get("xp_df", globalenv()), rt(v$f), rt(v$s), v$search)
    expect_length(attr(rows, "errors"), 0)
    for (style in c("base", if (requireNamespace("dplyr", quietly = TRUE)) "dplyr")) {
      code <- ns$convert_explorer_view_to_code("xp_df", rt(v$f), rt(v$s), v$search, style = style)
      got <- eval(parse(text = code), new.env(parent = globalenv()))
      expect_identical(as.integer(got$id), as.integer(rows), info = code)
    }
  }
})

test_that("filters behave as documented", {
  d <- xp_data()
  r <- function(...) as.integer(ns$explorer_view_rows(d, rt(list(...)), NULL, NULL))
  expect_identical(r(F("score", "greater_than", 2)), c(1L, 3L, 5L, 8L))
  expect_identical(r(F("first name", "contains", "AN")), c(1L, 7L))   # literal, any case
  expect_true(3L %in% r(F("first name", "not_equals", "Bob")))         # NA kept
  bad <- attr(ns$explorer_view_rows(d, rt(list(F("score", "less_than", "abc"))), NULL, NULL), "errors")
  expect_match(bad[[1]], "isn't a number")
  desc <- as.integer(ns$explorer_view_rows(d, NULL, rt(list(S("score", "desc"))), NULL))
  expect_identical(desc[1], 8L)
  expect_identical(tail(desc, 1), 2L)                                    # NA last
})

test_that("search is exact while skipping columns that can't match", {
  set.seed(1)
  d <- data.frame(t = as.POSIXct("2024-01-01", tz = "America/New_York") + sample(0:3e7, 5000, TRUE),
                  x = round(runif(5000), 3), k = sample(1:30, 5000, TRUE),
                  day = as.Date("2024-01-01") + sample(0:300, 5000, TRUE), s = sample(c("ab", "Cd"), 5000, TRUE))
  for (term in c("0", "2024-02", "12:3", "c", "0.5", "-0", "7")) {
    pat <- ns$.rgent_regex_escape(term)
    slow <- which(Reduce(`|`, lapply(d, function(x) grepl(pat, x, ignore.case = TRUE))))
    expect_identical(as.integer(ns$explorer_view_rows(d, NULL, NULL, term)), slow, info = term)
  }
})

test_that("values are formatted for the grid", {
  fv <- ns$explorer_format_values
  expect_identical(fv(c(3.5, 1/3, 1e-5, 12345678, NA, NaN, Inf)), c("3.5", "0.3333333", "1e-05", "12345678", NA, "NaN", "Inf"))
  expect_identical(fv(list(1:2, "a", NULL)), c("<integer [2]>", "a", "<NULL>"))
  expect_true(is.na(fv(c("a", NA))[2]))
})

test_that("pages carry original row numbers, kinds and row names", {
  assign("xp_df", xp_data(), envir = globalenv()); assign("xp_cars", mtcars, envir = globalenv())
  on.exit(rm("xp_df", "xp_cars", envir = globalenv()), add = TRUE)
  ch <- ns$get_dataframe_chunk_filtered("xp_df", 1, 3, NULL, rt(list(S("score", "desc"))), NULL, NULL)
  expect_identical(ch$row_labels, c("8", "3", "5"))
  expect_identical(ch$column_kinds$when, "date")
  expect_identical(ns$get_dataframe_chunk_filtered("xp_cars", 1, 1, NULL, NULL, NULL, NULL)$row_labels, "Mazda RX4")
  expect_false(ns$get_dataframe_chunk_filtered("nope_not_here", 1, 1, NULL, NULL, NULL, NULL)$exists)
})

test_that("package globals are hidden from the object list", {
  expect_identical(ns$explorer_user_objects(c("df", "current_access_code", "websocket_server")), "df")
})
