test_that("object view drills into lists and models with a usable R path", {
  assign("ov_fit", lm(mpg ~ wt, data = mtcars), envir = globalenv())
  assign("ov_cfg", list(a = 1:3, b = list(c = "x"), "odd name" = TRUE, 5, a = 2), envir = globalenv())
  on.exit(rm(ov_fit, ov_cfg, envir = globalenv()))

  v <- explorer_object_view("ov_fit")
  expect_equal(v$kind, "tree")
  expect_true(any(grepl("Coefficients", v$summary$lines)))

  v <- explorer_object_view("ov_fit", list(list(t = "n", v = "coefficients")))
  expect_equal(v$kind, "vector")
  expect_equal(v$expr, "ov_fit$coefficients")
  expect_equal(unlist(v$labels), c("(Intercept)", "wt"))

  v <- explorer_object_view("ov_cfg")
  labels <- vapply(v$children, `[[`, "", "label")
  # Duplicate and blank names fall back to positions
  expect_equal(labels, c("[[1]]", "b", "odd name", "[[4]]", "[[5]]"))
  v <- explorer_object_view("ov_cfg", list(list(t = "n", v = "odd name")))
  expect_equal(v$expr, 'ov_cfg[["odd name"]]')
  expect_identical(eval(parse(text = v$expr)), TRUE)
})

test_that("object view handles functions, big vectors and missing objects", {
  assign("ov_f", eval(parse(text = "function(x, y = 2) {\n  # keep me\n  x + y\n}", keep.source = TRUE)), envir = globalenv())
  assign("ov_big", runif(5000), envir = globalenv())
  on.exit(rm(ov_f, ov_big, envir = globalenv()))

  v <- explorer_object_view("ov_f")
  expect_equal(v$kind, "function")
  expect_true(is.na(v$args[[1]]$default))
  expect_equal(v$args[[2]]$default, "2")
  expect_true(any(grepl("keep me", v$code)))

  v <- explorer_object_view("ov_big", offset = 1000)
  expect_equal(v$length, 5000)
  expect_length(v$values, 1000)

  expect_false(explorer_object_view("ov_not_there")$exists)
})

test_that("Claude hears once what happened to its edits", {
  .rgent_edits$log <- NULL
  expect_equal(edit_history_note(), "")
  .rgent_edit_note("accepted your edit (lines 2-3 of a.R)")
  .rgent_edit_note("undid your edit (lines 2-3 of a.R) with its Undo button")
  note <- edit_history_note()
  expect_match(note, "accepted your edit")
  expect_match(note, "undid your edit")
  expect_equal(edit_history_note(), "")
  expect_match(edit_instructions("a.Rmd"), "wrap the edit in ````", fixed = TRUE)
  expect_match(edit_instructions("a.Rmd"), "Undo")
})
