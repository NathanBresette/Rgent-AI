test_that("%||% is the package's own, so R before 4.4 works", {
  expect_true(exists("%||%", envir = asNamespace("rstudioai"), inherits = FALSE))
  f <- get("%||%", envir = asNamespace("rstudioai"))
  expect_equal(f(NULL, 2), 2)
  expect_equal(f(1, 2), 1)
})
