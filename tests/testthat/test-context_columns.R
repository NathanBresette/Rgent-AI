# One-line column summaries in Claude's context (R/context_columns.R)
f <- function(x) rstudioai:::.rgent_column_summary(x)

test_that("summaries show the codes that trip up a first guess", {
  expect_equal(f(c(26, -99, 84)), "range -99 to 84; values -99 (1), 26 (1), 84 (1)")
  expect_match(f(c(rep(50, 20), 9999)), "range 50 to 9999")
  expect_equal(f(c("N", "S", "", "N/A", "N")), "4 distinct: 'N' (2), '' (1), 'N/A' (1), 'S' (1)")
  sex <- structure(c(1L, 2L, 2L), labels = c(male = 1, female = 2))
  expect_match(f(sex), "^labels: male=1, female=2; range 1 to 2; values 1 \\(1\\), 2 \\(2\\)$")
  expect_equal(f(factor("a", levels = c("a", "b", "c"))), "3 levels (2 unused); 1 distinct: 'a' (1)")
  expect_equal(f(c(1.5, NA, 3)), "range 1.5 to 3; values 1.5 (1), 3 (1); 1 NA")
  expect_equal(f(as.Date("2026-01-01") + 0:2), "range 2026-01-01 to 2026-01-03")
  expect_equal(f(paste0("id", 1:50)), "50 distinct")
  # up to 12 values are listed (a status column with 9 spellings), 13 aren't
  st <- c("shipped", "Shipped", "SHIPPED", "pending", "Pending", "cancelled", "Cancelled", "CANCELLED", "returned")
  expect_match(f(st), "^9 distinct: 'cancelled' \\(1\\)")
  expect_equal(f(paste0("v", 1:13)), "13 distinct")
  expect_equal(f(list(1, 2)), "")                       # list columns: nothing, no error
  expect_equal(f(c(NA_real_, NA_real_)), "2 NA")
})

test_that("big data stays fast and says when it summarised part of a column", {
  big <- data.frame(id = 1:1e6, g = sample(letters, 1e6, TRUE), x = runif(1e6))
  t0 <- Sys.time(); cols <- rstudioai:::.rgent_df_columns(big)
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  expect_match(cols[[2]]$summary, "26 distinct; \\(from the first 200,000 rows\\)")
  expect_equal(cols[[1]]$summary, "range 1 to 1000000")
  wide <- as.data.frame(matrix(1, 3, 60))
  expect_length(rstudioai:::.rgent_df_columns(wide), 40)
  expect_equal(rstudioai:::.rgent_df_columns(mtcars)[[1]]$sample_values, c("21", "21"))
})
