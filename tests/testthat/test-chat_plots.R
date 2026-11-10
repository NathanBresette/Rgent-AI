# Plots the chat runs draw on one device per run, like the Console (2.4.5)

run_chat <- function(code) {
  out <- NULL
  invisible(capture.output(out <- rstudioai:::execute_code_in_session(code)))
  out
}
n_plots <- function(res) length(gregexpr("PLOT_DATA:", res$output, fixed = TRUE)[[1]][gregexpr("PLOT_DATA:", res$output, fixed = TRUE)[[1]] > 0])

test_that("base plots with par, legend and hist(add = TRUE) in a loop work", {
  while (grDevices::dev.cur() > 1) grDevices::dev.off()
  # The user's failing case: 'plot.new has not been called yet' from the loop
  code <- '
cols <- c("#66C2A5", "#FC8D62", "#8DA0CB")
par(mar = c(4.5, 4.5, 3, 1), las = 1, bty = "l")
boxplot(Sepal.Length ~ Species, data = iris, col = cols, main = "box")
plot(iris$Sepal.Length, iris$Sepal.Width, col = cols[as.integer(iris$Species)], main = "scatter")
legend("bottom", legend = levels(iris$Species), col = cols, pch = 16, horiz = TRUE, bty = "n")
brks <- seq(min(iris$Petal.Length), max(iris$Petal.Length), length.out = 21)
hist(iris$Petal.Length[iris$Species == "setosa"], breaks = brks, col = cols[1], ylim = c(0, 30), main = "hist")
for (i in 2:3) {
  hist(iris$Petal.Length[iris$Species == levels(iris$Species)[i]], breaks = brks, col = cols[i], add = TRUE)
}
legend("top", legend = levels(iris$Species), fill = cols, horiz = TRUE, bty = "n")'
  r <- run_chat(code)
  expect_true(isTRUE(r$success), info = r$error)
  expect_equal(n_plots(r), 3)
  # par(mfrow) puts all three panels on one page, as in the Console
  r2 <- run_chat('op <- par(mfrow = c(1, 3), adj = 0)\nplot(1:3)\nhist(rnorm(50))\nfor (k in 1:2) abline(h = k)\nbarplot(1:4)\npar(op)')
  expect_true(isTRUE(r2$success), info = r2$error)
  expect_equal(n_plots(r2), 1)
  # no device left open
  expect_equal(grDevices::dev.cur()[[1]], 1L)
})

test_that("ggplots and base plots keep their order; the user's own png() is left alone", {
  skip_if_not_installed("ggplot2")
  while (grDevices::dev.cur() > 1) grDevices::dev.off()
  out_file <- tempfile(fileext = ".png")
  code <- paste0('library(ggplot2)\n',
                 'p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()\n',
                 'p\n',
                 'plot(1:5)\n',
                 'print(p + ggtitle("two"))\n',
                 'png(', deparse(out_file), '); plot(5:1); dev.off()\n',  # deparse: Windows paths have backslashes
                 'x <- 1')
  r <- run_chat(code)
  expect_true(isTRUE(r$success), info = r$error)
  expect_equal(n_plots(r), 3)
  expect_true(file.exists(out_file) && file.info(out_file)$size > 100)
  # a plot that fails reports the real error
  r3 <- run_chat('library(ggplot2)\nggplot(mtcars, aes(wt, no_such_column)) + geom_point()')
  expect_false(isTRUE(r3$success))
  expect_match(r3$error, "no_such_column")
  expect_equal(grDevices::dev.cur()[[1]], 1L)
})
