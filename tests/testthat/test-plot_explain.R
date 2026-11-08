test_that("Explain plot tells Claude the values the ggplot draws", {
  skip_if_not_installed("ggplot2")
  suppressMessages(library(ggplot2))
  d <- data.frame(month = rep(c("2025-01", "2025-02", "2025-03"), 2),
                  revenue = c(100, 250, 175, 90, 80, 300), product = rep(c("Basic", "Pro"), each = 3))
  p <- ggplot(d, aes(month, revenue, colour = product, group = product)) + geom_line()
  ggplot2::set_last_plot(p)
  note <- rstudioai:::.rgent_ggplot_note()
  expect_match(note, "Basic: 2025-01: 100, 2025-02: 250, 2025-03: 175", fixed = TRUE)
  expect_match(note, "Pro: 2025-01: 90, 2025-02: 80, 2025-03: 300", fixed = TRUE)
  expect_match(note, "Layers: Line", fixed = TRUE)
  old <- if (exists("last_plot_command", envir = .GlobalEnv)) get("last_plot_command", envir = .GlobalEnv)
  assign("last_plot_command", "ggplot(d, aes(month, revenue)) + geom_line()", envir = .GlobalEnv)
  assign("last_plot_timestamp", Sys.time(), envir = .GlobalEnv)
  on.exit(rm(list = intersect(c("last_plot_command", "last_plot_timestamp"), ls(.GlobalEnv)), envir = .GlobalEnv))
  full <- rstudioai:::.rgent_plot_note()
  expect_match(full, "Code that made it:", fixed = TRUE)
  expect_match(full, "never guess values", fixed = TRUE)
})

test_that("Explain plot with no plot says so instead of inventing one", {
  local_mocked_bindings(.rgent_plot_command = function() NULL, .rgent_ggplot_note = function(...) NULL,
                        .package = "rstudioai")
  expect_match(rstudioai:::.rgent_plot_note(), "no recent plot was found", fixed = TRUE)
})

test_that("Explain plot captures the plot as a small PNG for Claude", {
  img <- rstudioai:::.rgent_plot_image
  png_magic <- function(b64) identical(as.integer(jsonlite::base64_dec(substr(b64, 1, 12))[1:4]), c(137L, 80L, 78L, 71L))
  old <- options(device = function(...) { grDevices::pdf(NULL); grDevices::dev.control("enable") })
  on.exit({ options(old); while (grDevices::dev.cur() > 1) grDevices::dev.off() }, add = TRUE)
  g <- .GlobalEnv
  had <- exists("last_plot_command", envir = g, inherits = FALSE)
  on.exit(if (!had) suppressWarnings(rm("last_plot_command", "last_plot_timestamp", envir = g)), add = TRUE)
  # Base graphics on the current device
  assign("last_plot_command", "plot(1:10)", envir = g); assign("last_plot_timestamp", Sys.time(), envir = g)
  plot(1:10)
  b <- img()
  expect_true(is.character(b) && nchar(b) > 1000 && png_magic(b))
  expect_lt(nchar(b), 1.4e6)
  # ggplot2: the last ggplot
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  print(p)
  assign("last_plot_command", "ggplot(mtcars, aes(wt, mpg)) + geom_point()", envir = g)
  b2 <- img()
  expect_true(is.character(b2) && png_magic(b2))
  expect_false(grepl("\n", b2, fixed = TRUE))   # one unbroken string, as the API wants
  # No plot at all
  while (grDevices::dev.cur() > 1) grDevices::dev.off()
  store <- rstudioai:::.rgent_plot_store
  rm(list = ls(store), envir = store)
  assign("last_plot_command", "plot(1:10)", envir = g)
  expect_null(img())
  # A base plot the chat ran: drawn into a file and its device closed, yet still sent
  invisible(capture.output(rstudioai:::execute_code_in_session("plot(1:10, main = 'from chat')")))
  expect_equal(grDevices::dev.cur(), c(`null device` = 1L))
  b3 <- img()
  expect_true(is.character(b3) && nchar(b3) > 1000 && png_magic(b3))
  kept_b64 <- function() gsub("[\r\n]", "", jsonlite::base64_enc(readBin(rstudioai:::.rgent_kept_chat_plot(), "raw", 1e6)))
  # The user's own plot (RStudio's Plots pane) survives a chat run
  plot(10:1, main = "user's plot"); user_dev <- grDevices::dev.cur()
  invisible(capture.output(rstudioai:::execute_code_in_session("plot(1:10, main = 'from chat again')")))
  expect_true(user_dev %in% grDevices::dev.list())
  expect_equal(grDevices::dev.cur()[[1]], user_dev[[1]])
  # ...and Explain plot uses the chat's newer plot, not the user's device
  expect_identical(img(), kept_b64())
  # Once the user draws something, that is the newest plot
  plot(1:3, main = "user's newer plot")
  b4 <- img()
  expect_true(png_magic(b4) && !identical(b4, kept_b64()))
  rm(list = ls(store), envir = store)
})

test_that("Explain on a plot card names that plot and uses its picture and code", {
  code <- "plot(1:3)\nhist(rnorm(10))\nbarplot(1:4)"
  n <- rstudioai:::.rgent_plot_note(index = 2, count = 3, code = code)
  expect_match(n, "plot 2 of 3", fixed = TRUE)
  expect_match(n, "2nd plot", fixed = TRUE)
  expect_match(n, "barplot(1:4)", fixed = TRUE)
  # the composer's Explain plot (no index) doesn't claim a numbered plot
  expect_false(grepl("of 3", rstudioai:::.rgent_plot_note(code = code)))
  # the card's picture: PNG/JPEG base64 only
  f <- tempfile(fileext = ".png"); grDevices::png(f, 300, 240); plot(1); grDevices::dev.off()
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size)))
  expect_identical(rstudioai:::.rgent_card_image(b64), b64)
  expect_null(rstudioai:::.rgent_card_image(jsonlite::base64_enc(charToRaw("not an image"))))
  expect_null(rstudioai:::.rgent_card_image("<script>"))
  expect_null(rstudioai:::.rgent_card_image(NULL))
})

test_that("the message after code runs gets the question, code and output, trimmed", {
  p <- rstudioai:::.rgent_wrapup_prompt("which vars matter?", "summary(fit)", "Coefficients:\n x  0.5  p = 0.01", 0)
  expect_match(p, "What I asked: which vars matter?", fixed = TRUE)
  expect_match(p, "```r\nsummary(fit)\n```", fixed = TRUE)
  expect_match(p, "p = 0.01", fixed = TRUE)
  expect_match(p, "1-3 short sentences", fixed = TRUE)
  expect_false(grepl("plot", sub("drew plots", "", p)))
  # long output keeps the start and the end (where results usually are)
  long <- paste(c("HEADER", rep("filler line", 2000), "THE RESULT = 42"), collapse = "\n")
  q <- rstudioai:::.rgent_wrapup_prompt("q", "x", long, 2)
  expect_match(q, "HEADER", fixed = TRUE); expect_match(q, "THE RESULT = 42", fixed = TRUE)
  expect_lt(nchar(q), 6000)
  expect_match(q, "It drew 2 plots, shown to me in this chat", fixed = TRUE)
  expect_match(q, "don't mention that you can't see it", fixed = TRUE)
  expect_false(grepl("Plots pane", q, fixed = TRUE))
  expect_match(rstudioai:::.rgent_wrapup_prompt("q", "x", "", NULL), "It printed nothing.", fixed = TRUE)
})
