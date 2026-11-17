# The history sent with a question (2.4.20): older messages in a short form fixed
# when they're written, growing only at the end, so the backend can cache it.

reset_history <- function() {
  st <- rstudioai:::.rgent_state
  assign("conversation_history", list(), envir = st)
  assign("history_from", NULL, envir = st)
}

add <- function(role, text) rstudioai:::add_to_conversation_history(role, text)

test_that("the older history only grows at the end, and the latest replies come whole", {
  reset_history()
  add("user", "plot mpg")
  long_reply <- paste0("Here it is.\n\n```r\n", paste(rep("x <- 1", 30), collapse = "\n"), "\n```\n", strrep("words ", 400))
  add("ai", long_reply)
  p1 <- rstudioai:::.rgent_history_parts()
  expect_equal(p1$older, "USER: plot mpg")
  expect_equal(p1$latest, long_reply)

  add("user", "now color it")
  add("ai", "Colored.")
  add("ai", "WRAPUP: the plot shows 3 groups.")
  p2 <- rstudioai:::.rgent_history_parts()
  expect_equal(p2$older[seq_along(p1$older)], p1$older)          # byte-identical start
  expect_length(p2$older, 3)
  expect_match(p2$older[2], "^AI: Here it is\\.")
  expect_match(p2$older[2], "[r block, 30 lines", fixed = TRUE)    # code shortened to a note
  expect_true(nchar(p2$older[2]) < 1600)
  expect_equal(p2$latest, "Colored.\n\nWRAPUP: the plot shows 3 groups.")   # every reply to the last question, whole

  add("user", "add a title")
  add("ai", "Added.")
  p3 <- rstudioai:::.rgent_history_parts()
  expect_equal(p3$older[seq_along(p2$older)], p2$older)
})

test_that("a long history is trimmed in one step and then stays put", {
  reset_history()
  for (i in 1:12) { add("user", paste("question", i, strrep("q", 500))); add("ai", paste("answer", i, strrep("a", 900))) }
  p <- rstudioai:::.rgent_history_parts()
  expect_lte(sum(nchar(p$older)), rstudioai:::.rgent_history_drop_to)   # trimmed down to 6,000 at once
  first <- p$older[1]
  add("user", "one more"); add("ai", "ok")
  q <- rstudioai:::.rgent_history_parts()
  expect_equal(q$older[1], first)                                        # same start: still cacheable
  expect_equal(q$older[seq_along(p$older)], p$older)
})

test_that("New chat and an opened chat start a fresh window", {
  reset_history()
  add("user", "a"); add("ai", "b")
  assign("history_from", 999, envir = rstudioai:::.rgent_state)
  reset_history()
  add("user", "c"); add("ai", "d")
  expect_equal(rstudioai:::.rgent_history_parts()$older, "USER: c")
})

test_that("messages without ids (restored or from tests) are still sent", {
  reset_history()
  assign("conversation_history", list(list(role = "user", message = "old q"), list(role = "ai", message = "old a")),
         envir = rstudioai:::.rgent_state)
  p <- rstudioai:::.rgent_history_parts()
  expect_equal(p$older, "USER: old q")
  expect_equal(p$latest, "old a")
})
