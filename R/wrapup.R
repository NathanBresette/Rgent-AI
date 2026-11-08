# After code from a reply runs, Rgent says what the result means (2.4.8).
#
# Before, a reply ended with the code and its output (or a plot), and the
# question ("which variables matter?") was never answered in words. The page
# sends `wrapup = TRUE` with the code, its output and the number of plots after
# a successful run; chat_with_ai sends this lean prompt (no workspace, file or
# history) as a continuation of the question, so it doesn't count as a request.

#' The prompt for the short message after code runs
#' @keywords internal
.rgent_wrapup_prompt <- function(question, code, output, plots = 0) {
  one <- function(x) paste(unlist(x, use.names = FALSE), collapse = "\n")
  q <- substr(one(question), 1, 600)
  code <- one(code)
  if (nchar(code) > 3000) code <- paste0(substr(code, 1, 3000), "\n...")
  out <- trimws(one(output))
  # Long output: the start (headers, call) and the end (the result usually comes last)
  if (nchar(out) > 4000) out <- paste0(substr(out, 1, 1500), "\n...\n", substr(out, nchar(out) - 2499, nchar(out)))
  plots <- suppressWarnings(as.integer(plots))
  if (length(plots) != 1 || is.na(plots) || plots < 0) plots <- 0L
  paste0(
    "What I asked: ", q, "\n\n",
    "=== THE CODE YOU GAVE RAN WITHOUT ERRORS ===\n",
    if (nzchar(code)) paste0("```r\n", code, "\n```\n") else "",
    if (nzchar(out)) paste0("Its output:\n```\n", out, "\n```\n") else "It printed nothing.\n",
    # Plots are cards in the chat, not the Plots pane; Claude doesn't get the
    # images, and used to say "I can't see it", which helps nobody
    if (plots > 0) paste0("It drew ", plots, if (plots == 1) " plot" else " plots",
                          ", shown to me in this chat. You don't get the image: say what it shows from the code, ",
                          "don't quote values you can't know, and don't mention that you can't see it or where it is shown.\n") else "",
    "\nNow answer what I asked from this result in 1-3 short sentences, quoting the actual numbers. ",
    "If the code only did a step (loaded, cleaned or saved something, or drew plots), say in one sentence what was done ",
    "and, if it helps, suggest one next step. No code blocks, no headings, don't repeat the output and don't describe the code."
  )
}
