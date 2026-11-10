# `x %||% y`: y when x is NULL. Base R has it only from 4.4.0, and the package
# supports R 4.0, so it's defined here (it used to live in visualization_agent.R,
# removed in 2.4.4).
`%||%` <- function(x, y) if (is.null(x)) y else x

# Names R CMD check can't see: `text` and `what` are scan()'s arguments, given to
# the prompt handler at run time (auto_recovery.R); `.Last.error` is rlang's, in
# the global environment when it exists.
utils::globalVariables(c("text", "what", ".Last.error"))
