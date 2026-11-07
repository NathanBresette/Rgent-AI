with_chat_dir <- function(code) {
  old <- Sys.getenv("R_USER_DATA_DIR")
  tmp <- tempfile("chats")
  Sys.setenv(R_USER_DATA_DIR = tmp)
  old_hist <- if (exists("conversation_history", envir = globalenv())) get("conversation_history", envir = globalenv()) else NULL
  on.exit({
    Sys.setenv(R_USER_DATA_DIR = old); unlink(tmp, recursive = TRUE); chat_store_new()
    assign("conversation_history", old_hist %||% list(), envir = globalenv())
  })
  chat_store_new()
  force(code)
}

test_that("chats are saved from the first user message, reopened and exported", {
  with_chat_dir({
    add_to_conversation_history("ai", "Hi! (greeting, not saved)")
    expect_length(chat_store_list()$chats, 0)
    add_to_conversation_history("user", "Plot mpg against wt")
    add_to_conversation_history("ai", "```r\nplot(mtcars$wt, mtcars$mpg)\n```")
    l <- chat_store_list()
    expect_length(l$chats, 1)
    expect_equal(l$chats[[1]]$title, "Plot mpg against wt")
    expect_equal(l$chats[[1]]$messages, 2)
    id <- l$chats[[1]]$id
    expect_true(startsWith(normalizePath(.rgent_chat_dir()), normalizePath(Sys.getenv("R_USER_DATA_DIR"))))

    chat_store_new()
    add_to_conversation_history("user", "Second chat")
    expect_length(chat_store_list()$chats, 2)

    ch <- chat_store_open(id)
    expect_equal(ch$messages[[2]]$text, "```r\nplot(mtcars$wt, mtcars$mpg)\n```")
    expect_equal(length(get("conversation_history", envir = globalenv())), 2)  # context restored
    add_to_conversation_history("user", "Add a trend line")                   # continues the reopened chat
    expect_equal(chat_store_list()$chats[[1]]$id, id)
    expect_equal(chat_store_list()$chats[[1]]$messages, 3)

    md <- chat_store_markdown(id)
    expect_match(md, "^# Plot mpg against wt")
    expect_match(md, "## You\n\nAdd a trend line")

    chat_store_delete(id)
    expect_length(chat_store_list()$chats, 1)
    chat_store_delete(all = TRUE)
    expect_length(chat_store_list()$chats, 0)
    expect_null(chat_store_open("../../etc/passwd"))   # ids are checked
  })
})

test_that("saved chats stay small", {
  with_chat_dir({
    # images and very long messages are cut
    add_to_conversation_history("user", paste0("look ![p](data:image/png;base64,", strrep("A", 50000), ")"))
    add_to_conversation_history("ai", strrep("x", 50000))
    id <- chat_store_list()$chats[[1]]$id
    ch <- chat_store_open(id)
    expect_match(ch$messages[[1]]$text, "[image]", fixed = TRUE)
    expect_lt(nchar(ch$messages[[2]]$text), 10100)
    # messages per chat are capped
    for (i in 1:120) add_to_conversation_history("user", paste("m", i))
    expect_equal(chat_store_list()$chats[[1]]$messages, .rgent_chat_limits$max_messages)

    # chat count and folder size are capped, oldest removed first, open chat kept
    old <- .rgent_chat_limits
    on.exit(assignInNamespace(".rgent_chat_limits", old, "rstudioai"), add = TRUE)
    lim <- old; lim$max_chats <- 5L; lim$max_total_bytes <- 20000
    assignInNamespace(".rgent_chat_limits", lim, "rstudioai")
    keep <- id
    for (k in 1:8) {
      chat_store_new()
      add_to_conversation_history("user", paste("chat", k, strrep("y", 3000)))
      Sys.setFileTime(.rgent_chat_file(.rgent_chat$current$id), Sys.time() + k)
    }
    l <- chat_store_list()
    expect_lte(length(l$chats), 5)
    expect_lte(l$bytes, 20000)
    expect_true(grepl("^chat 8", l$chats[[1]]$title))   # newest kept
    expect_false(any(vapply(l$chats, function(c) grepl("^chat 1 ", c$title), TRUE)))
  })
})
