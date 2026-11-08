# Tests save chats (add_to_conversation_history -> chat store) and access codes
# under tools::R_user_dir(); point both at a temp folder so a test run never
# writes into the developer's real History or saved code.
local({
  root <- file.path(tempdir(), "rgent-test-user-dirs")
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  Sys.setenv(R_USER_DATA_DIR = file.path(root, "data"),
             R_USER_CONFIG_DIR = file.path(root, "config"),
             R_USER_CACHE_DIR = file.path(root, "cache"))
})
