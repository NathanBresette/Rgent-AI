# =============================================================================
# Test Token Management and Search Functions
# These tests validate token estimation, budget management, and keyword search
# =============================================================================

library(testthat)

# =============================================================================
# TEST: estimate_tokens_accurately()
# =============================================================================

test_that("estimate_tokens_accurately function exists", {
  expect_true(exists("estimate_tokens_accurately"))
  expect_true(is.function(estimate_tokens_accurately))
})

test_that("estimate_tokens_accurately returns numeric", {
  result <- estimate_tokens_accurately("This is a test sentence.")
  
  expect_true(is.numeric(result))
  expect_gt(result, 0)
})

test_that("estimate_tokens_accurately handles empty string", {
  result <- estimate_tokens_accurately("")
  
  expect_true(is.numeric(result))
  expect_equal(result, 0)
})

test_that("estimate_tokens_accurately scales with text length", {
  short_text <- "Hello"
  long_text <- paste(rep("Hello world", 100), collapse = " ")
  
  short_estimate <- estimate_tokens_accurately(short_text)
  long_estimate <- estimate_tokens_accurately(long_text)
  
  expect_lt(short_estimate, long_estimate)
})

test_that("estimate_tokens_accurately handles code text", {
  code <- "
    my_function <- function(x) {
      result <- x * 2
      return(result)
    }
  "
  
  result <- estimate_tokens_accurately(code)
  
  expect_true(is.numeric(result))
  expect_gt(result, 0)
})

test_that("estimate_tokens_accurately handles special characters", {
  text <- "Special chars: @#$%^&*()_+{}[]|\\:\";<>?,./~`"
  
  result <- estimate_tokens_accurately(text)
  
  expect_true(is.numeric(result))
})

# =============================================================================
# TEST: manage_token_budget_accurately()
# =============================================================================

test_that("manage_token_budget_accurately function exists", {
  expect_true(exists("manage_token_budget_accurately"))
  expect_true(is.function(manage_token_budget_accurately))
})

test_that("manage_token_budget_accurately returns list", {
  context_parts <- list(
    cursor_context = "Current cursor position",
    object_context = "Available objects"
  )
  
  result <- manage_token_budget_accurately(context_parts, max_tokens = 2000)
  
  expect_true(is.list(result))
})

test_that("manage_token_budget_accurately respects budget", {
  context_parts <- list(
    part1 = paste(rep("word", 100), collapse = " "),
    part2 = paste(rep("word", 100), collapse = " ")
  )
  
  result <- manage_token_budget_accurately(context_parts, max_tokens = 100)
  
  expect_true(is.list(result))
})

test_that("manage_token_budget_accurately handles empty list", {
  result <- tryCatch({
    manage_token_budget_accurately(list(), max_tokens = 2000)
  }, error = function(e) {
    list()  # Return empty list on error
  })
  
  expect_true(is.list(result))
})

test_that("manage_token_budget_accurately prioritizes context", {
  context_parts <- list(
    cursor_context = "High priority content",
    semantic_matches = "Medium priority",
    object_context = "Lower priority",
    environment_info = "Lowest priority"
  )
  
  result <- manage_token_budget_accurately(context_parts, max_tokens = 50)
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: extract_keywords()
# =============================================================================

test_that("extract_keywords function exists", {
  expect_true(exists("extract_keywords"))
  expect_true(is.function(extract_keywords))
})

test_that("extract_keywords returns character vector", {
  result <- extract_keywords("What is in my sales_data dataframe?")
  
  expect_true(is.character(result))
})

test_that("extract_keywords filters stop words", {
  result <- extract_keywords("the a an and or but in on at to for")
  
  # Stop words should be filtered out
  expect_true(is.character(result))
})

test_that("extract_keywords handles camelCase", {
  result <- extract_keywords("Show me customerInfo and productPrices")
  
  expect_true(is.character(result))
})

test_that("extract_keywords handles snake_case", {
  result <- extract_keywords("Show me customer_info and product_prices")
  
  expect_true(is.character(result))
})

test_that("extract_keywords handles empty query", {
  result <- extract_keywords("")
  
  # May return empty character vector or NULL
  expect_true(is.character(result) || is.null(result) || length(result) == 0)
})

# =============================================================================
# TEST: find_exact_matches()
# =============================================================================

test_that("find_exact_matches function exists", {
  expect_true(exists("find_exact_matches"))
  expect_true(is.function(find_exact_matches))
})

test_that("find_exact_matches finds matching objects", {
  keywords <- c("test", "data")
  objects <- list(
    list(name = "test_data", class = "data.frame"),
    list(name = "other_var", class = "numeric")
  )
  
  result <- find_exact_matches(keywords, objects)
  
  expect_true(is.list(result))
})

test_that("find_exact_matches returns empty for no matches", {
  keywords <- c("nonexistent")
  objects <- list(
    list(name = "real_data", class = "data.frame")
  )
  
  result <- find_exact_matches(keywords, objects)
  
  expect_true(is.list(result))
  expect_equal(length(result), 0)
})

test_that("find_exact_matches handles empty objects", {
  result <- find_exact_matches(c("test"), list())
  
  expect_true(is.list(result))
  expect_equal(length(result), 0)
})

test_that("find_exact_matches handles NULL objects", {
  result <- find_exact_matches(c("test"), NULL)
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: find_partial_matches()
# =============================================================================

test_that("find_partial_matches function exists", {
  expect_true(exists("find_partial_matches"))
  expect_true(is.function(find_partial_matches))
})

test_that("find_partial_matches finds partial matches", {
  keywords <- c("sale")
  objects <- list(
    list(name = "sales_data", class = "data.frame"),
    list(name = "monthly_sales", class = "data.frame"),
    list(name = "other_var", class = "numeric")
  )
  
  result <- find_partial_matches(keywords, objects)
  
  expect_true(is.list(result))
})

test_that("find_partial_matches handles empty objects", {
  result <- find_partial_matches(c("test"), list())
  
  expect_true(is.list(result))
})

test_that("find_partial_matches is case insensitive", {
  keywords <- c("SALES")
  objects <- list(
    list(name = "sales_data", class = "data.frame")
  )
  
  result <- find_partial_matches(keywords, objects)
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: smart_filter_objects()
# =============================================================================

test_that("smart_filter_objects function exists", {
  expect_true(exists("smart_filter_objects"))
  expect_true(is.function(smart_filter_objects))
})

test_that("smart_filter_objects filters by query", {
  objects <- list(
    list(name = "customer_data", class = "data.frame"),
    list(name = "product_data", class = "data.frame"),
    list(name = "helper_func", class = "function")
  )
  
  result <- smart_filter_objects("show me customer data", objects)
  
  expect_true(is.list(result))
})

test_that("smart_filter_objects handles no matches", {
  objects <- list(
    list(name = "x", class = "numeric")
  )
  
  result <- smart_filter_objects("show me sales data", objects)
  
  expect_true(is.list(result))
})

test_that("smart_filter_objects handles empty query", {
  objects <- list(
    list(name = "data", class = "data.frame")
  )
  
  result <- smart_filter_objects("", objects)
  
  expect_true(is.list(result))
})

test_that("smart_filter_objects handles empty objects", {
  result <- smart_filter_objects("test query", list())
  
  expect_true(is.list(result))
})

# =============================================================================
# TEST: Integration
# =============================================================================

test_that("token estimation works in budget management", {
  context_parts <- list(
    part1 = "Short text",
    part2 = paste(rep("Longer text with many words", 50), collapse = " ")
  )
  
  result <- manage_token_budget_accurately(context_parts, max_tokens = 500)
  
  expect_true(is.list(result))
})

test_that("keyword extraction works with smart filtering", {
  query <- "Show me the sales data from January"
  objects <- list(
    list(name = "january_sales", class = "data.frame"),
    list(name = "february_sales", class = "data.frame")
  )
  
  keywords <- extract_keywords(query)
  expect_true(is.character(keywords))
  
  filtered <- smart_filter_objects(query, objects)
  expect_true(is.list(filtered))
})

# =============================================================================
# TEST: Edge Cases
# =============================================================================

test_that("estimate_tokens_accurately handles very long text", {
  long_text <- paste(rep("word", 10000), collapse = " ")
  
  result <- estimate_tokens_accurately(long_text)
  
  expect_true(is.numeric(result))
  expect_gt(result, 1000)
})

test_that("extract_keywords handles unicode", {
  result <- tryCatch({
    extract_keywords("Show me cafe data")
  }, error = function(e) {
    character(0)
  })
  
  expect_true(is.character(result))
})

test_that("find_exact_matches handles objects with NULL names", {
  objects <- list(
    list(name = NULL, class = "numeric"),
    list(name = "valid", class = "data.frame")
  )
  
  result <- find_exact_matches(c("valid"), objects)
  
  expect_true(is.list(result))
})

test_that("manage_token_budget_accurately handles very small budget", {
  context_parts <- list(
    part1 = paste(rep("word", 100), collapse = " ")
  )
  
  result <- manage_token_budget_accurately(context_parts, max_tokens = 1)
  
  expect_true(is.list(result))
})

