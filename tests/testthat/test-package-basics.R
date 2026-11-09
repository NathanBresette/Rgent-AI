# Test basic package functionality

test_that("package loads without errors", {
  expect_true(require(rstudioai))
})

test_that("package has required components", {
  # Check that main functions exist
  expect_true(exists("run_rgent"))
  expect_true(exists("get_dataframe_info"))
  expect_true(exists("get_available_dataframes"))
  # The Agents code was removed in 2.4.4
  expect_false(exists("start_cleaning_agent", envir = asNamespace("rstudioai"), inherits = FALSE))
})

test_that("package metadata is correct", {
  desc <- packageDescription("rstudioai")
  expect_equal(desc$Package, "rstudioai")
  expect_match(desc$Title, "RgentAI")
  expect_equal(desc$License, "MIT + file LICENSE")
})

test_that("dependencies are available", {
  # Read Imports from DESCRIPTION: a hardcoded list went stale when 2.4.15
  # dropped shiny and clipr, and r-universe's check (which installs only
  # Imports) failed on it.
  imports <- packageDescription("rstudioai")$Imports
  required_packages <- trimws(sub("\\(.*", "", strsplit(imports, ",")[[1]]))
  required_packages <- required_packages[nzchar(required_packages)]
  expect_true(all(c("httr", "jsonlite", "rstudioapi", "httpuv") %in% required_packages))
  
  for (pkg in required_packages) {
    expect_true(require(pkg, character.only = TRUE, quietly = TRUE),
                info = paste("Package", pkg, "should be available"))
  }
})

test_that("namespace exports are correct", {
  # Check that expected functions are exported
  exports <- getNamespaceExports("rstudioai")
  
  expected_exports <- c(
    "run_rgent",
    "get_dataframe_info",
    "get_available_dataframes",
    "get_object_explorer_data",
    "check_and_update_package"
  )
  
  for (func in expected_exports) {
    expect_true(func %in% exports, 
                info = paste("Function", func, "should be exported"))
  }
})
