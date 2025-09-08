# Test basic package functionality

test_that("package loads without errors", {
  expect_true(require(rstudioai))
})

test_that("package has required components", {
  # Check that main functions exist
  expect_true(exists("run_rgent"))
  expect_true(exists("start_cleaning_agent"))
  expect_true(exists("start_transformation_agent"))
  expect_true(exists("start_statistical_analysis"))
  expect_true(exists("start_modeling_agent"))
})

test_that("package metadata is correct", {
  desc <- packageDescription("rstudioai")
  expect_equal(desc$Package, "rstudioai")
  expect_match(desc$Title, "RgentAI")
  expect_equal(desc$License, "MIT + file LICENSE")
})

test_that("dependencies are available", {
  required_packages <- c("httr", "jsonlite", "rstudioapi", "clipr", "shiny", "httpuv")
  
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
    "start_cleaning_agent", 
    "start_transformation_agent",
    "start_statistical_analysis",
    "start_modeling_agent"
  )
  
  for (func in expected_exports) {
    expect_true(func %in% exports, 
                info = paste("Function", func, "should be exported"))
  }
})
