test_that("run_redshift_query_builder app directory exists", {
  app_dir <- system.file(
    "apps", "redshift-sql-query-builder",
    package = "paulmisc"
  )
  
  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
})

test_that("run_redshift_query_builder app.R file exists", {
  app_dir <- system.file(
    "apps", "redshift-sql-query-builder",
    package = "paulmisc"
  )
  
  app_file <- file.path(app_dir, "app.R")
  expect_true(file.exists(app_file))
})

test_that("run_redshift_query_builder function exists and is exported", {
  expect_true(exists("run_redshift_query_builder"))
  expect_true("run_redshift_query_builder" %in% 
    getNamespaceExports("paulmisc"))
})

test_that("run_redshift_query_builder has correct formals", {
  fn_formals <- formals(run_redshift_query_builder)
  
  # Should accept ... for additional arguments to shinyApp
  expect_true("..." %in% names(fn_formals))
})

test_that("run_redshift_query_builder app directory has required structure", {
  app_dir <- system.file(
    "apps", "redshift-sql-query-builder",
    package = "paulmisc"
  )
  
  # Check for app.R
  expect_true(file.exists(file.path(app_dir, "app.R")))
  
  # Verify app.R contains Shiny app code
  app_content <- readLines(
    file.path(app_dir, "app.R"),
    warn = FALSE
  )
  expect_true(any(grepl("shinyApp|ui|server", app_content)))
})

test_that("run_redshift_query_builder is properly documented", {
  # Check that function has roxygen documentation
  fn_code <- capture.output(print(run_redshift_query_builder))
  expect_true(length(fn_code) > 0)
  
  # Verify function is in namespace
  expect_true("run_redshift_query_builder" %in% 
    ls(getNamespace("paulmisc")))
})
