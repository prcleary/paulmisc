test_that("simulate_outbreak returns a data frame with correct structure", {
  result <- simulate_outbreak(n = 10, seed = 123)
  
  # Check that output is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check dimensions
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 6)
  
  # Check column names
  expected_cols <- c("case_id", "onset_date", "age_group", "sex", "outcome", "setting")
  expect_equal(names(result), expected_cols)
})

test_that("simulate_outbreak produces correct column types", {
  result <- simulate_outbreak(n = 15, seed = 456)
  
  # Check column types
  expect_type(result$case_id, "character")
  expect_s3_class(result$onset_date, "Date")
  expect_type(result$age_group, "character")
  expect_type(result$sex, "character")
  expect_type(result$outcome, "character")
  expect_type(result$setting, "character")
})

test_that("simulate_outbreak produces valid categorical values", {
  result <- simulate_outbreak(n = 50, seed = 789)
  
  # Check age_group values
  expect_true(all(result$age_group %in% c("Child", "Adult", "Elderly")))
  
  # Check sex values
  expect_true(all(result$sex %in% c("Female", "Male")))
  
  # Check outcome values
  expect_true(all(result$outcome %in% c("Recovered", "Hospitalised")))
  
  # Check setting values
  expect_true(all(result$setting %in% c("Wedding A", "Wedding B")))
})

test_that("simulate_outbreak produces unique case IDs", {
  result <- simulate_outbreak(n = 30, seed = 111)
  
  expect_equal(length(unique(result$case_id)), 30)
  expect_equal(length(result$case_id), 30)
})

test_that("simulate_outbreak case IDs follow expected format", {
  result <- simulate_outbreak(n = 5, seed = 222)
  
  # Should match pattern C001, C002, etc.
  expect_true(all(grepl("^C\\d{3}$", result$case_id)))
  expect_equal(result$case_id, c("C001", "C002", "C003", "C004", "C005"))
})

test_that("simulate_outbreak respects seed for reproducibility", {
  result1 <- simulate_outbreak(n = 20, seed = 333)
  result2 <- simulate_outbreak(n = 20, seed = 333)
  
  expect_identical(result1, result2)
})

test_that("simulate_outbreak produces different results with different seeds", {
  result1 <- simulate_outbreak(n = 20, seed = 444)
  result2 <- simulate_outbreak(n = 20, seed = 555)
  
  expect_false(identical(result1, result2))
})

test_that("simulate_outbreak respects exposure parameter", {
  exposure_date <- as.Date("2025-01-01")
  result <- simulate_outbreak(n = 20, exposure = exposure_date, seed = 666)
  
  # All onset dates should be after exposure
  expect_true(all(result$onset_date > exposure_date))
})

test_that("simulate_outbreak accepts different date formats for exposure", {
  # Character string
  result1 <- simulate_outbreak(n = 5, exposure = "2025-03-15", seed = 777)
  expect_s3_class(result1$onset_date, "Date")
  
  # Date object
  result2 <- simulate_outbreak(n = 5, exposure = as.Date("2025-03-15"), seed = 777)
  expect_identical(result1, result2)
})

test_that("simulate_outbreak respects incubation period parameters", {
  # Very short incubation
  result_short <- simulate_outbreak(
    n = 100, 
    exposure = as.Date("2024-01-01"),
    meanlog = 0, 
    sdlog = 0.1, 
    seed = 888
  )
  
  # Very long incubation
  result_long <- simulate_outbreak(
    n = 100, 
    exposure = as.Date("2024-01-01"),
    meanlog = 3, 
    sdlog = 0.1, 
    seed = 888
  )
  
  # Median onset should differ
  median_days_short <- median(as.numeric(result_short$onset_date - as.Date("2024-01-01")))
  median_days_long <- median(as.numeric(result_long$onset_date - as.Date("2024-01-01")))
  
  expect_lt(median_days_short, median_days_long)
})

test_that("simulate_outbreak works with seed = NULL", {
  # Should not error
  expect_no_error(simulate_outbreak(n = 10, seed = NULL))
  
  # Results should differ (with high probability)
  result1 <- simulate_outbreak(n = 20, seed = NULL)
  result2 <- simulate_outbreak(n = 20, seed = NULL)
  
  # At least some differences expected
  expect_false(identical(result1$onset_date, result2$onset_date))
})

test_that("simulate_outbreak produces no missing values", {
  result <- simulate_outbreak(n = 30, seed = 999)
  
  expect_false(any(is.na(result$case_id)))
  expect_false(any(is.na(result$onset_date)))
  expect_false(any(is.na(result$age_group)))
  expect_false(any(is.na(result$sex)))
  expect_false(any(is.na(result$outcome)))
  expect_false(any(is.na(result$setting)))
})

test_that("simulate_outbreak handles n = 1", {
  result <- simulate_outbreak(n = 1, seed = 101)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$case_id, "C001")
})

test_that("simulate_outbreak handles large n", {
  result <- simulate_outbreak(n = 1000, seed = 202)
  
  expect_equal(nrow(result), 1000)
  expect_equal(length(unique(result$case_id)), 1000)
})

# Tests for time_unit parameter
test_that("simulate_outbreak with time_unit = 'hourly' returns POSIXct", {
  result <- simulate_outbreak(n = 20, time_unit = "hourly", seed = 303)
  
  expect_s3_class(result, "data.frame")
  expect_true("onset_time" %in% names(result))
  expect_false("onset_date" %in% names(result))
  expect_s3_class(result$onset_time, "POSIXct")
  expect_equal(nrow(result), 20)
})

test_that("simulate_outbreak with time_unit = 'weekly' returns Date", {
  result <- simulate_outbreak(n = 20, time_unit = "weekly", seed = 404)
  
  expect_s3_class(result, "data.frame")
  expect_true("onset_date" %in% names(result))
  expect_s3_class(result$onset_date, "Date")
  expect_equal(nrow(result), 20)
})

test_that("simulate_outbreak with time_unit = 'daily' returns Date (default)", {
  result <- simulate_outbreak(n = 20, time_unit = "daily", seed = 505)
  
  expect_s3_class(result, "data.frame")
  expect_true("onset_date" %in% names(result))
  expect_s3_class(result$onset_date, "Date")
  expect_equal(nrow(result), 20)
})

test_that("simulate_outbreak hourly data includes demographic attributes", {
  result <- simulate_outbreak(n = 15, time_unit = "hourly", seed = 606)
  
  # Should have all demographic columns
  expect_true("age_group" %in% names(result))
  expect_true("sex" %in% names(result))
  expect_true("outcome" %in% names(result))
  expect_true("setting" %in% names(result))
  
  # Check valid values
  expect_true(all(result$age_group %in% c("Child", "Adult", "Elderly")))
  expect_true(all(result$sex %in% c("Female", "Male")))
})

# Tests for pattern parameter
test_that("simulate_outbreak with pattern = 'continuous' returns uniform distribution", {
  result <- simulate_outbreak(
    n = 200,
    pattern = "continuous",
    date_range = 10,
    exposure = "2024-01-01",
    seed = 707
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 200)
  
  # Dates should span approximately date_range days
  date_spread <- as.numeric(max(result$onset_date) - min(result$onset_date))
  expect_lte(date_spread, 10)
})

test_that("simulate_outbreak with pattern = 'point_source' follows log-normal (default)", {
  result <- simulate_outbreak(
    n = 100,
    pattern = "point_source",
    exposure = "2024-01-01",
    seed = 808
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 100)
  
  # All dates should be after exposure
  expect_true(all(result$onset_date > as.Date("2024-01-01")))
})

test_that("simulate_outbreak continuous pattern with hourly time_unit", {
  result <- simulate_outbreak(
    n = 50,
    time_unit = "hourly",
    pattern = "continuous",
    date_range = 5,
    exposure = "2024-06-01",
    seed = 909
  )
  
  expect_s3_class(result$onset_time, "POSIXct")
  expect_equal(nrow(result), 50)
  
  # Time spread should be approximately date_range days in hours
  time_spread_hours <- as.numeric(difftime(max(result$onset_time), min(result$onset_time), units = "hours"))
  expect_lte(time_spread_hours, 5 * 24)
})

test_that("simulate_outbreak date_range parameter affects continuous pattern", {
  result_narrow <- simulate_outbreak(
    n = 100,
    pattern = "continuous",
    date_range = 5,
    exposure = "2024-01-01",
    seed = 1001
  )
  
  result_wide <- simulate_outbreak(
    n = 100,
    pattern = "continuous",
    date_range = 20,
    exposure = "2024-01-01",
    seed = 1001
  )
  
  spread_narrow <- as.numeric(max(result_narrow$onset_date) - min(result_narrow$onset_date))
  spread_wide <- as.numeric(max(result_wide$onset_date) - min(result_wide$onset_date))
  
  expect_lt(spread_narrow, spread_wide)
})
