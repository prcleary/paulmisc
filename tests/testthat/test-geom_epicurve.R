test_that("geom_epicurve creates a ggplot layer", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 123)
  
  p <- ggplot(cases, aes(x = onset_date)) +
    geom_epicurve()
  
  # Check that it's a ggplot object
  expect_s3_class(p, "ggplot")
  
  # Check that the layer was added
  expect_equal(length(p$layers), 1)
})

test_that("geom_epicurve uses StatEpicurve by default", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 456)
  
  p <- ggplot(cases, aes(x = onset_date)) +
    geom_epicurve()
  
  # Check the stat class
  expect_s3_class(p$layers[[1]]$stat, "StatEpicurve")
})

test_that("geom_epicurve uses GeomRect", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 789)
  
  p <- ggplot(cases, aes(x = onset_date)) +
    geom_epicurve()
  
  # Check the geom class - now uses standard GeomRect for plotly compatibility
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
})

test_that("stat_epicurve creates a ggplot layer", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 111)
  
  p <- ggplot(cases, aes(x = onset_date)) +
    stat_epicurve()
  
  # Check that it's a ggplot object
  expect_s3_class(p, "ggplot")
  
  # Check that the layer was added
  expect_equal(length(p$layers), 1)
})

test_that("stat_epicurve uses GeomRect by default", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 222)
  
  p <- ggplot(cases, aes(x = onset_date)) +
    stat_epicurve()
  
  # Check the geom class - now uses standard GeomRect for plotly compatibility
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
})

test_that("StatEpicurve computes stacking positions correctly", {
  library(ggplot2)
  
  # Create simple test data with known duplicates
  test_data <- data.frame(
    x = c(1, 1, 1, 2, 2, 3),
    group = 1
  )
  
  # Compute panel
  result <- StatEpicurve$compute_panel(test_data, width = 0.9, height = 0.9)
  
  # Check y values (stacking index)
  # Padding rows with y=0 are filtered out in compute_panel
  expect_equal(result$y, c(1, 2, 3, 1, 2, 1))
  
  # Check that xmin, xmax, ymin, ymax are computed for geom_rect
  expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% names(result)))
})

test_that("StatEpicurve handles single observation per x", {
  library(ggplot2)
  
  test_data <- data.frame(
    x = c(1, 2, 3, 4),
    group = 1
  )
  
  result <- StatEpicurve$compute_panel(test_data, width = 0.9, height = 0.9)
  
  # Each date has only one case, so all y values should be 1
  # Padding rows with y=0 are filtered out
  expect_equal(result$y, c(1, 1, 1, 1))
})

test_that("StatEpicurve respects grouping", {
  library(ggplot2)
  
  # Data with different groups
  test_data <- data.frame(
    x = c(1, 1, 1, 1),
    group = c(1, 1, 2, 2)
  )
  
  # Sort by x and group as the stat does
  test_data <- test_data[order(test_data$x, test_data$group), , drop = FALSE]
  
  result <- StatEpicurve$compute_panel(test_data, width = 0.9, height = 0.9)
  
  # Should stack within each x value
  # Padding rows with y=0 are filtered out
  expect_equal(result$y, c(1, 2, 3, 4))
})

test_that("geom_epicurve accepts width parameter", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 333)
  
  # Should not error with custom width
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve(width = 0.5)
  })
})

test_that("geom_epicurve accepts height parameter", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 444)
  
  # Should not error with custom height
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve(height = 0.7)
  })
})

test_that("geom_epicurve works with fill aesthetic", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 30, seed = 555)
  
  # Should not error with fill mapping
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date, fill = age_group)) +
      geom_epicurve()
  })
})

test_that("geom_epicurve works with colour aesthetic", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 30, seed = 666)
  
  # Should not error with colour mapping
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date, colour = sex)) +
      geom_epicurve()
  })
})

test_that("geom_epicurve works with faceting", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 40, seed = 777)
  
  # Should not error with facets
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve() +
      facet_wrap(~ setting)
  })
})

test_that("geom_epicurve can be built", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 888)
  
  p <- ggplot(cases, aes(x = onset_date)) +
    geom_epicurve()
  
  # Building the plot should not error
  expect_no_error(ggplot_build(p))
})

test_that("geom_epicurve produces data in built plot", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 25, seed = 999)
  
  p <- ggplot(cases, aes(x = onset_date)) +
    geom_epicurve()
  
  built <- ggplot_build(p)
  
  # Should have data in the built plot
  # Note: May have extra dummy row for y-axis range, so check >= 25
  expect_true(nrow(built$data[[1]]) >= 25)
  
  # Should have x and y information
  expect_true("x" %in% names(built$data[[1]]) || "xmin" %in% names(built$data[[1]]))
  expect_true("y" %in% names(built$data[[1]]) || "ymin" %in% names(built$data[[1]]))
})

test_that("GeomEpicurve has required aesthetics", {
  expect_equal(GeomEpicurve$required_aes, c("x", "y"))
})

test_that("StatEpicurve has required aesthetics", {
  expect_equal(StatEpicurve$required_aes, "x")
})

test_that("GeomEpicurve has sensible default aesthetics", {
  defaults <- GeomEpicurve$default_aes
  
  expect_true("fill" %in% names(defaults))
  expect_true("colour" %in% names(defaults))
  expect_true("linewidth" %in% names(defaults))
  expect_true("linetype" %in% names(defaults))
  expect_true("alpha" %in% names(defaults))
})

test_that("geom_epicurve works with theme customization", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 101)
  
  # Should not error with theme
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve() +
      theme_minimal()
  })
})

test_that("geom_epicurve handles na.rm parameter", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 202)
  cases$onset_date[c(1, 5)] <- NA
  
  # Should handle NAs without error when na.rm = TRUE
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve(na.rm = TRUE)
    ggplot_build(p)
  })
})

test_that("geom_epicurve works with scales", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 30, seed = 303)
  
  # Should work with date scale
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date, fill = age_group)) +
      geom_epicurve() +
      scale_x_date(date_labels = "%b %d") +
      scale_fill_brewer(palette = "Set2")
    ggplot_build(p)
  })
})

test_that("geom_epicurve can be combined with other layers", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 404)
  
  # Should work with multiple layers
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve() +
      geom_vline(xintercept = as.Date("2024-06-05"), linetype = "dashed")
    ggplot_build(p)
  })
})

test_that("stat_epicurve works independently", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 505)
  
  # stat_epicurve with different geom
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      stat_epicurve(geom = "point")
    ggplot_build(p)
  })
})

# ============================================================================
# Time Period Flexibility Tests
# ============================================================================

test_that("geom_epicurve works with hourly POSIXct data", {
  library(ggplot2)
  
  # Create hourly data
  start_time <- as.POSIXct("2024-06-01 08:00:00", tz = "UTC")
  hourly_cases <- data.frame(
    onset_time = start_time + 3600 * c(0, 1, 1, 2, 3, 3, 3, 4, 5),
    case_id = 1:9
  )
  
  # Should work without specifying width
  expect_no_error({
    p <- ggplot(hourly_cases, aes(x = onset_time)) +
      geom_epicurve()
    built <- ggplot_build(p)
  })
  
  # Check that plot builds and has data
  p <- ggplot(hourly_cases, aes(x = onset_time)) +
    geom_epicurve()
  built <- ggplot_build(p)
  expect_true(nrow(built$data[[1]]) >= 9)
})

test_that("geom_epicurve auto-detects width for hourly data", {
  library(ggplot2)
  
  # Create hourly data
  start_time <- as.POSIXct("2024-06-01 08:00:00", tz = "UTC")
  hourly_data <- data.frame(
    onset_time = start_time + 3600 * seq(0, 23),  # 24 hours
    case_id = 1:24
  )
  
  # Test via full ggplot pipeline
  p <- ggplot(hourly_data, aes(x = onset_time)) +
    geom_epicurve()  # width = NULL triggers auto-detection
  
  built <- ggplot_build(p)
  
  # Check that xmin and xmax are computed appropriately
  # For hourly data, width should be around 3600 * 0.9 = 3240 seconds
  expect_true(all(!is.na(built$data[[1]]$xmin)))
  expect_true(all(!is.na(built$data[[1]]$xmax)))
  
  # Check that width is reasonable for hourly data (within 1 hour range)
  actual_width <- built$data[[1]]$xmax[1] - built$data[[1]]$xmin[1]
  expect_true(actual_width > 1800 && actual_width < 5400)  # 0.5 to 1.5 hours
})

test_that("geom_epicurve works with weekly Date data", {
  library(ggplot2)
  
  # Create weekly data (7-day intervals)
  weekly_cases <- data.frame(
    epi_week = as.Date("2024-01-01") + 7 * c(0, 1, 1, 2, 2, 2, 3, 4),
    case_id = 1:8
  )
  
  # Should work without specifying width
  expect_no_error({
    p <- ggplot(weekly_cases, aes(x = epi_week)) +
      geom_epicurve()
    built <- ggplot_build(p)
  })
  
  # Check that plot builds and has data
  p <- ggplot(weekly_cases, aes(x = epi_week)) +
    geom_epicurve()
  built <- ggplot_build(p)
  expect_true(nrow(built$data[[1]]) >= 8)
})

test_that("geom_epicurve auto-detects width for weekly data", {
  library(ggplot2)
  
  # Create weekly data
  weekly_data <- data.frame(
    epi_week = as.Date("2024-01-01") + 7 * seq(0, 9),  # 10 weeks
    case_id = 1:10
  )
  
  # Test via full ggplot pipeline
  p <- ggplot(weekly_data, aes(x = epi_week)) +
    geom_epicurve()  # width = NULL triggers auto-detection
  
  built <- ggplot_build(p)
  
  # Width should be auto-detected from weekly data (~7 * 0.9 = 6.3 days)
  expect_true(all(!is.na(built$data[[1]]$xmin)))
  expect_true(all(!is.na(built$data[[1]]$xmax)))
  
  # Check that width is reasonable for weekly data (5-9 days)
  actual_width <- as.numeric(built$data[[1]]$xmax[1] - built$data[[1]]$xmin[1])
  expect_true(actual_width > 5 && actual_width < 9)
})

test_that("geom_epicurve respects explicit width parameter with POSIXct", {
  library(ggplot2)
  
  # Create hourly data
  start_time <- as.POSIXct("2024-06-01 08:00:00", tz = "UTC")
  hourly_data <- data.frame(
    onset_time = start_time + 3600 * seq(0, 5),
    case_id = 1:6
  )
  
  # Specify explicit width (in seconds for POSIXct)
  custom_width <- 1800  # 30 minutes
  p <- ggplot(hourly_data, aes(x = onset_time)) +
    geom_epicurve(width = custom_width)
  
  built <- ggplot_build(p)
  
  # Should use the specified width
  actual_width <- built$data[[1]]$xmax[1] - built$data[[1]]$xmin[1]
  expect_equal(actual_width, custom_width)
})

test_that("geom_epicurve respects explicit width parameter with Date", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 606)
  
  # Build with explicit width
  custom_width <- 0.5
  p <- ggplot(cases, aes(x = onset_date)) +
    geom_epicurve(width = custom_width)
  
  built <- ggplot_build(p)
  
  # Should use the specified width
  actual_width <- as.numeric(built$data[[1]]$xmax[1] - built$data[[1]]$xmin[1])
  expect_equal(actual_width, custom_width)
})

test_that("detect_epicurve_width handles daily Date data", {
  # Daily dates
  daily_dates <- as.Date("2024-01-01") + seq(0, 30)
  width <- paulmisc:::detect_epicurve_width(daily_dates)
  
  # Should return ~0.9 for daily data
  expect_true(width > 0.8 && width < 1.0)
})

test_that("detect_epicurve_width handles hourly POSIXct data", {
  # Hourly timestamps
  start <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  hourly_times <- start + 3600 * seq(0, 48)
  width <- paulmisc:::detect_epicurve_width(hourly_times)
  
  # Should return ~3240 seconds (3600 * 0.9) for hourly data
  expect_true(width > 2700 && width < 4000)
})

test_that("detect_epicurve_width handles weekly Date data", {
  # Weekly dates
  weekly_dates <- as.Date("2024-01-01") + 7 * seq(0, 12)
  width <- paulmisc:::detect_epicurve_width(weekly_dates)
  
  # Should return ~6.3 days (7 * 0.9) for weekly data
  expect_true(width > 5.5 && width < 7.5)
})

test_that("detect_epicurve_width handles numeric data", {
  # Generic numeric x-axis
  numeric_x <- seq(10, 100, by = 5)
  width <- paulmisc:::detect_epicurve_width(numeric_x)
  
  # Should return ~4.5 (5 * 0.9) for step size of 5
  expect_true(width > 4.0 && width < 5.0)
})

test_that("detect_epicurve_width handles small datasets", {
  # Single point
  single_date <- as.Date("2024-01-01")
  width <- paulmisc:::detect_epicurve_width(single_date)
  
  # Should return default 0.9
  expect_equal(width, 0.9)
})

test_that("detect_epicurve_width handles data with NAs", {
  # Data with missing values
  dates_with_na <- c(as.Date("2024-01-01") + seq(0, 10), NA, NA)
  width <- paulmisc:::detect_epicurve_width(dates_with_na)
  
  # Should handle NAs gracefully and detect daily pattern
  expect_true(width > 0.8 && width < 1.0)
})
