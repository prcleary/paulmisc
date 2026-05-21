test_that("annotate_event creates annotation object", {
  library(ggplot2)
  
  # Create annotation
  event <- annotate_event(
    date = as.Date("2024-06-05"),
    label = "Test Event",
    colour = "red"
  )
  
  # Should return an S3 annotation object
  expect_s3_class(event, "epicurve_event_annotation")
  expect_s3_class(event, "epicurve_annotation")
  expect_equal(event$label, "Test Event")
  expect_equal(event$colour, "red")
})

test_that("annotate_event works with Date", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 123)
  
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve() +
      annotate_event(
        date = as.Date("2024-06-05"),
        label = "Event"
      )
    ggplot_build(p)
  })
})

test_that("annotate_event works with POSIXct", {
  library(ggplot2)
  
  # Create hourly data
  start_time <- as.POSIXct("2024-06-01 08:00:00", tz = "UTC")
  hourly_cases <- data.frame(
    onset_time = start_time + 3600 * c(0, 1, 1, 2, 3, 3, 4)
  )
  
  expect_no_error({
    p <- ggplot(hourly_cases, aes(x = onset_time)) +
      geom_epicurve() +
      annotate_event(
        date = start_time + 3600 * 2,
        label = "Event"
      )
    ggplot_build(p)
  })
})

test_that("annotate_event accepts American spelling of color", {
  library(ggplot2)
  
  # Should not error with color instead of colour
  expect_no_error({
    event <- annotate_event(
      date = as.Date("2024-06-05"),
      label = "Test",
      color = "blue"  # American spelling
    )
  })
})

test_that("annotate_event accepts custom label positioning", {
  library(ggplot2)
  
  # Test with numeric position
  expect_no_error({
    event <- annotate_event(
      date = as.Date("2024-06-05"),
      label = "Test",
      label_y = 10
    )
  })
  
  # Test with "top" keyword
  expect_no_error({
    event <- annotate_event(
      date = as.Date("2024-06-05"),
      label = "Test",
      label_y = "top"
    )
  })
  
  # Test with "bottom" keyword
  expect_no_error({
    event <- annotate_event(
      date = as.Date("2024-06-05"),
      label = "Test",
      label_y = "bottom"
    )
  })
})

test_that("annotate_period creates annotation object", {
  library(ggplot2)
  
  # Create annotation
  period <- annotate_period(
    date = as.Date("2024-05-25"),
    end_date = as.Date("2024-06-01"),
    label = "Test Period"
  )
  
  # Should return an S3 annotation object
  expect_s3_class(period, "epicurve_period_annotation")
  expect_s3_class(period, "epicurve_annotation")
  expect_equal(period$label, "Test Period")
})

test_that("annotate_period requires end_date", {
  library(ggplot2)
  
  # Should error without end_date
  expect_error({
    annotate_period(
      date = as.Date("2024-06-01"),
      label = "Test"
    )
  }, "end_date is required")
})

test_that("annotate_period works with Date", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 456)
  
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve() +
      annotate_period(
        date = as.Date("2024-06-01"),
        end_date = as.Date("2024-06-05"),
        label = "Period"
      )
    ggplot_build(p)
  })
})

test_that("annotate_period works with POSIXct", {
  library(ggplot2)
  
  # Create hourly data
  start_time <- as.POSIXct("2024-06-01 08:00:00", tz = "UTC")
  hourly_cases <- data.frame(
    onset_time = start_time + 3600 * c(0, 1, 1, 2, 3, 3, 4)
  )
  
  expect_no_error({
    p <- ggplot(hourly_cases, aes(x = onset_time)) +
      geom_epicurve() +
      annotate_period(
        date = start_time,
        end_date = start_time + 3600 * 3,
        label = "Period"
      )
    ggplot_build(p)
  })
})

test_that("annotate_period accepts American spelling of color", {
  library(ggplot2)
  
  # Should not error with color instead of colour
  expect_no_error({
    period <- annotate_period(
      date = as.Date("2024-06-01"),
      end_date = as.Date("2024-06-05"),
      label = "Test",
      color = "blue"  # American spelling
    )
  })
})

test_that("multiple annotations work together", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 30, seed = 789)
  
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve() +
      annotate_period(
        date = as.Date("2024-05-28"),
        end_date = as.Date("2024-06-02"),
        label = "Period 1",
        fill = "yellow"
      ) +
      annotate_event(
        date = as.Date("2024-06-03"),
        label = "Event 1",
        colour = "red"
      ) +
      annotate_event(
        date = as.Date("2024-06-07"),
        label = "Event 2",
        colour = "blue"
      )
    ggplot_build(p)
  })
})

test_that("annotations work with faceted plots", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 40, seed = 101)
  
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve() +
      facet_wrap(~ setting) +
      annotate_event(
        date = as.Date("2024-06-05"),
        label = "Event"
      )
    ggplot_build(p)
  })
})

test_that("annotate_period handles custom alpha and fill", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 202)
  
  expect_no_error({
    p <- ggplot(cases, aes(x = onset_date)) +
      geom_epicurve() +
      annotate_period(
        date = as.Date("2024-06-01"),
        end_date = as.Date("2024-06-05"),
        label = "Custom period",
        fill = "coral",
        alpha = 0.5
      )
    ggplot_build(p)
  })
})

test_that("annotate_event customization parameters work", {
  library(ggplot2)
  
  event <- annotate_event(
    date = as.Date("2024-06-05"),
    label = "Custom",
    colour = "purple",
    linetype = "dotted",
    linewidth = 1.5,
    label_size = 4
  )
  
  # Check that parameters are stored on the annotation object
  expect_equal(event$linetype, "dotted")
  expect_equal(event$colour, "purple")
  expect_equal(event$linewidth, 1.5)
  expect_equal(event$label_size, 4)
})
