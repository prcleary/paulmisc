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

test_that("geom_epicurve uses GeomEpicurve", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 789)
  
  p <- ggplot(cases, aes(x = onset_date)) +
    geom_epicurve()
  
  # Check the geom class
  expect_s3_class(p$layers[[1]]$geom, "GeomEpicurve")
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

test_that("stat_epicurve uses GeomEpicurve by default", {
  library(ggplot2)
  
  cases <- simulate_outbreak(n = 20, seed = 222)
  
  p <- ggplot(cases, aes(x = onset_date)) +
    stat_epicurve()
  
  # Check the geom class
  expect_s3_class(p$layers[[1]]$geom, "GeomEpicurve")
})

test_that("StatEpicurve computes stacking positions correctly", {
  library(ggplot2)
  
  # Create simple test data with known duplicates
  test_data <- data.frame(
    x = c(1, 1, 1, 2, 2, 3),
    group = 1
  )
  
  # Compute panel
  result <- StatEpicurve$compute_panel(test_data)
  
  # Check y values (stacking index)
  expect_equal(result$y, c(1, 2, 3, 1, 2, 1))
})

test_that("StatEpicurve handles single observation per x", {
  library(ggplot2)
  
  test_data <- data.frame(
    x = c(1, 2, 3, 4),
    group = 1
  )
  
  result <- StatEpicurve$compute_panel(test_data)
  
  # All y values should be 1
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
  
  result <- StatEpicurve$compute_panel(test_data)
  
  # Should stack within each x value
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
  
  # Should have data in the built plot with correct number of rows
  expect_equal(nrow(built$data[[1]]), 25)
  
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
