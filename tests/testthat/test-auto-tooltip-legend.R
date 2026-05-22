test_that("default tooltip is populated with count for date x", {
  cases <- simulate_outbreak(n = 25, seed = 11, prop_missing = 0)
  p <- ggplot2::ggplot(cases, ggplot2::aes(x = onset_date)) +
    geom_epicurve()
  built <- ggplot2::ggplot_build(p)
  d <- built$data[[1]]
  expect_true("text" %in% names(d))
  expect_true(any(nzchar(d$text)))
})

test_that("user-supplied text aesthetic is preserved", {
  cases <- simulate_outbreak(n = 10, seed = 12, prop_missing = 0)
  cases$tip <- paste0("Case ", cases$case_id)
  p <- ggplot2::ggplot(cases, ggplot2::aes(x = onset_date, text = tip)) +
    geom_epicurve()
  built <- ggplot2::ggplot_build(p)
  d <- built$data[[1]]
  expect_true(all(grepl("^Case ", d$text)))
})

test_that("named symbol returns list with guides element", {
  cases <- simulate_outbreak(n = 10, seed = 13, prop_missing = 0)
  sym <- c(Female = "\u2640", Male = "\u2642")
  res <- geom_epicurve(symbol = sym, symbol_size = 5)
  expect_type(res, "list")
  expect_true(any(vapply(res, inherits, logical(1), what = "Guides")))
})

test_that("scalar symbol returns a list including a Guides element", {
  res <- geom_epicurve(symbol = "\u25CF")
  expect_type(res, "list")
  expect_true(any(vapply(res, inherits, logical(1), what = "Layer")))
  expect_true(any(vapply(res, inherits, logical(1), what = "Guides")))
})
