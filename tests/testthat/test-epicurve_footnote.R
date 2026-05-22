test_that("epicurve_footnote returns a labs object", {
  cases <- simulate_outbreak(n = 20, seed = 1, prop_missing = 0)
  ftn <- epicurve_footnote(cases)
  expect_true(inherits(ftn, "labels") || inherits(ftn, "ggplot2::labels"))
  expect_true("caption" %in% names(ftn))
  expect_true(nzchar(ftn$caption))
})

test_that("epicurve_footnote reports zero missing when prop_missing = 0", {
  cases <- simulate_outbreak(n = 30, seed = 2, prop_missing = 0)
  ftn <- epicurve_footnote(cases, show_timestamp = FALSE)
  expect_match(ftn$caption, "0", fixed = TRUE)
})

test_that("epicurve_footnote reports non-zero missing when NAs present", {
  cases <- simulate_outbreak(n = 80, seed = 3, prop_missing = 0.3)
  ftn <- epicurve_footnote(cases, show_timestamp = FALSE)
  expect_match(ftn$caption, "%")
})

test_that("epicurve_footnote can disable missing summary", {
  cases <- simulate_outbreak(n = 20, seed = 4, prop_missing = 0)
  ftn <- epicurve_footnote(
    cases, show_missing = FALSE, show_timestamp = FALSE,
    extra = "Source: test"
  )
  expect_match(ftn$caption, "Source: test", fixed = TRUE)
})

test_that("epicurve_footnote respects columns argument", {
  df <- data.frame(a = c(1, NA, 3), b = c(NA, NA, 3))
  ftn_a <- epicurve_footnote(df, columns = "a", show_timestamp = FALSE)
  ftn_b <- epicurve_footnote(df, columns = "b", show_timestamp = FALSE)
  expect_true(nzchar(ftn_a$caption))
  expect_true(nzchar(ftn_b$caption))
})

test_that("epicurve_footnote can be added to a ggplot", {
  cases <- simulate_outbreak(n = 15, seed = 5, prop_missing = 0)
  p <- ggplot2::ggplot(cases, ggplot2::aes(x = onset_date)) +
    geom_epicurve() +
    epicurve_footnote(cases)
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$labels$caption))
})
