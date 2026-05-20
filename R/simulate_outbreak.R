#' Simulate onset dates for a point-source disease outbreak
#'
#' Generates a small, realistic line list for a single point-source outbreak.
#' Incubation periods are drawn from a log-normal distribution (the classical
#' choice for many enteric and respiratory pathogens), producing the
#' right-skewed onset distribution typical of point-source events. A handful
#' of categorical case attributes (age group, sex, outcome, setting) are
#' simulated alongside, so that the resulting data frame can be used to
#' demonstrate or test plotting functions such as [geom_epicurve()].
#'
#' @param n Integer. Number of cases to simulate. Defaults to `20`.
#' @param exposure A `Date` (or object coercible to `Date`) giving the
#'   common point-source exposure date. Defaults to `"2024-06-01"`.
#' @param meanlog,sdlog Numeric. Parameters of the log-normal incubation
#'   period distribution on the log scale, passed to [stats::rlnorm()].
#'   The median incubation period is `exp(meanlog)` days, and `sdlog`
#'   controls the spread. Defaults (`meanlog = 1.6`, `sdlog = 0.45`) give
#'   a median of ~5 days with moderate spread, typical of many foodborne
#'   pathogens. For quick-onset diseases like norovirus, try
#'   `meanlog = 0.5` (median ~1.6 days). For slow-onset diseases like
#'   hepatitis A, try `meanlog = 3` (median ~20 days).
#' @param seed Optional integer used to seed the random number generator
#'   for reproducibility. Use `NULL` to leave the RNG state untouched.
#'
#' @return A data frame with one row per case and the columns
#'   `case_id`, `onset_date`, `age_group`, `sex`, `outcome`, and `setting`.
#'
#' @examples
#' cases <- simulate_outbreak()
#' head(cases)
#'
#' # A larger outbreak with a different exposure date
#' big <- simulate_outbreak(n = 100, exposure = as.Date("2025-03-15"))
#' range(big$onset_date)
#'
#' # Short incubation period (e.g., Salmonella)
#' # meanlog = 0.5 gives median of exp(0.5) = 1.6 days
#' fast <- simulate_outbreak(meanlog = 0.5, sdlog = 0.3)
#'
#' # Long incubation period (e.g., Hepatitis A)
#' # meanlog = 3 gives median of exp(3) = 20 days
#' slow <- simulate_outbreak(meanlog = 3, sdlog = 0.5)
#'
#' @importFrom stats rlnorm ave
#' @export
simulate_outbreak <- function(n        = 20,
                              exposure = as.Date("2024-06-01"),
                              meanlog  = 1.6,
                              sdlog    = 0.45,
                              seed     = 42) {
  if (!is.null(seed))
    set.seed(seed)

  exposure   <- as.Date(exposure)
  incubation <- pmax(1, round(stats::rlnorm(n, meanlog, sdlog)))
  onset      <- exposure + incubation

  data.frame(
    case_id    = sprintf("C%03d", seq_len(n)),
    onset_date = onset,
    age_group  = sample(
      c("Child", "Adult", "Elderly"),
      n,
      replace = TRUE,
      prob = c(0.30, 0.50, 0.20)
    ),
    sex        = sample(c("Female", "Male"), n, replace = TRUE),
    outcome    = sample(
      c("Recovered", "Hospitalised"),
      n,
      replace = TRUE,
      prob = c(0.75, 0.25)
    ),
    setting    = sample(c("Wedding A", "Wedding B"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}
