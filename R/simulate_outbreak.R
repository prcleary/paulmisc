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
#' @param time_unit Character string specifying the time resolution of the
#'   outbreak data. Options: `"daily"` (default, returns Date), `"hourly"`
#'   (returns POSIXct with hour resolution), or `"weekly"` (returns Date
#'   aggregated to weeks).
#' @param pattern Character string specifying the outbreak pattern. Options:
#'   `"point_source"` (default, log-normal incubation from single exposure)
#'   or `"continuous"` (uniform distribution over a date range for ongoing
#'   transmission).
#' @param date_range Integer. For continuous pattern, the number of days/hours/weeks
#'   over which cases are uniformly distributed. Ignored for point_source pattern.
#' @param prop_missing Numeric in `[0, 1]`. Approximate proportion of values
#'   that should be set to `NA` in each non-ID column (including the onset
#'   date/time and each categorical attribute). Defaults to `0.05` so that
#'   demonstration data exercises the missing-data code paths in
#'   [geom_epicurve()]. Set to `0` to disable.
#' @param seed Optional integer used to seed the random number generator
#'   for reproducibility. Use `NULL` to leave the RNG state untouched.
#'
#' @return A data frame with one row per case. For daily/weekly data: columns
#'   `case_id`, `onset_date`, `age_group`, `sex`, `outcome`, and `setting`.
#'   For hourly data: `onset_time` instead of `onset_date`.
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
#' # Hourly outbreak data for rapid response
#' hourly <- simulate_outbreak(n = 15, time_unit = "hourly", seed = 123)
#' head(hourly)
#'
#' # Weekly surveillance data
#' weekly <- simulate_outbreak(n = 20, time_unit = "weekly", seed = 456)
#' head(weekly)
#'
#' # Large continuous outbreak (not point-source)
#' large <- simulate_outbreak(n = 300, pattern = "continuous", date_range = 14, seed = 789)
#' table(large$onset_date)
#'
#' @importFrom stats rlnorm ave runif
#' @export
simulate_outbreak <- function(n            = 20,
                              exposure     = as.Date("2024-06-01"),
                              meanlog      = 1.6,
                              sdlog        = 0.45,
                              time_unit    = c("daily", "hourly", "weekly"),
                              pattern      = c("point_source", "continuous"),
                              date_range   = 10,
                              prop_missing = 0.05,
                              seed         = 42) {
  if (!is.null(seed))
    set.seed(seed)

  if (!is.numeric(prop_missing) || length(prop_missing) != 1 ||
      is.na(prop_missing) || prop_missing < 0 || prop_missing > 1) {
    stop("`prop_missing` must be a single number in [0, 1]", call. = FALSE)
  }

  time_unit <- match.arg(time_unit)
  pattern <- match.arg(pattern)
  
  # Generate onset times based on pattern
  if (pattern == "point_source") {
    # Point-source: log-normal incubation from single exposure
    exposure_base <- as.Date(exposure)
    incubation <- pmax(1, round(stats::rlnorm(n, meanlog, sdlog)))
    onset_dates <- exposure_base + incubation
  } else {
    # Continuous: uniform distribution over date_range
    exposure_base <- as.Date(exposure)
    onset_dates <- exposure_base + sample(0:date_range, n, replace = TRUE)
  }
  
  # Convert to appropriate time unit
  if (time_unit == "hourly") {
    # Convert dates to POSIXct with random hours
    base_time <- as.POSIXct(paste(exposure, "08:00:00"), tz = "UTC")
    if (pattern == "point_source") {
      # Add incubation in hours
      onset_times <- base_time + 3600 * (incubation * 24 + sample(0:23, n, replace = TRUE))
    } else {
      # Uniform over date_range in hours
      hours_range <- date_range * 24
      onset_times <- base_time + 3600 * sample(0:hours_range, n, replace = TRUE)
    }
    
    result <- data.frame(
      case_id    = sprintf("C%03d", seq_len(n)),
      onset_time = onset_times,
      stringsAsFactors = FALSE
    )
  } else if (time_unit == "weekly") {
    # Aggregate to week starts (nearest Monday or first of period)
    week_start <- as.Date(exposure) - as.numeric(format(as.Date(exposure), "%u")) + 1
    onset_weeks <- week_start + 7 * floor(as.numeric(onset_dates - week_start) / 7)
    
    result <- data.frame(
      case_id    = sprintf("C%03d", seq_len(n)),
      onset_date = onset_weeks,
      stringsAsFactors = FALSE
    )
  } else {
    # Daily (default)
    result <- data.frame(
      case_id    = sprintf("C%03d", seq_len(n)),
      onset_date = onset_dates,
      stringsAsFactors = FALSE
    )
  }
  
  # Add demographic attributes (same for all time units)
  result$age_group <- sample(
    c("Child", "Adult", "Elderly"),
    n,
    replace = TRUE,
    prob = c(0.30, 0.50, 0.20)
  )
  result$sex <- sample(c("Female", "Male"), n, replace = TRUE)
  result$outcome <- sample(
    c("Recovered", "Hospitalised"),
    n,
    replace = TRUE,
    prob = c(0.75, 0.25)
  )
  result$setting <- sample(c("Wedding A", "Wedding B"), n, replace = TRUE)

  # Inject a small proportion of missing values into each non-ID column.
  # This mirrors real outbreak line lists where late-arriving information
  # leaves some fields blank, and gives downstream tools (e.g. the
  # epicurve geom) something to exercise their NA-handling on.
  if (prop_missing > 0) {
    miss_cols <- setdiff(names(result), "case_id")
    for (col in miss_cols) {
      idx <- which(stats::runif(n) < prop_missing)
      if (length(idx) > 0) {
        result[[col]][idx] <- NA
      }
    }
  }

  result
}
