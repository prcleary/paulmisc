# Simulate onset dates for a point-source disease outbreak

Generates a small, realistic line list for a single point-source
outbreak. Incubation periods are drawn from a log-normal distribution
(the classical choice for many enteric and respiratory pathogens),
producing the right-skewed onset distribution typical of point-source
events. A handful of categorical case attributes (age group, sex,
outcome, setting) are simulated alongside, so that the resulting data
frame can be used to demonstrate or test plotting functions such as
[`geom_epicurve()`](https://prcleary.github.io/paulmisc/reference/geom_epicurve.md).

## Usage

``` r
simulate_outbreak(
  n = 20,
  exposure = as.Date("2024-06-01"),
  meanlog = 1.6,
  sdlog = 0.45,
  time_unit = c("daily", "hourly", "weekly"),
  pattern = c("point_source", "continuous"),
  date_range = 10,
  seed = 42
)
```

## Arguments

- n:

  Integer. Number of cases to simulate. Defaults to `20`.

- exposure:

  A `Date` (or object coercible to `Date`) giving the common
  point-source exposure date. Defaults to `"2024-06-01"`.

- meanlog, sdlog:

  Numeric. Parameters of the log-normal incubation period distribution
  on the log scale, passed to
  [`stats::rlnorm()`](https://rdrr.io/r/stats/Lognormal.html). The
  median incubation period is `exp(meanlog)` days, and `sdlog` controls
  the spread. Defaults (`meanlog = 1.6`, `sdlog = 0.45`) give a median
  of ~5 days with moderate spread, typical of many foodborne pathogens.
  For quick-onset diseases like norovirus, try `meanlog = 0.5` (median
  ~1.6 days). For slow-onset diseases like hepatitis A, try
  `meanlog = 3` (median ~20 days).

- time_unit:

  Character string specifying the time resolution of the outbreak data.
  Options: `"daily"` (default, returns Date), `"hourly"` (returns
  POSIXct with hour resolution), or `"weekly"` (returns Date aggregated
  to weeks).

- pattern:

  Character string specifying the outbreak pattern. Options:
  `"point_source"` (default, log-normal incubation from single exposure)
  or `"continuous"` (uniform distribution over a date range for ongoing
  transmission).

- date_range:

  Integer. For continuous pattern, the number of days/hours/weeks over
  which cases are uniformly distributed. Ignored for point_source
  pattern.

- seed:

  Optional integer used to seed the random number generator for
  reproducibility. Use `NULL` to leave the RNG state untouched.

## Value

A data frame with one row per case. For daily/weekly data: columns
`case_id`, `onset_date`, `age_group`, `sex`, `outcome`, and `setting`.
For hourly data: `onset_time` instead of `onset_date`.

## Examples

``` r
cases <- simulate_outbreak()
head(cases)
#>   case_id onset_date age_group    sex      outcome   setting
#> 1    C001 2024-06-10     Adult Female    Recovered Wedding B
#> 2    C002 2024-06-05     Adult   Male    Recovered Wedding B
#> 3    C003 2024-06-07     Adult   Male    Recovered Wedding A
#> 4    C004 2024-06-08   Elderly   Male    Recovered Wedding B
#> 5    C005 2024-06-07     Adult   Male Hospitalised Wedding A
#> 6    C006 2024-06-06   Elderly   Male    Recovered Wedding B

# A larger outbreak with a different exposure date
big <- simulate_outbreak(n = 100, exposure = as.Date("2025-03-15"))
range(big$onset_date)
#> [1] "2025-03-16" "2025-03-29"

# Short incubation period (e.g., Salmonella)
# meanlog = 0.5 gives median of exp(0.5) = 1.6 days
fast <- simulate_outbreak(meanlog = 0.5, sdlog = 0.3)

# Long incubation period (e.g., Hepatitis A)
# meanlog = 3 gives median of exp(3) = 20 days
slow <- simulate_outbreak(meanlog = 3, sdlog = 0.5)

# Hourly outbreak data for rapid response
hourly <- simulate_outbreak(n = 15, time_unit = "hourly", seed = 123)
head(hourly)
#>   case_id          onset_time age_group    sex   outcome   setting
#> 1    C001 2024-06-05 16:00:00     Adult   Male Recovered Wedding A
#> 2    C002 2024-06-06 02:00:00     Adult Female Recovered Wedding B
#> 3    C003 2024-06-11 11:00:00   Elderly Female Recovered Wedding B
#> 4    C004 2024-06-06 21:00:00     Adult   Male Recovered Wedding A
#> 5    C005 2024-06-07 00:00:00     Adult Female Recovered Wedding A
#> 6    C006 2024-06-12 18:00:00     Child Female Recovered Wedding B

# Weekly surveillance data
weekly <- simulate_outbreak(n = 20, time_unit = "weekly", seed = 456)
head(weekly)
#>   case_id onset_date age_group    sex      outcome   setting
#> 1    C001 2024-06-03     Adult Female    Recovered Wedding B
#> 2    C002 2024-06-03     Adult Female    Recovered Wedding B
#> 3    C003 2024-06-03     Adult   Male    Recovered Wedding B
#> 4    C004 2024-06-03     Adult   Male Hospitalised Wedding A
#> 5    C005 2024-06-03     Adult Female    Recovered Wedding B
#> 6    C006 2024-06-03     Adult Female Hospitalised Wedding B

# Large continuous outbreak (not point-source)
large <- simulate_outbreak(n = 300, pattern = "continuous", date_range = 14, seed = 789)
table(large$onset_date)
#> 
#> 2024-06-01 2024-06-02 2024-06-03 2024-06-04 2024-06-05 2024-06-06 2024-06-07 
#>         16         24         21         24         19         21         18 
#> 2024-06-08 2024-06-09 2024-06-10 2024-06-11 2024-06-12 2024-06-13 2024-06-14 
#>         19         13         23         23         17         18         23 
#> 2024-06-15 
#>         21 
```
