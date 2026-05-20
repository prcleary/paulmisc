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
  [`stats::rlnorm()`](https://rdrr.io/r/stats/Lognormal.html). Defaults
  (`meanlog = 1.6`, `sdlog = 0.45`) give a median incubation period of
  roughly five days.

- seed:

  Optional integer used to seed the random number generator for
  reproducibility. Use `NULL` to leave the RNG state untouched.

## Value

A data frame with one row per case and the columns `case_id`,
`onset_date`, `age_group`, `sex`, `outcome`, and `setting`.

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
```
