# Y-axis scale with integer breaks for epidemic curves

A convenience function that sets y-axis breaks to integers only, which
is appropriate for count data in epidemic curves. This ensures the
y-axis never shows decimal values like 2.5 cases.

## Usage

``` r
scale_y_epicurve(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).

## Value

A ggplot2 scale layer.

## Examples

``` r
library(ggplot2)
cases <- simulate_outbreak(n = 20)

ggplot(cases, aes(x = onset_date)) +
  geom_epicurve(fill = "steelblue") +
  scale_y_epicurve() +
  theme_minimal()

```
