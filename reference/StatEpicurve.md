# StatEpicurve: ggproto for stacking individual cases

Computes, within each panel, a stacking index `y` for every case sharing
the same `x` value. Exported so that other developers can extend it.

## Usage

``` r
StatEpicurve
```

## Format

A
[`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)
object inheriting from
[ggplot2::Stat](https://ggplot2.tidyverse.org/reference/Stat.html).
