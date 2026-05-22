# GeomEpicurveRect: GeomRect that knows about the `text` aesthetic

A thin subclass of
[ggplot2::GeomRect](https://ggplot2.tidyverse.org/reference/Geom.html)
that declares the `text` aesthetic. This is needed so that the tooltip
text added by
[StatEpicurve](https://prcleary.github.io/paulmisc/reference/StatEpicurve.md)
(or supplied by the user via `aes(text = ...)`) is propagated through to
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
without ggplot2 emitting an "Ignoring unknown aesthetics: text" warning
during static rendering.

## Usage

``` r
GeomEpicurveRect
```

## Format

A
[`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)
object inheriting from
[ggplot2::GeomRect](https://ggplot2.tidyverse.org/reference/Geom.html).
