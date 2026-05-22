# GeomEpicurveText: GeomText that knows about `text` and draws symbol keys

A thin subclass of
[ggplot2::GeomText](https://ggplot2.tidyverse.org/reference/Geom.html)
used in symbol mode. It declares the `text` aesthetic (so plotly
tooltips work without a warning) and overrides `draw_key` so that legend
keys display the actual case symbol at the requested size rather than
the default "a" glyph.

## Usage

``` r
GeomEpicurveText
```

## Format

A
[`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)
object inheriting from
[ggplot2::GeomText](https://ggplot2.tidyverse.org/reference/Geom.html).
