# GeomEpicurve: ggproto for drawing case squares

Draws each case as a rectangle by delegating to
[ggplot2::GeomRect](https://ggplot2.tidyverse.org/reference/Geom.html).
Exported so that other developers can extend it.

## Usage

``` r
GeomEpicurve
```

## Format

A
[`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)
object inheriting from
[ggplot2::Geom](https://ggplot2.tidyverse.org/reference/Geom.html).
