# Coordinate system for epidemic curves

A thin
[ggplot2::CoordCartesian](https://ggplot2.tidyverse.org/reference/Coord.html)
subclass that automatically constrains the *panel* aspect ratio when the
x-axis is a datetime (POSIXct) so that case rectangles look
approximately square instead of tall, narrow sticks on typical figure
dimensions. For Date / numeric x axes it behaves exactly like
[`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)
(no aspect is imposed).

## Usage

``` r
coord_epicurve(clip = "on")
```

## Arguments

- clip:

  Should drawing be clipped to the panel extent? Passed through to
  [ggplot2::CoordCartesian](https://ggplot2.tidyverse.org/reference/Coord.html).
  Defaults to `"on"`.

## Value

A ggproto
[ggplot2::CoordCartesian](https://ggplot2.tidyverse.org/reference/Coord.html)
subclass.

## Details

[`geom_epicurve()`](https://prcleary.github.io/paulmisc/reference/geom_epicurve.md)
adds `coord_epicurve()` automatically; users can opt out with
`auto_aspect = FALSE`, or override by adding their own coordinate system
(e.g.
[`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html))
after the geom.
