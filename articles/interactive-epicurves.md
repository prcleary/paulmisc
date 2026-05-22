# Interactive Epidemic Curves with Plotly

## Overview

[`geom_epicurve()`](https://prcleary.github.io/paulmisc/reference/geom_epicurve.md)
plots convert cleanly to interactive plotly widgets. Use
[`epicurve_ggplotly()`](https://prcleary.github.io/paulmisc/reference/epicurve_ggplotly.md)
instead of calling
[`ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html) directly:
it wraps the conversion with sensible defaults so you don’t have to
fight plotly afterwards. It:

- uses the auto-generated `text` aesthetic for hover labels;
- strips the unhelpful `"trace 0"`, `"trace 1"` … suffix from tooltips;
- re-injects the `subtitle` that
  [`ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html) would
  otherwise drop;
- suppresses plotly’s placeholder `"Aa"` legend for symbol-mode plots.

``` r

library(paulmisc)
library(ggplot2)
library(plotly)
#> 
#> Attaching package: 'plotly'
#> The following object is masked from 'package:ggplot2':
#> 
#>     last_plot
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following object is masked from 'package:graphics':
#> 
#>     layout

cases <- simulate_outbreak(n = 50, seed = 123, prop_missing = 0)
```

## Basic interactive plot

``` r

p <- ggplot(cases, aes(x = onset_date)) +
  geom_epicurve(fill = "steelblue") +
  labs(
    title = "Interactive Epidemic Curve",
    x = "Date of Onset", y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

epicurve_ggplotly(p)
```

Hover over any case to see the date and the case count for that day.

## By age group

``` r

cases$tooltip <- with(cases, paste0(
  "Case ", case_id, "<br>",
  "Date: ", format(onset_date, "%d %B %Y"), "<br>",
  "Age Group: ", age_group, "<br>",
  "Sex: ", sex, "<br>",
  "Setting: ", setting, "<br>",
  "Outcome: ", outcome
))

p <- ggplot(cases, aes(x = onset_date, fill = age_group, text = tooltip)) +
  geom_epicurve() +
  scale_fill_brewer(palette = "Set2", name = "Age Group",
                    na.translate = FALSE) +
  labs(
    title = "Interactive Epidemic Curve by Age Group",
    x = "Date of Onset", y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

epicurve_ggplotly(p)
```

`na.translate = FALSE` keeps unknown-age-group cases out of the legend.

## Faceted by setting

``` r

p <- ggplot(cases, aes(x = onset_date, fill = sex, text = tooltip)) +
  geom_epicurve() +
  scale_fill_manual(
    values = c("Female" = "#D55E00", "Male" = "#0072B2"),
    name = "Sex", na.translate = FALSE
  ) +
  facet_wrap(~ setting, ncol = 1) +
  labs(
    title = "Interactive Epidemic Curves by Setting",
    x = "Date of Onset", y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

epicurve_ggplotly(p)
```

Every panel has a labelled y-axis (even a single-case panel shows the
`0` and `1` breaks), and the bar width is always one day — sparse panels
can never produce a bar that spans multiple days.

## Custom HTML tooltips

``` r

cases$tooltip <- with(cases, paste0(
  "<b>Case ", case_id, "</b><br>",
  "<b>Date:</b> ", format(onset_date, "%d %B %Y"), "<br>",
  "<b>Demographics:</b> ", age_group, ", ", sex, "<br>",
  "<b>Setting:</b> ", setting, "<br>",
  "<b>Outcome:</b> ", outcome
))

p <- ggplot(cases, aes(x = onset_date, fill = outcome, text = tooltip)) +
  geom_epicurve() +
  scale_fill_brewer(palette = "Pastel1", name = "Outcome",
                    na.translate = FALSE) +
  labs(
    title = "Interactive Epidemic Curve by Outcome",
    x = "Date of Onset", y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

epicurve_ggplotly(p)
```

## Hourly outbreak

For sub-daily data, the auto-detected bar width is one hour. Make the
x-axis show hour-of-day instead of dates with
[`scale_x_datetime()`](https://ggplot2.tidyverse.org/reference/scale_date.html):

``` r

hourly_cases <- simulate_outbreak(
  n = 80, time_unit = "hourly", pattern = "continuous",
  date_range = 3, exposure = "2024-06-01",
  seed = 321, prop_missing = 0
)

p <- ggplot(hourly_cases, aes(x = onset_time, fill = sex)) +
  geom_epicurve() +
  scale_fill_manual(values = c(Female = "#D55E00", Male = "#0072B2"),
                    na.translate = FALSE) +
  scale_x_datetime(date_breaks = "6 hours",
                   date_labels = "%H:%M\n%d %b") +
  labs(
    title = "Hourly Epidemic Curve (Interactive)",
    x = "Time of Onset", y = "Number of Cases", fill = "Sex"
  ) +
  scale_y_epicurve() +
  theme_minimal()

epicurve_ggplotly(p)
```

## Large outbreak: auto column-chart mode + fill + facet

When any time point has more than `max_stack` cases, the plot
auto-switches to column-chart mode. Aesthetics and facets all still
work:

``` r

large_outbreak <- simulate_outbreak(
  n = 600, pattern = "continuous", date_range = 14,
  exposure = "2024-01-01", seed = 456, prop_missing = 0
)

p <- ggplot(large_outbreak, aes(x = onset_date, fill = age_group)) +
  geom_epicurve(max_stack = 20) +
  scale_fill_brewer(palette = "Set2", name = "Age group",
                    na.translate = FALSE) +
  labs(
    title = "Large Outbreak (Column Chart Mode)",
    subtitle = "Auto-switched, coloured by age group",
    x = "Date of Onset", y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

epicurve_ggplotly(p)
```

## Per-case tooltips for big plots

``` r

p <- ggplot(
  large_outbreak,
  aes(x = onset_date,
      text = paste0("<b>", case_id, "</b><br>",
                    format(onset_date, "%d %b %Y")))
) +
  geom_epicurve(fill = "coral", max_stack = 20) +
  labs(title = "Custom per-case tooltips", x = "Date", y = "Cases") +
  scale_y_epicurve() +
  theme_minimal()

epicurve_ggplotly(p)
```

## Symbols

For symbol mode the ggplot legend renders each key as the actual Unicode
glyph, but plotly’s legend would show a generic `"Aa"`. By default
[`epicurve_ggplotly()`](https://prcleary.github.io/paulmisc/reference/epicurve_ggplotly.md)
therefore hides the symbol legend and relies on the tooltip to
disambiguate categories — hover any symbol to see the sex.

``` r

symbol_cases <- simulate_outbreak(n = 30, seed = 789, prop_missing = 0)

sex_symbols <- c(Female = "\u2640", Male = "\u2642")

p <- ggplot(symbol_cases,
            aes(x = onset_date, colour = sex,
                text = paste0("Sex: ", sex, "<br>",
                              format(onset_date, "%d %b %Y")))) +
  geom_epicurve(symbol = sex_symbols, symbol_size = 6) +
  scale_colour_manual(
    values = c("Female" = "#D55E00", "Male" = "#0072B2"),
    name = "Sex", na.translate = FALSE
  ) +
  labs(
    title = "Interactive Epidemic Curve with Symbols",
    x = "Date of Onset", y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

epicurve_ggplotly(p)
```

## Timeline annotations

[`annotate_event()`](https://prcleary.github.io/paulmisc/reference/annotate_epicurve.md)
and
[`annotate_period()`](https://prcleary.github.io/paulmisc/reference/annotate_epicurve.md)
convert cleanly. Period labels self-clip to the visible x-range, so they
never run off the edge of the panel.

``` r

p <- ggplot(cases, aes(x = onset_date)) +
  geom_epicurve(fill = "steelblue") +
  annotate_period(
    date = as.Date("2024-05-28"),
    end_date = as.Date("2024-06-02"),
    label = "Exposure period",
    fill = "yellow", alpha = 0.25
  ) +
  annotate_event(
    date = as.Date("2024-06-05"),
    label = "Investigation",
    colour = "red"
  ) +
  labs(
    title = "Interactive Outbreak Timeline",
    subtitle = "Hover over cases for details",
    x = "Date", y = "Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

epicurve_ggplotly(p)
```
