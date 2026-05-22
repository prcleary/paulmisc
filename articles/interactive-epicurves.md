# Interactive Epidemic Curves with Plotly

## Overview

The
[`geom_epicurve()`](https://prcleary.github.io/paulmisc/reference/geom_epicurve.md)
function works seamlessly with [plotly](https://plotly.com/r/) to create
interactive epidemic curves with custom tooltips. This vignette
demonstrates:

- **Basic interactivity**: Converting static plots to interactive
  visualisations
- **Custom tooltips**: HTML-formatted hover information
- **Time periods**: Hourly, daily, and weekly epidemic curves
- **Large outbreaks**: Automatic column chart mode with tooltips
- **Symbols**: Using Unicode symbols and emoji with interactivity
- **Annotations**: Adding timeline context (best for static plots)

Simply create your plot with ggplot2 and convert it using
[`ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html).

## Basic Interactive Example

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

# Generate example outbreak data
cases <- simulate_outbreak(n = 50, seed = 123)
```

First, let’s create a basic interactive epidemic curve:

``` r

# Create ggplot with text aesthetic for tooltips
p <- ggplot(cases, aes(x = onset_date, text = format(onset_date, "%d %B %Y"))) +
  geom_epicurve(fill = "steelblue") +
  labs(
    title = "Interactive Epidemic Curve",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  theme_minimal()

# Convert to interactive plotly plot
# tooltip = "text" shows only our custom tooltip
ggplotly(p, tooltip = "text")
```

Hover over any square to see its onset date. Each square represents one
case, stacked vertically by date!

## Interactive by Age Group

Now let’s colour by age group and add custom tooltips:

``` r

# Add custom tooltip text (plain text format)
cases$tooltip <- paste0(
  "Case ", cases$case_id, "\n",
  "Date: ", format(cases$onset_date, "%d %B %Y"), "\n",
  "Age Group: ", cases$age_group, "\n",
  "Sex: ", cases$sex, "\n",
  "Setting: ", cases$setting, "\n",
  "Outcome: ", cases$outcome
)

# Create plot with custom tooltips
p <- ggplot(cases, aes(x = onset_date, fill = age_group, text = tooltip)) +
  geom_epicurve() +
  scale_fill_brewer(palette = "Set2", name = "Age Group") +
  labs(
    title = "Interactive Epidemic Curve by Age Group",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

# Convert to interactive, showing only our custom tooltip
ggplotly(p, tooltip = "text")
```

## Faceted Interactive Plots

Interactive plots work with faceting too:

``` r

p <- ggplot(cases, aes(x = onset_date, fill = sex, text = tooltip)) +
  geom_epicurve() +
  scale_fill_manual(
    values = c("Female" = "#D55E00", "Male" = "#0072B2"),
    name = "Sex"
  ) +
  facet_wrap(~ setting, ncol = 1, scales = "free_y") +
  labs(
    title = "Interactive Epidemic Curves by Setting",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

ggplotly(p, tooltip = "text")
```

## Customising Tooltip Content

You have complete control over tooltip content. Use HTML formatting for
rich tooltips:

``` r

# Create formatted tooltips with HTML
cases$tooltip <- with(cases, paste0(
  "<b>Case ", case_id, "</b><br>",
  "<b>Date:</b> ", format(onset_date, "%d %B %Y"), "<br>",
  "<b>Demographics:</b> ", age_group, ", ", sex, "<br>",
  "<b>Setting:</b> ", setting, "<br>",
  "<b>Outcome:</b> ", outcome
))

p <- ggplot(cases, aes(x = onset_date, fill = outcome, text = tooltip)) +
  geom_epicurve() +
  scale_fill_brewer(palette = "Pastel1", name = "Outcome") +
  labs(
    title = "Interactive Epidemic Curve by Outcome",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

ggplotly(p, tooltip = "text")
```

## Interactive Time Period Variants

Epidemic curves work with different time scales. Here is a richer hourly
outbreak with a continuous source over three days — this produces a
curve whose bars vary in height, with proper time labels on the x-axis:

``` r

hourly_cases <- simulate_outbreak(
  n = 80,
  time_unit = "hourly",
  pattern = "continuous",
  date_range = 3,
  exposure = "2024-06-01",
  seed = 321,
  prop_missing = 0
)

p <- ggplot(hourly_cases, aes(x = onset_time, fill = sex)) +
  geom_epicurve() +
  scale_fill_manual(values = c(Female = "#D55E00", Male = "#0072B2")) +
  labs(
    title = "Hourly Epidemic Curve (Interactive)",
    x = "Time of Onset",
    y = "Number of Cases",
    fill = "Sex"
  ) +
  scale_y_epicurve() +
  theme_minimal()

ggplotly(p, tooltip = "text")
```

The width automatically adjusts based on the time unit detected.

## Interactive Large Outbreaks

For large outbreaks, the plot automatically switches to column chart
mode when case counts exceed the threshold. By default
[`geom_epicurve()`](https://prcleary.github.io/paulmisc/reference/geom_epicurve.md)
builds a sensible tooltip per case — date plus case count for that day —
so you don’t need to pre-aggregate the data yourself:

``` r

large_outbreak <- simulate_outbreak(
  n = 200,
  pattern = "continuous",
  date_range = 10,
  exposure = "2024-01-01",
  seed = 456,
  prop_missing = 0
)

p <- ggplot(large_outbreak, aes(x = onset_date)) +
  geom_epicurve(fill = "coral", max_stack = 20) +
  labs(
    title = "Large Outbreak (Column Chart Mode)",
    subtitle = "Auto-switched because some dates have >20 cases",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

ggplotly(p, tooltip = "text")
```

If you want a richer tooltip, just map your own to `text`:

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

ggplotly(p, tooltip = "text")
```

## Interactive Symbols

Use custom symbols with interactive tooltips. Named symbol vectors map
to categories automatically, and the legend updates without needing any
[`guides()`](https://ggplot2.tidyverse.org/reference/guides.html)
boilerplate:

``` r

symbol_cases <- simulate_outbreak(n = 30, seed = 789, prop_missing = 0)

sex_symbols <- c(Female = "\u2640", Male = "\u2642")

p <- ggplot(symbol_cases, aes(x = onset_date, colour = sex)) +
  geom_epicurve(symbol = sex_symbols, symbol_size = 6) +
  scale_colour_manual(
    values = c("Female" = "#D55E00", "Male" = "#0072B2"),
    name = "Sex"
  ) +
  labs(
    title = "Interactive Epidemic Curve with Symbols",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

ggplotly(p, tooltip = "text")
```

## Timeline Annotations

Add context to your epidemic curves with annotations.

### Interactive Annotations

Timeline annotations work seamlessly with plotly using the same code as
for static plots:

``` r

# Create plot with annotations - same code works for both static and interactive!
# The default tooltip already shows date and case count per day.
p <- ggplot(cases, aes(x = onset_date)) +
  geom_epicurve(fill = "steelblue") +
  annotate_period(
    date = as.Date("2024-05-28"),
    end_date = as.Date("2024-06-02"),
    label = "Exposure period",
    fill = "yellow",
    alpha = 0.25
  ) +
  annotate_event(
    date = as.Date("2024-06-05"),
    label = "Investigation",
    colour = "red"
  ) +
  labs(
    title = "Interactive Outbreak Timeline",
    subtitle = "Hover over cases for details",
    x = "Date",
    y = "Cases"
  ) +
  scale_y_epicurve() +
  theme_minimal()

# Convert to plotly - all annotations transfer automatically!
# ggplotly() doesn't render ggplot subtitles, so re-inject the title via
# layout() using an HTML <br> + <sup> for the subtitle.
ggplotly(p, tooltip = "text") |>
  layout(title = list(
    text = paste0(
      "Interactive Outbreak Timeline",
      "<br><sup>Hover over cases for details</sup>"
    )
  ))
```

The shaded period, event line, and all labels convert to plotly
automatically!

## Tips for Interactive Visualisation

- **Simple conversion**: Just wrap your ggplot with
  [`ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html) for basic
  interactivity
- **Custom tooltips**: Add a `text` aesthetic and use
  `ggplotly(p, tooltip = "text")` to show only your custom tooltip
- **Tooltip formatting**: Use `\n` for line breaks in plain text, or use
  HTML tags (e.g., `<br>`, `<b>`) for rich formatting
- **Performance**: Interactive plots with many cases (\>500) may be
  slow. Consider filtering for large datasets
- **Mobile devices**: Interactive features work on touch devices - tap
  to see tooltips
- **Export**: Use plotly’s built-in export button to save static images
- **Styling**: Default plot aesthetics work best - avoid overriding
  fill/colour in geom parameters when using aesthetics

## More Information

For more details on plotly customisation, see the [plotly for R
documentation](https://plotly-r.com/).
