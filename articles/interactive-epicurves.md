# Interactive Epidemic Curves with Plotly

## Overview

The
[`geom_epicurve()`](https://prcleary.github.io/paulmisc/reference/geom_epicurve.md)
function works seamlessly with [plotly](https://plotly.com/r/) to create
interactive epidemic curves with custom tooltips. Simply create your
plot with ggplot2 and convert it using
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

# Create ggplot
p <- ggplot(cases, aes(x = onset_date)) +
  geom_epicurve(fill = "steelblue") +
  labs(
    title = "Interactive Epidemic Curve",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  theme_minimal()
#> Warning: Duplicated aesthetics after name standardisation: fill

# Convert to interactive plotly plot
ggplotly(p)
```

Hover over any square to see details!

## Interactive by Age Group

Now let’s colour by age group and add custom tooltips:

``` r

# Add custom tooltip text
cases$tooltip <- paste0(
  "<b>Case ", cases$case_id, "</b><br>",
  "Date: ", format(cases$onset_date, "%d %B %Y"), "<br>",
  "Age Group: ", cases$age_group, "<br>",
  "Sex: ", cases$sex, "<br>",
  "Setting: ", cases$setting, "<br>",
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
    values = c("Male" = "#0072B2", "Female" = "#D55E00"),
    name = "Sex"
  ) +
  facet_wrap(~ setting, ncol = 1, scales = "free_y") +
  labs(
    title = "Interactive Epidemic Curves by Setting",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  theme_minimal()

ggplotly(p, tooltip = "text")
#> Warning: No shared levels found between `names(values)` of the manual scale and the
#> data's fill values.
```

## Customising Tooltip Content

You have complete control over tooltip content. Use HTML formatting for
rich tooltips:

``` r

# Create highly customised tooltips with HTML formatting
cases$tooltip <- with(cases, paste0(
  "<b style='font-size:14px'>Case ", case_id, "</b><br>",
  "<hr style='margin:2px'>",
  "<i>Date:</i> ", format(onset_date, "%d %B %Y"), "<br>",
  "<i>Demographics:</i> ", age_group, ", ", sex, "<br>",
  "<i>Setting:</i> ", setting, "<br>",
  "<i>Outcome:</i> ", outcome
))

p <- ggplot(cases, aes(x = onset_date, fill = outcome, text = tooltip)) +
  geom_epicurve() +
  scale_fill_brewer(palette = "Pastel1", name = "Outcome") +
  labs(
    title = "Interactive Epidemic Curve by Outcome",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  theme_minimal()

ggplotly(p, tooltip = "text")
```

## Tips for Interactive Visualisation

- **Simple conversion**: Just wrap your ggplot with
  [`ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html) for basic
  interactivity
- **Custom tooltips**: Add a `text` aesthetic and use
  `ggplotly(p, tooltip = "text")` to show only your custom tooltip
- **Tooltip formatting**: Use HTML tags (`<br>`, `<b>`, `<i>`, `<hr>`)
  for rich formatting
- **Performance**: Interactive plots with many cases (\>500) may be
  slow. Consider filtering for large datasets
- **Mobile devices**: Interactive features work on touch devices - tap
  to see tooltips
- **Export**: Use plotly’s built-in export button to save static images

## More Information

For more details on plotly customisation, see the [plotly for R
documentation](https://plotly-r.com/).
