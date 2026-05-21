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
p <- ggplot(cases, aes(x = onset_date, fill = "Cases")) +
  geom_epicurve() +
  scale_fill_manual(values = c("Cases" = "steelblue"), name = NULL) +
  labs(
    title = "Interactive Epidemic Curve",
    x = "Date of Onset",
    y = "Number of Cases"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Convert to interactive plotly plot
ggplotly(p, tooltip = c("x", "y"))
```

Hover over any square to see the date and count!

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
  theme_minimal()

ggplotly(p, tooltip = "text")
```

## Customising Tooltip Content

You have complete control over tooltip content. Use HTML formatting for
rich tooltips:

``` r

# Create formatted tooltips (using line breaks for structure)
cases$tooltip <- with(cases, paste0(
  "Case ", case_id, "\n",
  "-------------------\n",
  "Date: ", format(onset_date, "%d %B %Y"), "\n",
  "Demographics: ", age_group, ", ", sex, "\n",
  "Setting: ", setting, "\n",
  "Outcome: ", outcome
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
- **Tooltip formatting**: Use `\n` for line breaks and simple text
  formatting (HTML is not rendered in ggplotly tooltips)
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
