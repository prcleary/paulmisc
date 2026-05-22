# LLM Context for the `paulmisc` Package

This file is written for an LLM (or a human stepping in fresh) who needs
to make changes to this package. It captures the architecture, the
non-obvious design decisions, the common pitfalls, and the repo
workflow. Read this before editing anything in `R/`.

## What this package is

`paulmisc` is a personal R package whose flagship feature is a set of
`ggplot2`-native helpers for **epidemic curves**: stacked-square (case
chart) and bar-mode epicurves, custom Unicode/emoji symbols, timeline
annotations (events and periods), and helpers for interactive
(`plotly`) versions of the same plots. It also contains some Nextcloud
calendar utilities (`R/get_nextcloud_tasks.R` and friends) which are
unrelated to the epicurve code.

The user values **API ergonomics**: helpers should "just work" without
the user having to wire up `guides()`, manual aggregation for
tooltips, etc.

## File map

```
R/
  geom_epicurve.R            Main geom + StatEpicurve + ggproto subclasses
  scale_y_epicurve.R         Integer-only y scale wrapper
  annotate_epicurve.R        annotate_event / annotate_period via S3 ggplot_add
  simulate_outbreak.R        Random outbreak data generator
  epicurve_footnote.R        labs(caption=) helper summarising missing+timestamp
  rand_cb_tasks.R            (calendar helpers, separate domain)
  get_nextcloud_tasks.R      (calendar helpers)
  fetch_calendar_tasks.R     (calendar helpers)
  parse_icalendar*.R         (calendar helpers)
  discover_calendars.R       (calendar helpers)
  run_app.R                  Wrappers for inst/apps shiny apps
tests/testthat/              testthat edition 3
vignettes/
  interactive-epicurves.Rmd  Plotly + annotations
inst/apps/                   Two small shiny apps
README.Rmd                   Main docs (README.md is built from this)
```

## Critical design decisions

### 1. Annotations use S3 `ggplot_add`, not layers

`annotate_event()` and `annotate_period()` return a list with a custom
S3 class, and a `ggplot_add.epicurve_annotation` method injects the
underlying geoms at the right place. **Why**: this lets us read the
plot's already-computed y-axis and anchor labels above the bars
without users specifying y coordinates. It also lets us "snapshot" the
y-axis (stored in an attribute on the plot) so subsequent annotations
don't shift if more cases are added.

### 2. `scales$x` class detection inside Stats

Inside `StatEpicurve$compute_panel(data, scales, ...)`, `data$x` has
already been transformed to numeric (days-since-1970 for dates,
seconds for POSIXct). The original column class is **gone**. To
recover it, inspect the scale:

```r
if (inherits(scales$x, "ScaleContinuousDate")) {
  x_label <- format(as.Date(data$x, origin = "1970-01-01"))
} else if (inherits(scales$x, "ScaleContinuousDatetime")) {
  x_label <- format(as.POSIXct(data$x, origin = "1970-01-01", tz = "UTC"))
} else {
  x_label <- as.character(data$x)
}
```

This is how the auto-tooltip (`data$text`) is built so plotly shows
"3 Jun 2024" rather than "19877".

### 3. Returning `list(layer, guides(...))` from `geom_epicurve()`

ggplot2's `+.gg` accepts a list of elements, so a geom function may
return both a layer and a `guides()` object. We use this so that when
the user passes a *named* `symbol` vector (e.g. `c(F = "\u2640", M =
"\u2642")`) we automatically override the colour/fill legend keys.
Users no longer write `guides(colour = guide_legend(override.aes =
list(label = ..., size = ...)))`.

Tests on the return type must therefore allow **both**: scalar
`symbol` returns a list with a `Layer` and a `Guides` element; no
symbol returns just a `Layer`.

### 4. `GeomEpicurveRect` / `GeomEpicurveText` subclasses

We subclass `GeomRect` and `GeomText` so we can:

- declare `text = NULL` in `default_aes`, silencing the
  "Ignoring unknown aesthetics: text" warning when users wire up
  custom plotly tooltips;
- give `GeomEpicurveText` a custom `draw_key` that renders the actual
  symbol (not the default "Aa" glyph) at the user's requested size in
  the legend.

`inherits()` checks for `"GeomRect"`/`"GeomText"` still pass, so
existing tests are unaffected.

### 5. White-border default has been intentionally removed

An earlier attempt to inject a white border by default caused
*visible* width inconsistencies in the hourly chart, because the
border was drawn outside the rectangle. The geom now relies on the
auto-computed width's built-in 10% gap. **Do not re-add a default
white border without addressing the width artefact.**

### 6. `simulate_outbreak()` injects missing data by default

`prop_missing = 0.05` is the default. This mirrors real notification
data. Any test that asserts an exact `nrow()`, expects `all(x %in%
levels)`, or otherwise can't tolerate `NA` must explicitly pass
`prop_missing = 0`. **Many existing tests had to be patched for this**
— if you add a new test, default to `prop_missing = 0` unless the
test is specifically about missingness.

### 7. `epicurve_footnote()`

Returns a `ggplot2::labs(caption = ...)` object. Class is
`"ggplot2::labels"` in newer ggplot2 (S7-backed) — old `"labels"`
test strings won't match. The footnote summarises missingness and a
timestamp; both are toggleable.

## Common pitfalls

1. **`ggplotly()` drops the ggplot `subtitle`.** Re-inject via
   `layout(title = list(text = "title<br><sup>subtitle</sup>"))`.
2. **`data$x` in `compute_panel` is numeric** — see (2) above.
3. **`render-readme.yaml` workflow auto-rebuilds README.md** on every
   push to `R/`, `DESCRIPTION`, `README.Rmd`, or the workflow file
   itself. **Never commit README.md by hand**; always edit README.Rmd
   and let CI rebuild. Because CI pushes a commit, the local push
   pattern is:
   ```
   git add -A
   git commit -m "..."
   git pull --rebase
   git push
   ```
4. **`man/*.Rd` and `NAMESPACE`** are generated by
   `devtools::document()`. Run it after editing roxygen comments;
   never hand-edit those files.
5. The `%||%` operator is defined locally at the top of
   `R/geom_epicurve.R` for portability — do not assume it's exported
   by rlang.
6. `vapply` checks for `Guides`/`Layer` ggproto inheritance are
   case-sensitive: `"Guides"` (capital G), `"Layer"`.

## Test conventions

- testthat edition 3.
- `tests/testthat/test-*.R` — one file per `R/*.R` file roughly.
- Use `prop_missing = 0` in any `simulate_outbreak()` call where the
  test cannot tolerate `NA`.
- Use `ggplot2::ggplot_build(p)` to inspect computed layer data.
- Coverage is measured with `covr::package_coverage()`; target ≥80%.

## Quality gates before pushing

```r
devtools::document()
devtools::test()        # all PASS, 0 FAIL
devtools::check()       # no errors / warnings
goodpractice::gp()      # review notes
covr::package_coverage()
```

## Commit/push workflow

```powershell
cd "C:\path\to\paulmisc"
git add -A
git commit -m "concise summary"
git pull --rebase
git push
```

The `render-readme` workflow may push README.md changes seconds later;
the rebase handles that.

## Things deliberately NOT done

- **No automatic theme.** Users pick their own (`theme_minimal()` is
  used in docs but never injected).
- **No date axis formatting** inside the geom — the user controls it
  via `scale_x_date()`.
- **No data validation** beyond what `aes()` already does — the geom
  trusts that `x` is a date, datetime, or numeric.

## Quick mental model for the geom pipeline

```
user data (one row per case, x = date/time/numeric)
  -> StatEpicurve$compute_panel:
       - aggregates onto a discrete x grid
       - stacks cases (1, 2, 3, ...) up the y-axis
       - injects data$text tooltip (date+count) if not set
       - switches to column mode when stack > max_stack
  -> GeomEpicurveRect  (default; rectangle per case)
     or GeomEpicurveText  (when symbol= is given; textGrob per case)
  -> optional auto guides() when symbol is named
```

Keep this file updated when you change any of the above.
