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
(`plotly`) versions of the same plots. It also contains a thin shiny
launcher (`run_redshift_query_builder()`) for an app under
`inst/apps/`.

The user values **API ergonomics**: helpers should "just work" without
the user having to wire up `guides()`, manual aggregation for
tooltips, plotly hover templates, custom widget heights, etc.

## File map

```
R/
  geom_epicurve.R            Main geom + StatEpicurve + ggproto subclasses
                             + scale_y_epicurve() + coord_epicurve()
  annotate_epicurve.R        annotate_event / annotate_period via S3 ggplot_add
  simulate_outbreak.R        Random outbreak data generator
  epicurve_footnote.R        labs(caption=) helper summarising missing+timestamp
  epicurve_ggplotly.R        plotly wrapper: hover fixup, symbol legend, height
  zzz.R                      .onLoad: registers plotly S3 methods for our Geoms
  run_redshift_query_builder.R  Wrapper for the shiny app
  paulmisc-package.R         Package-level roxygen
tests/testthat/              testthat edition 3 (212 PASS, 1 pre-existing WARN)
vignettes/
  interactive-epicurves.Rmd  Plotly + annotations
  realistic-epicurves.Rmd    Larger-scale realistic outbreak walkthrough
inst/apps/                   Shiny app(s)
README.Rmd                   Main docs (README.md is built from this)
LLM_CONTEXT.md               THIS FILE
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

### 8. `coord_epicurve()` for static aspect ratio

`coord_epicurve()` is a named ggproto subclass of `CoordCartesian`
(class **`"CoordEpicurve"`** — first class matters, `inherits()` is
used downstream). It overrides `aspect()` to return a sensible
ratio **only for datetime axes** (when `x_min > 1e7`, i.e. POSIXct
seconds-since-epoch) so hourly / sub-daily epicurves render with
near-square cases without manual `theme(aspect.ratio = ...)`.
Returning `NULL` for date / numeric axes leaves panel sizing alone.

`geom_epicurve()` returns `list(layer, coord_epicurve())` by default
(controlled by `auto_aspect = TRUE`) so users get the right aspect
automatically.

### 9. plotly wrapper architecture (`R/epicurve_ggplotly.R` + `R/zzz.R`)

`epicurve_ggplotly(p)` is the **only** supported entry point for
interactive epicurves; do not tell users to call `plotly::ggplotly()`
directly. The wrapper:

1. Detects symbol layers (walking `p$layers`) to drive a custom
   top-right annotation legend (plotly's auto legend renders symbol
   keys as the placeholder `"Aa"` glyph).
2. If `inherits(p$coordinates, "CoordEpicurve")` **and** the x axis is
   datetime, computes a widget `height` that matches the static
   aspect, then swaps `p$coordinates <- coord_cartesian()` because
   plotly cannot honour a custom `Coord`.
3. Calls `plotly::ggplotly(p, tooltip = "text", ...)`.
4. Post-processes every trace via `.epicurve_fix_hover()`:
   * traces with non-empty `text` get
     `hovertemplate = "%{text}<extra></extra>"`, `hoverinfo = "text"`;
   * traces without useful text get `hoverinfo = "skip"` so the user
     never sees `"trace 0"`, `"trace 1"`, ... or a literal
     `%{text}` (which plotly prints when the referenced field is
     missing).
5. Re-injects the dropped `subtitle` into the title via
   `<br><sup>...</sup>`.
6. Adds the custom symbol-legend annotation block if needed.

`R/zzz.R` registers two plotly S3 methods so ggplotly() actually
renders our subclasses:

* `to_basic.GeomEpicurveRect` → delegates to `plotly:::to_basic.GeomRect`
* `geom2trace.GeomEpicurveText` → delegates to `plotly:::geom2trace.GeomText`

**DO NOT register a custom `geom2trace.GeomEpicurveRect`.** A previous
attempt that emitted plotly `bar` traces per fill group produced
layout-correct basic plots but broke stacked column mode (all fills
rendered at `base = 0` and overlapped under any `barmode`). The
stock polygon renderer handles stacking, faceting, fills and date
axes correctly out of the box; per-bar tooltips on basic plots are
the accepted trade-off (the polygon trace merges rectangles, so
hover on the fill returns nothing — `hoverinfo = "skip"` keeps it
clean rather than mislabelled).

## Common pitfalls

1. **`ggplotly()` drops the ggplot `subtitle`.** `epicurve_ggplotly()`
   already re-injects it; if you write your own plotly path do the
   same with `layout(title = list(text = "title<br><sup>sub</sup>"))`.
2. **`data$x` in `compute_panel` is numeric** — see Design Decision 2.
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
7. **plotly groups traces by aesthetic at layer setup, not by the
   `group` column emitted from `compute_panel`.** Subdividing
   `data$group` in `compute_panel` does NOT cause plotly to split
   the polygon trace per row. Don't waste cycles trying.
8. **PowerShell `Rscript` always returns exit code 1** when R writes
   to stderr (e.g. "package was built under R version X.Y.Z"). Treat
   `NativeCommandError` as informational unless the output itself
   shows a real failure.
9. **`vignettes/*.html` is tracked** in this repo. `devtools::build_vignettes()`
   moves output to `doc/` (gitignored) and **deletes** the tracked
   `vignettes/*.html`. Re-render in place with
   `devtools::load_all('.'); rmarkdown::render('vignettes/<name>.Rmd')`
   to restore them before committing.
10. **Date / POSIXct axis numeric magnitudes** (useful for detecting
    axis kind from numeric `data$x`):
    * POSIXct: seconds since epoch, `> 1e7`
    * Date: days since epoch, `> 1000` and `< 1e5`
    * numeric counts: typically `< 1000`
11. **During `git rebase`, `--theirs` means the commits being
    replayed (your local work)** and `--ours` means the upstream
    branch. This is the opposite of `git merge`. README PNG
    conflicts after `git pull --rebase`: `git checkout --theirs
    man/figures/*.png` keeps your freshly built versions.

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
- **No custom `geom2trace.GeomEpicurveRect`.** See Design Decision 9 —
  this was tried and reverted; it broke stacked column layouts. If
  the trade-off (no per-rect hover on basic stacked plots) becomes
  unacceptable, prefer a user-facing solution (let users map a
  `tooltip` aesthetic) over overriding plotly internals.
- **No automatic per-case tooltip on basic plotly plots.** The
  polygon trace plotly emits for stacked rectangles cannot carry
  per-row text without breaking layout. Custom tooltips work fine
  when the user maps the `text` aesthetic explicitly.

## Recent regression lessons (keep!)

- Verify visual rendering (open the HTML widget or inspect the
  built `htmlwidget` in detail), not just trace metadata, before
  claiming a plotly fix works. Trace counts and `text` vectors look
  fine even when the widget renders blank/overlapped.
- When a fix risks introducing a regression, prefer **disabling
  the broken bit cleanly** (e.g. `hoverinfo = "skip"`) over
  inventing a parallel trace structure.
- Once `coord_epicurve()` was added, `plotly::ggplotly()` started
  silently falling back when it saw a non-cartesian coord. Always
  strip back to `coord_cartesian()` before handing a ggplot to
  plotly.

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
