# Null-coalescing helper used by helpers below. Inline definition keeps
# the package compatible with older R versions where `%||%` is not in base.
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Epicurve geom: one square per case
#'
#' `geom_epicurve()` draws a classical epidemiological curve in which every
#' individual case is rendered as a small square stacked on top of others
#' sharing the same date (or other binning unit on the x-axis). The
#' companion stat, [stat_epicurve()], assigns each case a vertical
#' stacking position within its x-value bin.
#'
#' Because the geom delegates drawing to [ggplot2::GeomRect], all of the
#' usual rectangle aesthetics are supported (`fill`, `colour`, `alpha`,
#' `linewidth`, `linetype`) and integrate naturally with any ggplot2
#' scale, theme, facet, or coordinate system.
#'
#' @section Aesthetics:
#' `geom_epicurve()` understands the following aesthetics (required in
#' **bold**):
#' \itemize{
#'   \item **`x`** — typically a `Date` representing the date of onset.
#'   \item `y` — supplied automatically by [stat_epicurve()].
#'   \item `fill`, `colour`, `alpha`, `linewidth`, `linetype`, `group`.
#' }
#'
#' @section Interactive Visualisation:
#' Convert to interactive plotly plots using `plotly::ggplotly()`:
#' ```r
#' library(plotly)
#' p <- ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve(fill = "steelblue") +
#'   theme_minimal()
#' ggplotly(p, tooltip = c("x", "y"))
#' ```
#' For custom tooltips, add a `text` aesthetic and use the `tooltip` parameter:
#' ```r
#' cases$tooltip <- paste("Case ID:", cases$case_id)
#' p <- ggplot(cases, aes(x = onset_date, fill = age_group, text = tooltip)) +
#'   geom_epicurve()
#' ggplotly(p, tooltip = "text")
#' ```
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data; defaults
#'   to `"epicurve"`.
#' @param geom The geometric object to use; defaults to `"epicurve"`.
#' @param position Position adjustment, defaults to `"identity"`.
#' @param width Numeric width of each case square in x-axis units. If `NULL`
#'   (the default), automatically determines appropriate width based on the
#'   time unit: 0.9 for daily Date data, 3600 seconds for hourly POSIXct data,
#'   6.3 for weekly data, etc. Specify explicitly to override auto-detection.
#' @param height Numeric height of each case square in y-axis units;
#'   defaults to `0.9`. Values below 1 produce visible gaps between
#'   stacked cases.
#' @param max_stack Numeric threshold for switching from individual case
#'   squares to a column chart. If the maximum count at any x-value exceeds
#'   this threshold, the plot automatically displays as a column chart
#'   instead of stacked squares. Set to `NULL` to always show squares
#'   (default: `20`).
#' @param symbol Character string specifying a Unicode symbol or emoji to use
#'   instead of squares (default: `NULL` for squares). When provided, each
#'   case is rendered as the specified symbol. Examples: `"●"` (bullet),
#'   `"■"` (square), `"▲"` (triangle), `"♥"` (heart), `"😷"` (face mask emoji).
#'   Ignored if `max_stack` threshold is exceeded (uses column chart instead).
#'   Pass a named character vector (e.g. `c(Female = "♀", Male = "♂")`) to
#'   use different symbols per category; the names are matched against the
#'   discrete aesthetic mapping (typically `colour` or `fill`).
#' @param symbol_size Size of symbols when `symbol` is used (default: `3`).
#'   Adjust if symbols appear too large or small relative to the plot.
#' @param na.rm If `FALSE` (the default), missing values are removed with
#'   a warning.
#' @param show.legend Logical. Should this layer be included in the
#'   legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather
#'   than combining with them.
#' @param ... Other arguments passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer that can be added to a [ggplot2::ggplot()] object.
#'
#' @examples
#' library(ggplot2)
#'
#' cases <- simulate_outbreak()
#'
#' # Minimal epicurve (daily data)
#' ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve(fill = "steelblue") +
#'   theme_minimal()
#'
#' # Coloured by age group, faceted by setting
#' ggplot(cases, aes(x = onset_date, fill = age_group)) +
#'   geom_epicurve(colour = "grey20") +
#'   facet_wrap(~ setting, ncol = 1) +
#'   scale_fill_brewer(palette = "Set2") +
#'   theme_bw()
#'
#' # Hourly data (width auto-detects from POSIXct intervals)
#' hourly_cases <- data.frame(
#'   onset_time = as.POSIXct("2024-06-01 08:00:00") + 3600 * c(0, 1, 1, 2, 3, 3, 4),
#'   case_id = 1:7
#' )
#' ggplot(hourly_cases, aes(x = onset_time)) +
#'   geom_epicurve(fill = "darkred") +
#'   theme_minimal() +
#'   labs(title = "Hourly Epidemic Curve")
#'
#' # Weekly data (width auto-detects from Date intervals)
#' weekly_cases <- data.frame(
#'   epi_week = as.Date("2024-01-01") + 7 * c(0, 1, 1, 1, 2, 2, 3, 4),
#'   case_id = 1:8
#' )
#' ggplot(weekly_cases, aes(x = epi_week)) +
#'   geom_epicurve(fill = "forestgreen") +
#'   theme_minimal() +
#'   labs(title = "Weekly Epidemic Curve")
#'
#' # Automatic column chart for large outbreaks (max_stack threshold)
#' # When any date has > 20 cases, automatically switches to column chart
#' large_outbreak <- data.frame(
#'   onset_date = as.Date("2024-01-01") + sample(0:10, 150, replace = TRUE)
#' )
#' ggplot(large_outbreak, aes(x = onset_date)) +
#'   geom_epicurve(fill = "coral", max_stack = 20) +
#'   theme_minimal() +
#'   labs(title = "Large Outbreak (auto-switched to column chart)")
#'
#' # Force square mode even for large counts by setting max_stack = NULL
#' ggplot(large_outbreak, aes(x = onset_date)) +
#'   geom_epicurve(fill = "coral", max_stack = NULL) +
#'   theme_minimal() +
#'   labs(title = "Large Outbreak (forced square mode)")
#'
#' # Use symbols instead of squares (Unicode glyphs need font support;
#' # not rendered during R CMD check on minimal devices)
#' \dontrun{
#' cases_symbols <- simulate_outbreak(n = 30, seed = 999)
#' ggplot(cases_symbols, aes(x = onset_date)) +
#'   geom_epicurve(symbol = "\u25CF", symbol_size = 4, colour = "darkblue") +
#'   theme_minimal() +
#'   labs(title = "Epidemic Curve with Bullet Symbols")
#'
#' # Use emoji symbols (requires font support)
#' ggplot(cases_symbols, aes(x = onset_date, colour = sex)) +
#'   geom_epicurve(symbol = "\U0001F637", symbol_size = 5) +
#'   scale_colour_manual(values = c("Female" = "#D55E00", "Male" = "#0072B2")) +
#'   theme_minimal() +
#'   labs(title = "COVID-19 Cases with Face Mask Emoji")
#' }
#'
#' @seealso [simulate_outbreak()] for generating example data, [scale_y_epicurve()] for integer y-axis labels.
#'
#' @importFrom ggplot2 layer ggproto Stat aes
#' @export
geom_epicurve <- function(mapping = NULL,
                          data = NULL,
                          stat = "epicurve",
                          position = "identity",
                          ...,
                          width = NULL,
                          height = 0.9,
                          max_stack = 20,
                          symbol = NULL,
                          symbol_size = 3,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  # Determine which geom to use based on symbol parameter.
  # If `symbol` is supplied we render each case as text; otherwise we
  # render rectangles. Both helper geoms (GeomEpicurveRect /
  # GeomEpicurveText) declare the `text` aesthetic as known so that the
  # plotly tooltip mechanism works without ggplot2 warning about an
  # unknown aesthetic.
  use_geom <- if (!is.null(symbol)) GeomEpicurveText else GeomEpicurveRect

  dots <- list(...)

  # Build params list, excluding NULL width to avoid aesthetic warnings
  params <- c(
    list(
      height = height,
      max_stack = max_stack,
      symbol = symbol,
      symbol_size = symbol_size,
      na.rm = na.rm
    ),
    dots
  )
  if (!is.null(width)) {
    params$width <- width
  }

  layer_obj <- ggplot2::layer(
    geom        = use_geom,
    mapping     = mapping,
    data        = data,
    stat        = stat,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = params
  )

  # When `symbol` is a named character vector, automatically override the
  # colour/fill legend so each key shows the per-category symbol rather
  # than the default "a" glyph from GeomText. Returning a list lets us add
  # the guides() alongside the layer in a single `+` step. We override
  # both colour and fill guides: whichever the user maps will pick up the
  # override; the other is a no-op.
  if (!is.null(symbol) && length(symbol) > 1 && !is.null(names(symbol))) {
    override <- list(label = unname(symbol), size = symbol_size)
    return(list(
      layer_obj,
      ggplot2::guides(
        colour = ggplot2::guide_legend(override.aes = override),
        fill   = ggplot2::guide_legend(override.aes = override)
      )
    ))
  }
  # Single-symbol case: still auto-override so the legend shows the
  # symbol rather than a default text glyph.
  if (!is.null(symbol) && length(symbol) == 1) {
    override <- list(label = symbol, size = symbol_size)
    return(list(
      layer_obj,
      ggplot2::guides(
        colour = ggplot2::guide_legend(override.aes = override),
        fill   = ggplot2::guide_legend(override.aes = override)
      )
    ))
  }

  layer_obj
}

#' @rdname geom_epicurve
#' @export
stat_epicurve <- function(mapping = NULL,
                          data = NULL,
                          geom = "rect",
                          position = "identity",
                          ...,
                          width = NULL,
                          height = 0.9,
                          max_stack = 20,
                          symbol = NULL,
                          symbol_size = 3,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  dots <- list(...)

  # Build params list, excluding NULL width to avoid aesthetic warnings
  params <- c(
    list(
      height = height,
      max_stack = max_stack,
      symbol = symbol,
      symbol_size = symbol_size,
      na.rm = na.rm
    ),
    dots
  )
  if (!is.null(width)) {
    params$width <- width
  }
  
  ggplot2::layer(
    stat        = StatEpicurve,
    data        = data,
    mapping     = mapping,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = params
  )
}

#' Detect appropriate width for epicurve based on x-axis data
#'
#' @param x Vector of x-axis values (Date, POSIXct, or numeric)
#' @return Numeric width value appropriate for the data type and range
#' @keywords internal
#' @noRd
detect_epicurve_width <- function(x) {
  # Remove NAs for analysis
  x <- x[!is.na(x)]
  
  if (length(x) < 2) {
    # Not enough data to detect - use default
    return(0.9)
  }
  
  # Handle POSIXct/POSIXlt (datetime)
  if (inherits(x, "POSIXt")) {
    # Calculate median time difference in seconds
    x_sorted <- sort(x)
    diffs <- as.numeric(diff(x_sorted), units = "secs")
    median_diff <- stats::median(diffs, na.rm = TRUE)
    
    # Determine time unit based on typical difference
    if (median_diff <= 90) {
      # Minute-level data (≤1.5 minutes)
      return(60 * 0.9)  # 54 seconds
    } else if (median_diff <= 5400) {
      # Hourly data (≤1.5 hours)
      return(3600 * 0.9)  # 54 minutes in seconds
    } else if (median_diff <= 129600) {
      # Daily data (≤1.5 days)
      return(86400 * 0.9)  # ~21.6 hours in seconds
    } else {
      # Weekly or longer - use median difference * 0.9
      return(median_diff * 0.9)
    }
  }
  
  # Handle Date objects
  if (inherits(x, "Date")) {
    # Calculate median difference in days
    x_sorted <- sort(x)
    diffs <- as.numeric(diff(x_sorted))
    median_diff <- stats::median(diffs, na.rm = TRUE)
    
    if (median_diff <= 1.5) {
      # Daily data
      return(0.9)
    } else if (median_diff <= 10) {
      # Weekly-ish data (2-10 days between points)
      return(median_diff * 0.9)
    } else if (median_diff <= 45) {
      # Monthly-ish data (10-45 days between points)
      return(median_diff * 0.9)
    } else {
      # Longer periods
      return(median_diff * 0.9)
    }
  }
  
  # Handle numeric x-axis (generic fallback)
  x_sorted <- sort(x)
  diffs <- diff(x_sorted)
  median_diff <- stats::median(diffs, na.rm = TRUE)
  
  # For numeric, use 90% of median difference or 0.9 as minimum
  return(max(median_diff * 0.9, 0.9))
}

#' StatEpicurve: ggproto for stacking individual cases
#'
#' Computes, within each panel, a stacking index `y` for every case sharing
#' the same `x` value. Exported so that other developers can extend it.
#'
#' @format A [ggplot2::ggproto()] object inheriting from [ggplot2::Stat].
#' @keywords internal
#' @export
StatEpicurve <- ggplot2::ggproto(
  "StatEpicurve",
  ggplot2::Stat,
  required_aes = "x",

  compute_panel = function(self, data, scales, na.rm = FALSE, width = NULL, height = 0.9, max_stack = 20, symbol = NULL, symbol_size = 3) {
    # Auto-detect appropriate width if not specified
    if (is.null(width)) {
      width <- detect_epicurve_width(data$x)
    }
    
    data <- data[order(data$x, data$group), , drop = FALSE]
    data$y <- stats::ave(seq_len(nrow(data)), data$x, FUN = seq_along)
    
    # Check if we should switch to column chart mode
    max_count <- max(data$y, na.rm = TRUE)
    use_column_mode <- !is.null(max_stack) && max_count > max_stack
    
    # Determine if we're using symbols (only if NOT in column mode)
    use_symbol_mode <- !is.null(symbol) && !use_column_mode
    
    if (use_column_mode) {
      # Aggregate to counts for column chart mode
      # Keep one row per x value with the count as y
      # Preserve grouping aesthetics (fill, colour, etc.)
      
      # Identify grouping columns (x + aesthetic mappings)
      aesthetic_cols <- intersect(names(data), c("fill", "colour", "color", "alpha", "linetype", "linewidth"))
      group_cols <- unique(c("x", aesthetic_cols, "PANEL"))
      group_cols <- group_cols[group_cols %in% names(data)]
      
      # Create a grouping key
      if (length(group_cols) > 1) {
        data$group_key <- interaction(data[, group_cols, drop = FALSE], drop = TRUE)
      } else {
        data$group_key <- data[[group_cols[1]]]
      }
      
      # Aggregate counts
      counts <- stats::aggregate(
        y ~ group_key,
        data = data,
        FUN = length
      )
      names(counts)[names(counts) == "y"] <- "count"
      
      # Get first row of each group to preserve aesthetics
      data_unique <- data[!duplicated(data$group_key), , drop = FALSE]
      data_unique <- merge(data_unique, counts, by = "group_key", all.x = TRUE)
      data_unique$group_key <- NULL
      
      # Set y to the count for column chart
      data <- data_unique
      data$y <- data$count
      data$count <- NULL
      
      # Compute rectangle boundaries for column chart (bars from 0 to count)
      data$xmin <- data$x - width / 2
      data$xmax <- data$x + width / 2
      data$ymin <- 0
      data$ymax <- data$y
      
      # If symbol was requested but we're in column mode, show count as label instead
      # This ensures geom_text has the required label aesthetic
      if (!is.null(symbol)) {
        data$label <- as.character(data$y)
        data$size <- symbol_size
      }
    } else if (use_symbol_mode) {
      # Symbol mode - prepare data for geom_text
      # Use center positions instead of rectangle boundaries
      # y position is center of the "cell" where this case sits
      data$y <- data$y - 0.5  # Center vertically in the stacking position

      # Add label aesthetic with the symbol. When `symbol` is a named
      # character vector (e.g. c(Female = "\u2640", Male = "\u2642")), look up
      # the symbol per row using whichever discrete aesthetic column in `data`
      # has values matching the names of the vector. This allows different
      # symbols per category while sharing a single stacking sequence.
      if (length(symbol) > 1 && !is.null(names(symbol))) {
        match_col <- NULL
        for (col in c("colour", "color", "fill", "group")) {
          if (col %in% names(data)) {
            vals <- as.character(data[[col]])
            if (length(vals) > 0 && all(unique(vals) %in% names(symbol))) {
              match_col <- col
              break
            }
          }
        }
        if (is.null(match_col)) {
          stop("`symbol` is a named vector but no aesthetic column in the data ",
               "(colour, fill, group) has values matching its names: ",
               paste(names(symbol), collapse = ", "))
        }
        data$label <- symbol[as.character(data[[match_col]])]
      } else {
        data$label <- symbol[[1]]
      }
      data$size <- symbol_size

      # For geom_text, we don't need xmin/xmax/ymin/ymax
      # x and y are already set correctly
    } else {
      # Individual case square mode (original behavior)
      # Compute rectangle boundaries for geom_rect
      data$xmin <- data$x - width / 2
      data$xmax <- data$x + width / 2
      data$ymin <- pmax(0, data$y - 1 + (1 - height) / 2)
      data$ymax <- data$y - (1 - height) / 2
    }
    
    # Add padding points to ensure x-axis includes full width of edge rectangles
    # and a point at y=0 to ensure y-axis includes 0 for proper display
    if (nrow(data) > 0 && !use_symbol_mode) {
      # Only add rectangle-based padding for rect/column modes
      # Symbol mode handles positioning differently
      zero_row <- data[1, , drop = FALSE]
      zero_row$y <- 0
      zero_row$ymin <- 0
      zero_row$ymax <- 0
      
      # Add left edge padding (extends width/2 to the left of min x)
      left_pad <- zero_row
      left_pad$x <- min(data$x, na.rm = TRUE) - width / 2
      left_pad$xmin <- left_pad$x
      left_pad$xmax <- left_pad$x
      
      # Add right edge padding (extends width/2 to the right of max x)
      right_pad <- zero_row
      right_pad$x <- max(data$x, na.rm = TRUE) + width / 2
      right_pad$xmin <- right_pad$x
      right_pad$xmax <- right_pad$x
      
      data <- rbind(left_pad, zero_row, data, right_pad)
    }
    
    # Filter out anchor/padding rows (y=0) - they served their purpose for scales
    # In symbol mode, this also ensures we don't try to render padding symbols
    data <- data[data$y > 0, , drop = FALSE]

    # Auto-generate a sensible default tooltip if the user didn't supply
    # a `text` aesthetic. Counts per x are computed so the tooltip can
    # show "<date>: N case(s)" without the user having to pre-aggregate
    # and merge counts onto each row themselves. At this point ggplot2
    # has already mapped x to its scale's internal numeric representation,
    # so we look at `scales$x` to decide how to format it.
    if (nrow(data) > 0 && !("text" %in% names(data))) {
      x_key <- as.character(data$x)
      counts <- as.integer(table(x_key)[x_key])
      fmt_x <- tryCatch({
        if (!is.null(scales) && !is.null(scales$x) &&
            inherits(scales$x, "ScaleContinuousDatetime")) {
          format(as.POSIXct(data$x, origin = "1970-01-01", tz = "UTC"),
                 "%d %b %Y %H:%M")
        } else if (!is.null(scales) && !is.null(scales$x) &&
                   inherits(scales$x, "ScaleContinuousDate")) {
          format(as.Date(data$x, origin = "1970-01-01"), "%d %b %Y")
        } else if (inherits(data$x, "POSIXt")) {
          format(data$x, "%d %b %Y %H:%M")
        } else if (inherits(data$x, "Date")) {
          format(data$x, "%d %b %Y")
        } else {
          as.character(data$x)
        }
      }, error = function(e) as.character(data$x))
      data$text <- paste0(
        "<b>", fmt_x, "</b><br>",
        counts, " case", ifelse(counts == 1, "", "s")
      )
    }

    data
  }
)

#' GeomEpicurve: ggproto for drawing case squares
#'
#' Draws each case as a rectangle by delegating to [ggplot2::GeomRect].
#' Exported so that other developers can extend it.
#'
#' @format A [ggplot2::ggproto()] object inheriting from [ggplot2::Geom].
#' @keywords internal
#' @export
GeomEpicurve <- ggplot2::ggproto(
  "GeomEpicurve",
  ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(
    colour    = "white",
    fill      = "steelblue",
    linewidth = 0.4,
    linetype  = 1,
    alpha     = NA
  ),

  draw_key = ggplot2::draw_key_polygon,

  draw_panel = function(data,
                        panel_params,
                        coord,
                        width = 0.9,
                        height = 0.9) {
    # Filter out anchor point (y=0) used for scale range
    data <- data[data$y > 0, , drop = FALSE]
    
    data$xmin <- data$x - width  / 2
    data$xmax <- data$x + width  / 2
    # Ensure ymin is never below 0 to prevent truncation at bottom
    data$ymin <- pmax(0, data$y - 1 + (1 - height) / 2)
    data$ymax <- data$y     - (1 - height) / 2

    ggplot2::GeomRect$draw_panel(data, panel_params, coord)
  }
)

#' GeomEpicurveRect: GeomRect that knows about the `text` aesthetic
#'
#' A thin subclass of [ggplot2::GeomRect] that declares the `text`
#' aesthetic. This is needed so that the tooltip text added by
#' [StatEpicurve] (or supplied by the user via
#' `aes(text = ...)`) is propagated through to `plotly::ggplotly()`
#' without ggplot2 emitting an "Ignoring unknown aesthetics: text"
#' warning during static rendering.
#'
#' @format A [ggplot2::ggproto()] object inheriting from
#'   [ggplot2::GeomRect].
#' @keywords internal
#' @export
GeomEpicurveRect <- ggplot2::ggproto(
  "GeomEpicurveRect",
  ggplot2::GeomRect,
  default_aes = ggplot2::aes(
    colour    = NA,
    fill      = "grey35",
    linewidth = 0.5,
    linetype  = 1,
    alpha     = NA,
    text      = NULL
  )
)

#' GeomEpicurveText: GeomText that knows about `text` and draws symbol keys
#'
#' A thin subclass of [ggplot2::GeomText] used in symbol mode. It
#' declares the `text` aesthetic (so plotly tooltips work without a
#' warning) and overrides `draw_key` so that legend keys display the
#' actual case symbol at the requested size rather than the default
#' "a" glyph.
#'
#' @format A [ggplot2::ggproto()] object inheriting from
#'   [ggplot2::GeomText].
#' @keywords internal
#' @export
GeomEpicurveText <- ggplot2::ggproto(
  "GeomEpicurveText",
  ggplot2::GeomText,
  default_aes = ggplot2::aes(
    colour     = "black",
    size       = 3.88,
    angle      = 0,
    hjust      = 0.5,
    vjust      = 0.5,
    alpha      = NA,
    family     = "",
    fontface   = 1,
    lineheight = 1.2,
    text       = NULL
  ),
  draw_key = function(data, params, size) {
    # Use the label aesthetic from the legend data if present; fall back
    # to the symbol supplied via params (single-symbol case).
    lbl <- data$label
    if (is.null(lbl) || is.na(lbl) || nchar(as.character(lbl)) == 0) {
      lbl <- params$symbol %||% "\u25A0"
    }
    grid::textGrob(
      label = as.character(lbl)[1],
      x = 0.5, y = 0.5,
      gp = grid::gpar(
        col      = data$colour %||% "black",
        fontsize = (data$size %||% params$symbol_size %||% 3) * .pt,
        fontfamily = data$family %||% ""
      )
    )
  }
)

#' Y-axis scale with integer breaks for epidemic curves
#'
#' A convenience function that sets y-axis breaks to integers only,
#' which is appropriate for count data in epidemic curves. This ensures
#' the y-axis never shows decimal values like 2.5 cases.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_y_continuous()].
#'
#' @return A ggplot2 scale layer.
#'
#' @examples
#' library(ggplot2)
#' cases <- simulate_outbreak(n = 20)
#'
#' ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve(fill = "steelblue") +
#'   scale_y_epicurve() +
#'   theme_minimal()
#'
#' @importFrom ggplot2 scale_y_continuous
#' @export
scale_y_epicurve <- function(...) {
  ggplot2::scale_y_continuous(
    breaks = function(limits) {
      # Generate pretty integer breaks
      at_values <- pretty(limits, n = 5)
      # Keep only integers
      at_values[at_values == floor(at_values) & at_values >= 0]
    },
    ...
  )
}
