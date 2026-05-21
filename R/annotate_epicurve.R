#' Annotate epidemic curves with events and periods
#'
#' Helper functions to add contextual annotations to epidemic curves, such as
#' intervention dates (events) or exposure periods (shaded regions). These
#' functions work with both static ggplot2 plots and interactive plotly
#' conversions using `ggplotly()`.
#'
#' @param date Date or POSIXct value for the event or start of period
#' @param label Character string for the annotation label
#' @param colour,color Colour for the line or fill (American/British spelling accepted)
#' @param linetype Line type for event markers (default: "dashed")
#' @param linewidth Width of the event line (default: 0.75)
#' @param alpha Transparency for period shading (default: 0.3)
#' @param label_y Vertical position for the label (default: "top" for events,
#'   "top" for periods). Can be numeric or "top"/"bottom"/"middle".
#' @param label_hjust Horizontal justification for label (default: 0 for events,
#'   0.5 for periods)
#' @param label_vjust Vertical justification for label (default: 1 for events,
#'   1 for periods - labels hang down from the top)
#' @param label_size Text size for label (default: 3.5)
#' @param end_date End date for periods (required for `annotate_period()`)
#' @param ... Additional arguments passed to the underlying geom
#'
#' @return A list of ggplot2 layers that can be added to a plot
#'
#' @examples
#' library(ggplot2)
#'
#' cases <- simulate_outbreak(n = 50, seed = 123)
#'
#' # Add an event marker for an intervention
#' ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve(fill = "steelblue") +
#'   annotate_event(
#'     date = as.Date("2024-06-05"),
#'     label = "Contaminated\nfood recalled",
#'     colour = "red"
#'   ) +
#'   theme_minimal()
#'
#' # Add a period for exposure window
#' ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve(fill = "steelblue") +
#'   annotate_period(
#'     date = as.Date("2024-05-25"),
#'     end_date = as.Date("2024-06-01"),
#'     label = "Likely exposure period",
#'     fill = "coral"
#'   ) +
#'   theme_minimal()
#'
#' # Combine multiple annotations
#' ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve(fill = "steelblue") +
#'   annotate_period(
#'     date = as.Date("2024-05-28"),
#'     end_date = as.Date("2024-06-02"),
#'     label = "Incubation period",
#'     fill = "yellow"
#'   ) +
#'   annotate_event(
#'     date = as.Date("2024-06-03"),
#'     label = "Investigation\ninitiated",
#'     colour = "darkgreen"
#'   ) +
#'   annotate_event(
#'     date = as.Date("2024-06-07"),
#'     label = "Outbreak\ndeclared over",
#'     colour = "purple"
#'   ) +
#'   theme_minimal() +
#'   labs(title = "Outbreak Timeline with Annotations")
#'
#' # Works with plotly for interactive plots (same code!)
#' \dontrun{
#' library(plotly)
#' p <- ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve(fill = "steelblue") +
#'   annotate_period(
#'     date = as.Date("2024-05-28"),
#'     end_date = as.Date("2024-06-02"),
#'     label = "Exposure period",
#'     fill = "yellow"
#'   ) +
#'   annotate_event(
#'     date = as.Date("2024-06-05"),
#'     label = "Investigation",
#'     colour = "red"
#'   ) +
#'   theme_minimal()
#' ggplotly(p)
#' }
#'
#' @name annotate_epicurve
#' @importFrom ggplot2 geom_vline geom_rect geom_text geom_segment aes ggplot_add ggplot_build
NULL

# Annotations are stored as lightweight S3 objects and resolved lazily via
# ggplot_add(). When the user adds an annotation to a plot with `+`, we build
# the plot up to that point to obtain the trained y-scale, then construct
# standard ggplot2 geom layers with finite, explicit y coordinates. This is
# essential for plotly compatibility: plotly extracts data from layers via
# ggplot_build, and cannot handle Inf or untrained placeholder coordinates.
# Using finite explicit values means the same code works for both static
# ggplot2 plots and interactive ggplotly() conversions with no extra code.

# Compute the trained y-range for a plot, falling back to (0, 1) when the
# plot has no other layers contributing y data.
.epicurve_y_range <- function(plot) {
  yr <- tryCatch({
    b <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(plot)))
    b$layout$panel_scales_y[[1]]$dimension()
  }, error = function(e) NULL)
  if (is.null(yr) || length(yr) < 2 || any(!is.finite(yr))) {
    yr <- c(0, 1)
  }
  yr
}

# Resolve a fractional label_y (0 = bottom, 1 = top) given user input that
# may be Inf, -Inf, a fraction, or a string like "top"/"middle"/"bottom".
.resolve_y_frac <- function(label_y) {
  if (is.character(label_y)) {
    return(switch(tolower(label_y), "top" = 1, "bottom" = 0, "middle" = 0.5, 1))
  }
  if (is.infinite(label_y) && label_y > 0) return(1)
  if (is.infinite(label_y) && label_y < 0) return(0)
  label_y
}

#' @rdname annotate_epicurve
#' @export
annotate_event <- function(date,
                          label,
                          colour = "red",
                          color = NULL,
                          linetype = "dashed",
                          linewidth = 0.75,
                          label_y = Inf,
                          label_hjust = 0,
                          label_vjust = 0,
                          label_size = 3.5,
                          ...) {

  # Handle American spelling
  if (!is.null(color)) {
    colour <- color
  }

  structure(
    list(
      date = date,
      label = label,
      colour = colour,
      linetype = linetype,
      linewidth = linewidth,
      label_y = label_y,
      label_hjust = label_hjust,
      label_vjust = label_vjust,
      label_size = label_size,
      extra = list(...)
    ),
    class = c("epicurve_event_annotation", "epicurve_annotation")
  )
}

#' @rdname annotate_epicurve
#' @export
annotate_period <- function(date,
                           end_date,
                           label,
                           fill = "grey",
                           colour = NA,
                           color = NULL,
                           alpha = 0.3,
                           label_y = Inf,
                           label_hjust = 0.5,
                           label_vjust = 0,
                           label_size = 3.5,
                           ...) {

  if (missing(end_date)) {
    stop("end_date is required for annotate_period()", call. = FALSE)
  }

  # Handle American spelling
  if (!is.null(color)) {
    colour <- color
  }

  # Calculate midpoint for label
  if (inherits(date, "POSIXt") || inherits(end_date, "POSIXt")) {
    mid_date <- as.POSIXct((as.numeric(date) + as.numeric(end_date)) / 2,
                           origin = "1970-01-01", tz = attr(date, "tzone") %||% "UTC")
  } else {
    mid_date <- (as.numeric(date) + as.numeric(end_date)) / 2
    if (inherits(date, "Date")) {
      mid_date <- as.Date(mid_date, origin = "1970-01-01")
    }
  }

  structure(
    list(
      date = date,
      end_date = end_date,
      mid_date = mid_date,
      label = label,
      fill = fill,
      colour = colour,
      alpha = alpha,
      label_y = label_y,
      label_hjust = label_hjust,
      label_vjust = label_vjust,
      label_size = label_size,
      extra = list(...)
    ),
    class = c("epicurve_period_annotation", "epicurve_annotation")
  )
}

# ggplot_add methods: resolve annotations lazily when added to a plot via `+`.
# At this point we can build the existing plot to obtain the trained y-scale
# and inject finite coordinates suitable for both static rendering and
# ggplotly() conversion.

# Snapshot the data-driven y-range the first time an annotation is added, and
# stash it on the plot so subsequent annotations reuse the same baseline. This
# prevents "creep" where each annotation's geometry extends the trained scale
# and pushes later annotations progressively higher.
.epicurve_orig_yr <- function(plot) {
  yr <- attr(plot, "epicurve_orig_yr")
  if (is.null(yr)) yr <- .epicurve_y_range(plot)
  yr
}

#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.epicurve_event_annotation <- function(object, plot, object_name) {
  yr <- .epicurve_orig_yr(plot)
  span <- yr[2] - yr[1]
  y_frac <- .resolve_y_frac(object$label_y)
  # When labelling at the top, place the label in dedicated headroom above the
  # data top (yr[2]) so it doesn't overlap bars or shaded period rectangles.
  # expand_limits() below grows the panel to fit. Using a fixed offset from
  # the snapshotted yr (not the current built scale) prevents "creep".
  use_top_vjust <- y_frac >= 1 && object$label_vjust >= 0 && object$label_vjust <= 0.5
  if (use_top_vjust) {
    label_y_val <- yr[2] + 0.10 * span
    effective_vjust <- 0
    top_pad <- yr[2] + 0.18 * span
  } else {
    label_y_val <- yr[1] + y_frac * span
    effective_vjust <- object$label_vjust
    top_pad <- NULL
  }

  # Hover text for plotly tooltips (ignored by ggplot2 but used by ggplotly).
  hover_text <- paste0(object$label, "<br>", format(object$date))

  layers <- list(
    ggplot2::geom_segment(
      data = data.frame(x = object$date, xend = object$date,
                        y = yr[1], yend = yr[2], text = hover_text),
      mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend, text = text),
      inherit.aes = FALSE,
      show.legend = FALSE,
      linetype = object$linetype,
      colour = object$colour,
      linewidth = object$linewidth,
      na.rm = TRUE
    ),
    ggplot2::geom_text(
      data = data.frame(x = object$date, y = label_y_val,
                        label = object$label, text = hover_text),
      mapping = ggplot2::aes(x = x, y = y, label = label, text = text),
      inherit.aes = FALSE,
      show.legend = FALSE,
      hjust = object$label_hjust,
      vjust = effective_vjust,
      colour = object$colour,
      size = object$label_size,
      na.rm = TRUE
    )
  )
  if (!is.null(top_pad)) {
    layers <- c(layers, list(ggplot2::expand_limits(y = top_pad)))
  }
  result <- Reduce(`+`, layers, init = plot)
  attr(result, "epicurve_orig_yr") <- yr
  result
}

#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.epicurve_period_annotation <- function(object, plot, object_name) {
  yr <- .epicurve_orig_yr(plot)
  span <- yr[2] - yr[1]
  y_frac <- .resolve_y_frac(object$label_y)
  # See note in ggplot_add.epicurve_event_annotation: place the label in the
  # headroom added by expand_limits() so it sits clearly above the shaded
  # period rectangle rather than overlapping its top edge.
  use_top_vjust <- y_frac >= 1 && object$label_vjust >= 0 && object$label_vjust <= 0.5
  if (use_top_vjust) {
    label_y_val <- yr[2] + 0.10 * span
    effective_vjust <- 0
    top_pad <- yr[2] + 0.18 * span
  } else {
    label_y_val <- yr[1] + y_frac * span
    effective_vjust <- object$label_vjust
    top_pad <- NULL
  }
  label_colour <- if (is.na(object$colour)) "black" else object$colour

  # Hover text for plotly tooltips: label plus the date range.
  hover_text <- paste0(
    object$label, "<br>",
    format(object$date), " \u2013 ", format(object$end_date)
  )

  layers <- list(
    ggplot2::geom_rect(
      data = data.frame(xmin = object$date, xmax = object$end_date,
                        ymin = yr[1], ymax = yr[2], text = hover_text),
      mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                             text = text),
      inherit.aes = FALSE,
      show.legend = FALSE,
      fill = object$fill,
      colour = object$colour,
      alpha = object$alpha,
      na.rm = TRUE
    ),
    ggplot2::geom_text(
      data = data.frame(x = object$mid_date, y = label_y_val,
                        label = object$label, text = hover_text),
      mapping = ggplot2::aes(x = x, y = y, label = label, text = text),
      inherit.aes = FALSE,
      show.legend = FALSE,
      hjust = object$label_hjust,
      vjust = effective_vjust,
      colour = label_colour,
      size = object$label_size,
      na.rm = TRUE
    )
  )
  if (!is.null(top_pad)) {
    layers <- c(layers, list(ggplot2::expand_limits(y = top_pad)))
  }
  result <- Reduce(`+`, layers, init = plot)
  attr(result, "epicurve_orig_yr") <- yr
  result
}
