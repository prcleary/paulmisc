.onLoad <- function(libname, pkgname) {
  # Register plotly `to_basic` S3 methods for our custom Geom subclasses.
  # plotly dispatches on the *first* class of the Geom ggproto object, so
  # `GeomEpicurveRect` / `GeomEpicurveText` don't pick up the inherited
  # `to_basic.GeomRect` / `to_basic.GeomText` methods and ggplotly() would
  # otherwise warn "geom_GeomEpicurveRect() has yet to be implemented in
  # plotly" and produce an empty trace.
  if (requireNamespace("plotly", quietly = TRUE)) {
    text_trace_method <- tryCatch(
      utils::getFromNamespace("geom2trace.GeomText", "plotly"),
      error = function(e) NULL
    )
    if (!is.null(text_trace_method)) {
      registerS3method(
        "geom2trace", "GeomEpicurveText", text_trace_method,
        envir = asNamespace("plotly")
      )
    }
    # Identity to_basic so that the first class of the data stays
    # `GeomEpicurveRect` and our custom geom2trace below is dispatched.
    # (Delegating to plotly's to_basic.GeomRect would re-class the data
    # to GeomRect and route to the default polygon converter, which
    # drops per-row tooltip text.)
    registerS3method(
      "to_basic", "GeomEpicurveRect",
      function(data, prestats_data, layout, params, p, ...) data,
      envir = asNamespace("plotly")
    )
    # Custom geom2trace: emit a plotly bar trace per fill colour with
    # per-row hover text. plotly's default rect renderer merges every
    # rectangle into one polygon trace and loses per-bar tooltips.
    registerS3method(
      "geom2trace", "GeomEpicurveRect",
      .epicurve_geom2trace_rect,
      envir = asNamespace("plotly")
    )
  }
}

# Custom geom2trace that converts geom_rect-shaped data into a single
# plotly bar trace with per-row hover text. plotly groups input data by
# aesthetic before calling geom2trace, so we always emit one trace per
# call and rely on plotly's own grouping for fill/colour splits.
#
# Important: plotly's JS layer expects dates / datetimes as JS Date
# milliseconds. Our data$x at this stage is numeric (days-since-epoch
# for Date scales, seconds-since-epoch for POSIXct scales). We detect
# the axis kind by magnitude and rescale both bar centres and bar
# widths to milliseconds so bars line up with the date axis.
.epicurve_geom2trace_rect <- function(data, params, p) {
  needed <- c("xmin", "xmax", "ymin", "ymax")
  if (!all(needed %in% names(data)) || nrow(data) == 0) {
    return(list(type = "bar", x = numeric(), y = numeric(),
                hoverinfo = "skip", showlegend = FALSE))
  }

  x_centre <- (data$xmin + data$xmax) / 2
  width    <- data$xmax - data$xmin

  x_med <- suppressWarnings(stats::median(x_centre, na.rm = TRUE))
  if (is.finite(x_med)) {
    if (x_med > 1e7) {
      x_centre <- x_centre * 1000
      width    <- width * 1000
    } else if (x_med > 1000 && x_med < 1e5) {
      x_centre <- x_centre * 86400000
      width    <- width * 86400000
    }
  }

  fill_col   <- if ("fill" %in% names(data) && length(data$fill) > 0) data$fill[1] else "steelblue"
  line_col   <- if ("colour" %in% names(data) && length(data$colour) > 0) data$colour[1] else "white"
  line_w_raw <- if ("linewidth" %in% names(data) && length(data$linewidth) > 0) data$linewidth[1] else 0.3
  alpha_val  <- if ("alpha" %in% names(data) && !all(is.na(data$alpha))) data$alpha[1] else NA

  list(
    type   = "bar",
    x      = x_centre,
    y      = data$ymax - data$ymin,
    base   = data$ymin,
    width  = width,
    text   = if ("text" %in% names(data)) as.character(data$text) else rep("", nrow(data)),
    hoverinfo = "text",
    name   = fill_col,
    legendgroup = fill_col,
    showlegend = FALSE,
    marker = list(
      color   = fill_col,
      opacity = if (is.na(alpha_val)) 1 else alpha_val,
      line    = list(color = line_col, width = (line_w_raw %||% 0.3) * 1.5)
    ),
    xaxis = params$xaxis %||% "x",
    yaxis = params$yaxis %||% "y"
  )
}
