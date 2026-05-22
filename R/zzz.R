.onLoad <- function(libname, pkgname) {
  # Register plotly `to_basic` / `geom2trace` S3 methods for our custom
  # Geom subclasses. plotly dispatches on the *first* class of the Geom
  # ggproto object, so `GeomEpicurveRect` / `GeomEpicurveText` don't pick
  # up the inherited `to_basic.GeomRect` / `geom2trace.GeomText` methods
  # and ggplotly() would otherwise warn
  # "geom_GeomEpicurveRect() has yet to be implemented in plotly"
  # and produce an empty trace.
  #
  # We intentionally delegate to plotly's stock methods rather than
  # synthesise our own bar/scatter traces: plotly's polygon renderer
  # already handles stacking, faceting, fills and date axes correctly.
  # Per-bar hover text is recovered in `epicurve_ggplotly()` by
  # post-processing the build (no overriding of `geom2trace`).
  if (requireNamespace("plotly", quietly = TRUE)) {
    rect_method <- tryCatch(
      utils::getFromNamespace("to_basic.GeomRect", "plotly"),
      error = function(e) NULL
    )
    text_trace_method <- tryCatch(
      utils::getFromNamespace("geom2trace.GeomText", "plotly"),
      error = function(e) NULL
    )
    if (!is.null(rect_method)) {
      registerS3method(
        "to_basic", "GeomEpicurveRect", rect_method,
        envir = asNamespace("plotly")
      )
    }
    if (!is.null(text_trace_method)) {
      registerS3method(
        "geom2trace", "GeomEpicurveText", text_trace_method,
        envir = asNamespace("plotly")
      )
    }
  }
}
