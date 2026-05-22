.onLoad <- function(libname, pkgname) {
  # Register plotly `to_basic` S3 methods for our custom Geom subclasses.
  # plotly dispatches on the *first* class of the Geom ggproto object, so
  # `GeomEpicurveRect` / `GeomEpicurveText` don't pick up the inherited
  # `to_basic.GeomRect` / `to_basic.GeomText` methods and ggplotly() would
  # otherwise warn "geom_GeomEpicurveRect() has yet to be implemented in
  # plotly" and produce an empty trace.
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
