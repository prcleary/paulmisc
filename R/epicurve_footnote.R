#' Summary footnote for an epicurve
#'
#' Convenience helper that builds a `caption` summarising the missing-data
#' status of an epidemic curve and the time the chart was produced. Add the
#' result to a ggplot with `+`, e.g.
#'
#' ```r
#' ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve() +
#'   epicurve_footnote(cases)
#' ```
#'
#' By default the footnote reports the proportion of rows with at least one
#' missing value across the supplied columns (or all columns when
#' `columns = NULL`) and stamps the chart with the current time. The text
#' can be customised or extended via `extra`.
#'
#' @param data A data frame, typically the same one used to build the plot.
#' @param columns Optional character vector of columns to consider when
#'   summarising missingness. Defaults to all non-ID columns in `data`.
#' @param show_missing Logical. Whether to include a missing-data summary
#'   (default `TRUE`).
#' @param show_timestamp Logical. Whether to include a "produced at" stamp
#'   (default `TRUE`).
#' @param timestamp_format A `format()` template used for the timestamp
#'   (default `"\%d \%B \%Y \%H:\%M"`).
#' @param extra Optional character string appended verbatim to the caption.
#' @param ... Additional arguments passed to [ggplot2::labs()].
#'
#' @return A [ggplot2::labs()] object suitable for adding to a ggplot.
#'
#' @examples
#' library(ggplot2)
#' cases <- simulate_outbreak(n = 50, seed = 1)
#' ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve(fill = "steelblue") +
#'   epicurve_footnote(cases)
#'
#' @importFrom ggplot2 labs
#' @export
epicurve_footnote <- function(data,
                              columns = NULL,
                              show_missing = TRUE,
                              show_timestamp = TRUE,
                              timestamp_format = "%d %B %Y %H:%M",
                              extra = NULL,
                              ...) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  parts <- character(0)

  if (isTRUE(show_missing)) {
    cols <- columns %||% setdiff(names(data), c("case_id", "id"))
    cols <- intersect(cols, names(data))
    if (length(cols) > 0 && nrow(data) > 0) {
      na_per_col <- vapply(data[, cols, drop = FALSE],
                           function(v) sum(is.na(v)), integer(1))
      any_missing <- rowSums(is.na(data[, cols, drop = FALSE])) > 0
      pct_rows <- round(100 * mean(any_missing), 1)
      worst <- if (any(na_per_col > 0)) {
        idx <- which.max(na_per_col)
        paste0(" \u2014 worst: ", names(na_per_col)[idx],
               " (", na_per_col[idx], "/", nrow(data), ")")
      } else {
        ""
      }
      parts <- c(parts, sprintf(
        "Missing data: %s%% of rows have \u22651 missing value%s.",
        format(pct_rows, nsmall = 1), worst
      ))
    } else {
      parts <- c(parts, "Missing data: none.")
    }
  }

  if (isTRUE(show_timestamp)) {
    parts <- c(parts, paste0(
      "Produced ", format(Sys.time(), timestamp_format)
    ))
  }

  if (!is.null(extra)) {
    parts <- c(parts, as.character(extra))
  }

  caption <- paste(parts, collapse = " \u00b7 ")
  ggplot2::labs(caption = caption, ...)
}
