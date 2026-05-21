#' Annotate epidemic curves with events and periods
#'
#' Helper functions to add contextual annotations to epidemic curves, such as
#' intervention dates (events) or exposure periods (shaded regions).
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
#' @param label_vjust Vertical justification for label (default: -0.5 for events,
#'   -0.5 for periods)
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
#' @name annotate_epicurve
#' @importFrom ggplot2 geom_vline geom_rect annotate
NULL

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
                          label_vjust = -0.5,
                          label_size = 3.5,
                          ...) {
  
  # Handle American spelling
  if (!is.null(color)) {
    colour <- color
  }
  
  # Convert label_y shortcuts
  if (is.character(label_y)) {
    label_y <- switch(
      tolower(label_y),
      "top" = Inf,
      "bottom" = -Inf,
      "middle" = 0,
      Inf  # default to top if unrecognized
    )
  }
  
  list(
    ggplot2::geom_vline(
      xintercept = date,
      linetype = linetype,
      colour = colour,
      linewidth = linewidth,
      ...
    ),
    ggplot2::annotate(
      "text",
      x = date,
      y = label_y,
      label = label,
      hjust = label_hjust,
      vjust = label_vjust,
      colour = colour,
      size = label_size
    )
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
                           label_vjust = -0.5,
                           label_size = 3.5,
                           ...) {
  
  if (missing(end_date)) {
    stop("end_date is required for annotate_period()", call. = FALSE)
  }
  
  # Handle American spelling
  if (!is.null(color)) {
    colour <- color
  }
  
  # Convert label_y shortcuts
  if (is.character(label_y)) {
    label_y <- switch(
      tolower(label_y),
      "top" = Inf,
      "bottom" = -Inf,
      "middle" = 0,
      Inf  # default to top if unrecognized
    )
  }
  
  # Calculate midpoint for label
  if (inherits(date, "POSIXt") || inherits(end_date, "POSIXt")) {
    # For POSIXct, compute numeric midpoint and convert back
    mid_date <- as.POSIXct((as.numeric(date) + as.numeric(end_date)) / 2, 
                           origin = "1970-01-01", tz = attr(date, "tzone") %||% "UTC")
  } else {
    # For Date or numeric
    mid_date <- (as.numeric(date) + as.numeric(end_date)) / 2
    if (inherits(date, "Date")) {
      mid_date <- as.Date(mid_date, origin = "1970-01-01")
    }
  }
  
  list(
    ggplot2::geom_rect(
      xmin = date,
      xmax = end_date,
      ymin = -Inf,
      ymax = Inf,
      fill = fill,
      colour = colour,
      alpha = alpha,
      inherit.aes = FALSE,
      ...
    ),
    ggplot2::annotate(
      "text",
      x = mid_date,
      y = label_y,
      label = label,
      hjust = label_hjust,
      vjust = label_vjust,
      colour = if (is.na(colour)) "black" else colour,
      size = label_size
    )
  )
}
