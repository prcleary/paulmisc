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
#' p <- ggplot(cases, aes(x = onset_date, fill = age_group)) +
#'   geom_epicurve() +
#'   theme_minimal()
#' ggplotly(p)
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
#' @param width Numeric width of each case square in x-axis units. For
#'   daily date data this is in days; defaults to `0.9`.
#' @param height Numeric height of each case square in y-axis units;
#'   defaults to `0.9`. Values below 1 produce visible gaps between
#'   stacked cases.
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
#' # Minimal epicurve
#' ggplot(cases, aes(x = onset_date)) +
#'   geom_epicurve() +
#'   theme_minimal()
#'
#' # Coloured by age group, faceted by setting
#' ggplot(cases, aes(x = onset_date, fill = age_group)) +
#'   geom_epicurve(colour = "grey20") +
#'   facet_wrap(~ setting, ncol = 1) +
#'   scale_fill_brewer(palette = "Set2") +
#'   theme_bw()
#'
#' @seealso [simulate_outbreak()] for generating example data.
#'
#' @importFrom ggplot2 layer ggproto Stat aes
#' @export
geom_epicurve <- function(mapping = NULL,
                          data = NULL,
                          stat = "epicurve",
                          position = "identity",
                          ...,
                          width = 0.9,
                          height = 0.9,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    geom        = "rect",
    mapping     = mapping,
    data        = data,
    stat        = stat,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      width = width,
      height = height,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_epicurve
#' @export
stat_epicurve <- function(mapping = NULL,
                          data = NULL,
                          geom = "rect",
                          position = "identity",
                          ...,
                          width = 0.9,
                          height = 0.9,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    stat        = StatEpicurve,
    data        = data,
    mapping     = mapping,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      width = width,
      height = height,
      na.rm = na.rm,
      ...
    )
  )
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

  compute_panel = function(self, data, scales, na.rm = FALSE, width = 0.9, height = 0.9) {
    data <- data[order(data$x, data$group), , drop = FALSE]
    data$y <- stats::ave(seq_len(nrow(data)), data$x, FUN = seq_along)
    
    # Compute rectangle boundaries for geom_rect
    # (This allows plotly to recognize and convert the geom)
    data$xmin <- data$x - width / 2
    data$xmax <- data$x + width / 2
    data$ymin <- pmax(0, data$y - 1 + (1 - height) / 2)
    data$ymax <- data$y - (1 - height) / 2
    
    # Add padding points to ensure x-axis includes full width of edge rectangles
    # and a point at y=0 to ensure y-axis includes 0 for proper display
    if (nrow(data) > 0) {
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
    data <- data[data$y > 0, , drop = FALSE]
    
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
