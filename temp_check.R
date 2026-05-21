library(paulmisc); library(ggplot2); library(plotly); set.seed(123)
`%||%` <- function(a,b) if (is.null(a)) b else a
cases <- simulate_outbreak(n = 50)
p <- ggplot(cases, aes(x = onset_date)) +
  geom_epicurve(fill = "steelblue") +
  annotate_period(date = as.Date("2024-05-28"), end_date = as.Date("2024-06-02"), label = "Exposure period", fill = "yellow", alpha = 0.25) +
  annotate_event(date = as.Date("2024-06-05"), label = "Investigation", colour = "red") +
  scale_y_epicurve() + theme_minimal()
pl <- ggplotly(p, tooltip = "text")
for (i in seq_along(pl$x$data)) {
  tr <- pl$x$data[[i]]
  cat(sprintf("Trace %d (mode=%s) y=%s text=%s\n", i, tr$mode %||% "NA",
              paste(head(tr$y, 3), collapse=","),
              paste(head(tr$text, 1), collapse="")))
}