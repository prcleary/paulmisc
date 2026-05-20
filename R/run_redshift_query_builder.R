#' Run Redshift SQL Query Builder Shiny App
#'
#' Launches an interactive Shiny application for building Amazon Redshift SQL
#' queries without writing code. The app provides an intuitive interface with
#' form-based inputs for constructing complex SQL queries including table
#' selection, column specifications, WHERE conditions, date filters, GROUP BY,
#' ORDER BY, and more.
#'
#' @param background Logical. If `TRUE` (default), runs the app in a
#'   background R process (requires the `callr` package), allowing continued
#'   use of the console. If `FALSE`, blocks the console until the app is
#'   stopped.
#' @param ... Additional arguments passed to [shiny::runApp()] when
#'   `background = FALSE`.
#'
#' @return If `background = FALSE`, invisibly returns the result of
#'   [shiny::runApp()]. If `background = TRUE` (default), returns a process
#'   handle from [callr::r_bg()] which can be used to monitor or terminate
#'   the background process.
#'
#' @details
#' The Redshift SQL Query Builder includes:
#' \itemize{
#'   \item **Table Selection**: Specify schema, table name, and optional alias
#'   \item **Column Selection**: Choose all columns, specific columns, or
#'     aggregate functions (COUNT, SUM, AVG, MIN, MAX, COUNT DISTINCT)
#'   \item **WHERE Conditions**: Add up to 3 conditions with AND/OR logic,
#'     supporting operators like =, !=, >, <, LIKE, IN, BETWEEN, IS NULL, etc.
#'   \item **Date Filters**: Filter by date ranges, last N days, current
#'     month/year, or specific dates using Redshift functions
#'   \item **Sorting & Grouping**: GROUP BY, HAVING, ORDER BY, LIMIT, OFFSET
#'   \item **Validation**: Real-time error checking and helpful messages
#'   \item **Copy to Clipboard**: One-click copy of generated SQL
#' }
#'
#' @note This app requires the `shiny` and `bslib` packages. Running in
#'   background mode (default) requires the `callr` package.
#'
#' @examples
#' if (interactive()) {
#'   # Run in background (default, frees console, requires callr)
#'   app_process <- run_redshift_query_builder()
#'   # To stop: app_process$kill()
#'   
#'   # Run in foreground (blocks console)
#'   # run_redshift_query_builder(background = FALSE)
#' }
#'
#' @importFrom shiny runApp
#' @export
run_redshift_query_builder <- function(background = TRUE, ...) {
  app_dir <- system.file(
    "apps", "redshift-sql-query-builder",
    package = "paulmisc"
  )
  
  if (app_dir == "") {
    stop(
      "Could not find Redshift SQL Query Builder app directory. ",
      "Try re-installing `paulmisc`.",
      call. = FALSE
    )
  }
  
  if (background) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop(
        "Package 'callr' is required for background mode. ",
        "Install it with: install.packages('callr')",
        call. = FALSE
      )
    }
    
    message(
      "Starting Redshift SQL Query Builder in background process...\n",
      "To stop the app, use: app_process$kill()"
    )
    
    return(
      callr::r_bg(
        func = function(app_dir) {
          shiny::runApp(app_dir, launch.browser = TRUE)
        },
        args = list(app_dir = app_dir)
      )
    )
  }
  
  # Default to opening in browser for foreground mode too
  if (is.null(list(...)$launch.browser)) {
    shiny::runApp(app_dir, launch.browser = TRUE, ...)
  } else {
    shiny::runApp(app_dir, ...)
  }
}
