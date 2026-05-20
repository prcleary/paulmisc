#' Run Redshift SQL Query Builder Shiny App
#'
#' Launches an interactive Shiny application for building Amazon Redshift SQL
#' queries without writing code. The app provides an intuitive interface with
#' form-based inputs for constructing complex SQL queries including table
#' selection, column specifications, WHERE conditions, date filters, GROUP BY,
#' ORDER BY, and more.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return Invisibly returns the result of [shiny::shinyApp()].
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
#' @note This app requires the `shiny` and `bslib` packages.
#'
#' @examples
#' if (interactive()) {
#'   run_redshift_query_builder()
#' }
#'
#' @importFrom shiny shinyApp
#' @export
run_redshift_query_builder <- function(...) {
  app_dir <- system.file("apps", "redshift-sql-query-builder", package = "paulmisc")
  
  if (app_dir == "") {
    stop(
      "Could not find Redshift SQL Query Builder app directory. ",
      "Try re-installing `paulmisc`.",
      call. = FALSE
    )
  }
  
  shiny::shinyApp(
    ui = shiny::shinyAppDir(app_dir)$ui,
    server = shiny::shinyAppDir(app_dir)$server,
    ...
  )
}
