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
#'   the background process. The process is returned visibly so you can
#'   easily assign it to a variable.
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
#'   # Run in background (default) - IMPORTANT: assign to variable
#'   app <- run_redshift_query_builder()
#'   
#'   # Continue using console for other work...
#'   
#'   # Stop the app when done
#'   app$kill()
#'   
#'   # Run in foreground (blocks console)
#'   # run_redshift_query_builder(background = FALSE)
#' }
#'
#' @importFrom shiny runApp
#' @importFrom httpuv randomPort
#' @importFrom utils browseURL
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
    
    # Find an available port
    port <- httpuv::randomPort()
    url <- paste0("http://127.0.0.1:", port)
    
    message(
      "Starting Redshift SQL Query Builder in background process...\n",
      "URL: ", url, "\n",
      "Assign to a variable to stop later:\n",
      "  app <- run_redshift_query_builder()\n",
      "  app$kill()"
    )
    
    # Start app in background
    process <- callr::r_bg(
      func = function(app_dir, port) {
        shiny::runApp(app_dir, port = port, launch.browser = FALSE)
      },
      args = list(app_dir = app_dir, port = port)
    )
    
    # Give the app a moment to start, then open browser
    Sys.sleep(1)
    utils::browseURL(url)
    
    return(process)
  }
  
  # Default to opening in browser for foreground mode too
  if (is.null(list(...)$launch.browser)) {
    shiny::runApp(app_dir, launch.browser = TRUE, ...)
  } else {
    shiny::runApp(app_dir, ...)
  }
}
