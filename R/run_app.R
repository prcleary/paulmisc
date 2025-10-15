#' Run a Shiny app from the package
#'
#' @param app_name The name of the app folder in inst/apps/
#' @export
run_app <- function(app_name) {
  app_dir <- system.file("apps", app_name, package = "paulmisc")
  if (app_dir == "") stop("App not found: ", app_name, call. = FALSE)
  shiny::runApp(app_dir, display.mode = "normal")
}
