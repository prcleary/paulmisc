#' Get Nextcloud Tasks
#'
#' @description Gets Nextcloud tasks from a specified calendar.
#'
#' @param calendar_url URL of the specific Nextcloud calendar DAV endpoint e.g. 'https://nextcloud.domain.tld/remote.php/dav/calendars/admin'
#' @param username Nextcloud user name
#' @param password Nextcloud password
#' @return Data frame of tasks
#'
#' @export
get_nextcloud_tasks <- function(calendar_url, username, password) {
  calendar_url <- gsub("/+$", "", calendar_url)
  if (!grepl("^https?://", calendar_url)) {
    stop("Calendar URL must start with http:// or https://")
  }
  
  # Construct the full URL to the calendar endpoint
  calendar_url <- paste0(calendar_url, "/calendars/", username, "/")

  tasks <- tryCatch({
    fetch_calendar_tasks(calendar_url, username, password)
  }, error = function(e) {
    warning("Failed to fetch tasks: ", e$message)
    return(data.frame())
  })
  
  if (nrow(tasks) > 0) {
    tasks$calendar <- "Nextcloud Calendar"
  }
  
  return(tasks)
}
