#' Get Nextcloud Tasks
#'
#' @description Gets Nextcloud tasks.
#'
#' @param base_url URL of Nextcloud instance DAV endpoint e.g. 'https://nextcloud.domain.tld/remote.php/dav'
#' @param username Nextcloud user name
#' @param password Nextcloud password
#' @return Data frame of tasks
#'
#' @export
get_nextcloud_tasks <- function(base_url, username, password) {
  base_url <- gsub("/+$", "", base_url)
  if (!grepl("^https?://", base_url)) {
    stop("Base URL must start with http:// or https://")
  }
  calendars <- tryCatch({
    discover_calendars(base_url, username, password)
  }, error = function(e) {
    warning("Calendar discovery failed: ", e$message)
    return(data.frame())
  })
  if (nrow(calendars) == 0) {
    return(data.frame())
  }
  all_tasks <- data.frame()
  for (i in 1:nrow(calendars)) {
    calendar <- calendars[i,]
    cat("Processing calendar:", calendar$displayname, "\n")
    tasks <- tryCatch({
      fetch_calendar_tasks(calendar$url, username, password)
    }, error = function(e) {
      warning("Failed to fetch tasks: ", e$message)
      data.frame()
    })
    if (nrow(tasks) > 0) {
      tasks$calendar <- calendar$displayname
      all_tasks <- dplyr::bind_rows(all_tasks, tasks)
    }
  }
  return(all_tasks)
}
