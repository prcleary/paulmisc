#' Get Nextcloud Tasks
#'
#' @description Gets Nextcloud tasks from a specified calendar.
#'
#' @param calendar_url URL of the specific Nextcloud calendar DAV endpoint e.g. 'https://nextcloud.domain.tld/remote.php/dav/calendars/admin'. Defaults to environment variable 'NEXTCLOUD_BASE_URL'.
#' @param username Nextcloud user name. Defaults to environment variable 'NEXTCLOUD_USERNAME'.
#' @param password Nextcloud password. Defaults to environment variable 'NEXTCLOUD_PASSWORD'.
#' @param exclude_status Status values to exclude (default is "COMPLETED")
#' @param exclude_priority Priority values to exclude (default is NA)
#' @return Data frame of tasks
#'
#' @export
get_nextcloud_tasks <- function(calendar_url = Sys.getenv("NEXTCLOUD_BASE_URL"),
                                username = Sys.getenv("NEXTCLOUD_USERNAME"),
                                password = Sys.getenv("NEXTCLOUD_PASSWORD"),
                                exclude_status = "COMPLETED",
                                exclude_priority = NA) {
  calendar_url <- gsub("/+$", "", calendar_url)
  if (!grepl("^https?://", calendar_url)) {
    stop("Calendar URL must start with http:// or https://")
  }
  
  # Discover the correct calendar URL
  calendars <- discover_calendars(calendar_url, username, password)
  task_calendars <- calendars[calendars$type == "tasks", ]
  if (nrow(task_calendars) == 0) {
    warning("No task calendar found.")
    return(data.frame())
  }
  all_tasks <- lapply(task_calendars$url, function(url) {
    tryCatch({
      fetch_calendar_tasks(url, username, password)
    }, error = function(e) {
      warning("Failed to fetch tasks from ", url, ": ", e$message)
      return(data.frame())
    })
  })
  
  all_tasks_df <- do.call(rbind, all_tasks)
  
  if (nrow(all_tasks_df) > 0) {
    all_tasks_df$calendar <- task_calendars$displayname[match(all_tasks_df$url, task_calendars$url)]
  }
  
  # Filter tasks based on status and priority
  filtered_tasks_df <- all_tasks_df %>%
    dplyr::filter(!(status %in% exclude_status)) %>%
    dplyr::filter(!is.na(priority) & priority != exclude_priority)
  
  return(filtered_tasks_df)
}
