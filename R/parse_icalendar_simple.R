#' Simple Parsing Of iCal Content
#'
#' @description Simple parsing of iCal content.
#'
#' @param ical_content iCal content
#' @return Data frame of tasks
#'
#' @export
parse_icalendar_simple <- function(ical_content) {
  lines <- unlist(strsplit(ical_content, "\n"))
  task <- list()
  for (line in lines) {
    if (grepl("^SUMMARY:", line)) {
      task$summary <- gsub("SUMMARY:", "", line)
    } else if (grepl("^DESCRIPTION:", line)) {
      task$description <- gsub("DESCRIPTION:", "", line)
    } else if (grepl("^STATUS:", line)) {
      task$status <- gsub("STATUS:", "", line)
    } else if (grepl("^PRIORITY:", line)) {
      task$priority <- gsub("PRIORITY:", "", line)
    } else if (grepl("^DUE", line)) {
      task$due <- gsub(".*DUE[^:]*:", "", line)
    } else if (grepl("^UID:", line)) {
      task$uid <- gsub("UID:", "", line)
    }
  }
  if (!is.null(task$due)) {
    task$due <- gsub("T.*", "", task$due)
    if (grepl("^[0-9]{8}$", task$due)) {
      task$due <- paste0(substr(task$due, 1, 4),
                         "-",
                         substr(task$due, 5, 6),
                         "-",
                         substr(task$due, 7, 8))
    }
  }
  cat("Simple parser found DUE:", task$due, "\n")
  return(as.data.frame(task, stringsAsFactors = FALSE))
}
