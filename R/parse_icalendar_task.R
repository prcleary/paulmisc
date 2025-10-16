#' Parse iCalendar Task
#'
#' @description Parses iCalendar tasks
#'
#' @param ical_content iCal content
#' @return Data frame of tasks
#'
#' @export
parse_icalendar_task <- function(ical_content) {
  task <- list()
  ical_content <- gsub("\\r\\n[ \t]", "", ical_content)
  ical_content <- gsub("\\r\\n", "\n", ical_content)
  extract_property <- function(content, prop_name) {
    pattern <- paste0(prop_name, "[:]([\\s\\S]*?)(?=\\n[A-Z]|\\n\\n|$)")
    match <-
      regmatches(content, regexpr(pattern, content, perl = TRUE))
    if (length(match) > 0) {
      value <- gsub(paste0(prop_name, ":"), "", match[1])

      value <- gsub("\\n", " ", value)
      value <- trimws(value)
      return(value)
    }
    return(NA)
  }
  task$uid <- extract_property(ical_content, "UID")
  properties <-
    c(
      "SUMMARY",
      "DESCRIPTION",
      "STATUS",
      "PRIORITY",
      "DUE",
      "CREATED",
      "LAST-MODIFIED",
      "PERCENT-COMPLETE",
      "DTSTART",
      "DTEND",
      "COMPLETED"
    )
  for (prop in properties) {
    task[[tolower(prop)]] <- extract_property(ical_content, prop)
  }
  if (is.na(task$due)) {
    due_patterns <-
      c("DUE;VALUE=DATE[:]([0-9]{8})",
        "DUE[:]([0-9]{8}T[0-9]{6}Z?)")
    for (pattern in due_patterns) {
      match <-
        regmatches(ical_content, regexpr(pattern, ical_content, perl = TRUE))
      if (length(match) > 0) {
        task$due <- gsub(".*[:]", "", match[1])
        break
      }
    }
  }
  date_cleanup <- function(date_str) {
    if (is.na(date_str))
      return(NA)
    date_str <- gsub("T.*", "", date_str)
    if (grepl("^[0-9]{8}$", date_str)) {
      date_str <- paste0(substr(date_str, 1, 4),
                         "-",
                         substr(date_str, 5, 6),
                         "-",
                         substr(date_str, 7, 8))
    }
    return(date_str)
  }
  date_fields <-
    c("due",
      "created",
      "last-modified",
      "dtstart",
      "dtend",
      "completed")
  for (field in date_fields) {
    if (!is.null(task[[field]])) {
      task[[field]] <- date_cleanup(task[[field]])
    }
  }
  cat("Task UID:", task$uid, "\n")
  cat("Found DUE:", task$due, "\n")
  return(as.data.frame(task, stringsAsFactors = FALSE))
}
