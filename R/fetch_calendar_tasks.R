#' Fetch Calendar Tasks
#'
#' @description Fetches tasks from Nextcloud calendar
#'
#' @param calendar_url URL of Nextcloud calendar (as found by discover_calendars function)
#' @param username Nextcloud user name
#' @param password Nextcloud password
#' @return Data frame of tasks
#'
#' @export
fetch_calendar_tasks <- function(calendar_url, username, password) {
  calendar_url <- utils::URLencode(calendar_url)
  calendar_url <- gsub(" ", "%20", calendar_url)
  response <- tryCatch({
    httr::RETRY(
      "REPORT",
      url = calendar_url,
      httr::authenticate(username, password),
      httr::add_headers("Depth" = "1",
                  "Content-Type" = "application/xml; charset=utf-8"),
      body = '<?xml version="1.0" encoding="utf-8" ?>
<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
  <D:prop xmlns:D="DAV:">
    <D:getetag/>
    <C:calendar-data/>
  </D:prop>
  <C:filter>
    <C:comp-filter name="VCALENDAR">
      <C:comp-filter name="VTODO"/>
    </C:comp-filter>
  </C:filter>
</C:calendar-query>',
times = 2,
pause_min = 1
    )
  }, error = function(e) {
    warning("HTTP request failed: ", e$message)
    return(NULL)
  })
  if (is.null(response) || httr::status_code(response) != 207) {
    return(data.frame())
  }
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  doc <- xml2::read_xml(content)
  ns <- c(d = "DAV:",
          cal = "urn:ietf:params:xml:ns:caldav")
  responses <- xml2::xml_find_all(doc, "//d:response", ns = ns)
  tasks <- list()
  for (resp in responses) {
    href <- xml2::xml_text(xml2::xml_find_first(resp, ".//d:href", ns = ns))
    calendar_data_node <-
      xml2::xml_find_first(resp, ".//cal:calendar-data", ns = ns)
    if (is.na(calendar_data_node))
      next
    calendar_data <- xml2::xml_text(calendar_data_node)
    if (!is.na(calendar_data) &&
        grepl("BEGIN:VTODO", calendar_data)) {
      task <- tryCatch({
        parse_icalendar_task(calendar_data)
      }, error = function(e) {
        parse_icalendar_simple(calendar_data)
      })
      task$url <- href
      tasks <- c(tasks, list(task))
    }
  }
  if (length(tasks) > 0) {
    tasks_df <- dplyr::bind_rows(tasks)
    return(tasks_df)
  } else {
    return(data.frame())
  }
}
