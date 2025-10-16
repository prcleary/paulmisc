#' Discover Nextcloud Calendars
#'
#' @description Discovers Nextcloud calendars.
#'
#' @param base_url URL of Nextcloud instance DAV endpoint e.g. 'https://nextcloud.domain.tld/remote.php/dav'
#' @param username Nextcloud user name
#' @param password Nextcloud password
#' @return Data frame of Nextcloud calendars
#'
#' @export
discover_calendars <- function(base_url, username, password) {
  base_url <- gsub("/+$", "", base_url)
  cal_url <- paste0(base_url, "/calendars/", username, "/")
  cat("Discovering from:", cal_url, "\n")
  response <- httr::RETRY(
    "PROPFIND",
    url = cal_url,
    httr::authenticate(username, password),
    httr::add_headers("Depth" = "1",
                "Content-Type" = "application/xml; charset=utf-8"),
    body = '<?xml version="1.0" encoding="utf-8" ?>
<d:propfind xmlns:d="DAV:" xmlns:cs="http://calendarserver.org/ns/"
            xmlns:c="urn:ietf:params:xml:ns:caldav">
  <d:prop>
    <d:resourcetype />
    <d:displayname />
    <c:supported-calendar-component-set />
  </d:prop>
</d:propfind>'
  )
  if (httr::status_code(response) != 207) {
    stop("Failed to discover calendars. Status: ",
         httr::status_code(response))
  }
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  doc <- xml2::read_xml(content)
  ns <- c(d = "DAV:",
          c = "urn:ietf:params:xml:ns:caldav",
          cs = "http://calendarserver.org/ns/")
  responses <- xml2::xml_find_all(doc, "//d:response", ns = ns)
  calendars <- data.frame(
    url = character(),
    displayname = character(),
    type = character(),
    stringsAsFactors = FALSE
  )
  for (resp in responses) {
    href <- xml2::xml_text(xml2::xml_find_first(resp, ".//d:href", ns = ns))
    if (!grepl("/calendars/", href) ||
        grepl("/principals/", href))
      next
    href <- utils::URLdecode(href)
    if (grepl("^https?://", href)) {
      final_url <- href
    } else if (grepl("^/remote.php/dav", href)) {
      server_root <- gsub("/remote.php/dav.*", "", base_url)
      final_url <- paste0(server_root, href)
    } else if (grepl("^/calendars/", href)) {
      final_url <- paste0(base_url, href)
    } else {
      final_url <- paste0(base_url, "/", href)
    }
    final_url <- gsub("//+", "/", final_url)
    final_url <- gsub(":/", "://", final_url)
    final_url <- gsub("/+$", "", final_url)
    displayname <- tryCatch({
      xml2::xml_text(xml2::xml_find_first(resp, ".//d:displayname", ns = ns))
    }, error = function(e)
      basename(href))
    comp_nodes <- tryCatch({
      xml2::xml_find_all(resp, ".//c:comp", ns = ns)
    }, error = function(e)
      NULL)
    components <- if (!is.null(comp_nodes)) {
      xml2::xml_attr(comp_nodes, "name")
    } else {
      character()
    }
    calendar_type <-
      if ("VTODO" %in% components)
        "tasks"
    else
      "events"
    calendars <- rbind(
      calendars,
      data.frame(
        url = final_url,
        displayname = displayname,
        type = calendar_type,
        stringsAsFactors = FALSE
      )
    )
  }
  return(calendars)
}
