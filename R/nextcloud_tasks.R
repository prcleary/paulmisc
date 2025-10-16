# nextcloud_tasks.R
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
  content <- content(response, as = "text", encoding = "UTF-8")
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
