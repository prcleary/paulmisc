get_work_calendar <- function(start_day = Sys.Date(),
                              start_time = "00:00:01",
                              end_day = Sys.Date() + 1,
                              end_time = "23:59:59") {
  require(RDCOMClient)

  # testing
  start_day <- Sys.Date()
  start_time <- "00:00:01"
  end_day <- Sys.Date() + 2
  end_time <- "23:59:59"
  start_time <- as.POSIXct(paste(start_day, start_time))
  end_time <- as.POSIXct(paste(end_day, end_time))
  start_time_outlook <- format(start_time, '%Y-%m-%d %H:%M:%S')
  end_time_outlook <- format(end_time, '%Y-%m-%d %H:%M:%S')

  outlook_app <- COMCreate("Outlook.Application")
  namespace <- outlook_app$GetNamespace("MAPI")
  calendar <- namespace$GetDefaultFolder(9)
  items <- calendar$Items()
  items$Sort("[Start]")
  items[['IncludeRecurrences']] <- TRUE
  items_count <- items$Count()

  calendar_DT <- data.table("Start" = rep(NA_real_, items_count),
                            "End" = rep(NA_real_, items_count),
                            "Subject" = rep(NA_character_, items_count))
  for (i in seq_len(items$Count())) {
    item <- items$Item(i)
    set(calendar_DT, i = i, j = 1L, item$Start())
    set(calendar_DT, i = i, j = 2L, item$End())
    set(calendar_DT, i = i, j = 3L, item$Subject())


  }
}
