# library(shiny)
# library(bslib)
# library(clipr)

# Function definitions

daynote <- dn <- function(minutes_available = 16 * 60,
                          work_start = '06:00',
                          time_format = '%H:%M',
                          work_length = 25,
                          rest_length = 5,
                          breaks_every = 5) {
  work_start <- strptime(work_start, time_format)
  chunk_length <- work_length + rest_length
  n_chunks <- round(minutes_available / chunk_length)
  pomo_work <- work_start + seq(0, minutes_available - chunk_length, by = chunk_length) * 60
  pomo_rest <- pomo_work + work_length * 60
  longer_breaks <- seq(0, n_chunks, breaks_every)
  results <- data.frame(
    Start = format(pomo_work, time_format),
    End = format(pomo_rest, time_format),
    Note = ''
  )
  results$Note[longer_breaks] <- 'Longer break'

  output <- c(
    paste('#', format(Sys.Date(), '%Y-%m-%d'), 'daily planner'),
    '',
    '## Work hours',
    '',
    '<<< paste randomised tasks here >>>',
    '',
    knitr::kable(results, format = "pipe"),
    '',
    '## Notes',
    '',
    '- '
  )

  paste(output, collapse = "\n")
}

ui <- bslib::page_navbar(
  theme = bslib::bs_theme(bg = "#101010",
                          fg = "#FFF",
                          primary = "#E69F00",
                          secondary = "#0072B2",
                          success = "#009E73",
                          base_font = bslib::font_google("Inter"),
                          code_font = bslib::font_google("JetBrains Mono")),
  title = "Task and Day Note Generator",


  bslib::nav_panel("Day Note", bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::numericInput(
        "minutesAvailable",
        "Minutes Available",
        value = 16 * 60,
        min = 1
      ),
      shiny::textInput("workStart", "Work Start Time", value = "06:00"),
      shiny::numericInput(
        "workLength",
        "Work Length (minutes)",
        value = 25,
        min = 1
      ),
      shiny::numericInput(
        "restLength",
        "Rest Length (minutes)",
        value = 5,
        min = 1
      ),
      shiny::numericInput(
        "breaksEvery",
        "Breaks Every (pomodoros)",
        value = 5,
        min = 1
      ),
      shiny::actionButton("generateDayNote", "Generate Day Note")
    ),
    bslib::card(
      bslib::card_header("Day Note Output"),
      bslib::card_body(
        shiny::verbatimTextOutput("dayNoteOutput"),
        shiny::actionButton("copyDayNote", "Copy to Clipboard")
      )
    )
  )),

  bslib::nav_panel("Random Tasks", bslib::layout_sidebar(
    sidebar = bslib::sidebar(actionButton(
      "generateRandomTasks", "Generate Random Tasks"
    )), bslib::card(
      bslib::card_header("Random Tasks Output"),
      bslib::card_body(
        shiny::uiOutput("randomTasksOutput"),
        shiny::actionButton("copyRandomTasks", "Copy to Clipboard")
      )
    )
  )),
)

server <- function(input, output, session) {
  randomTasksResult <- shiny::reactiveVal("")
  dayNoteResult <- shiny::reactiveVal("")

  shiny::observeEvent(input$generateRandomTasks, {
    result <- rand_cb_tasks()
    randomTasksResult(result)
    output$randomTasksOutput <- shiny::renderUI({
      shiny::HTML(markdown::markdownToHTML(text = result, fragment.only = TRUE))
    })
  })

  shiny::observeEvent(input$generateDayNote, {

    result <- daynote(
      minutes_available = input$minutesAvailable,
      work_start = input$workStart,
      work_length = input$workLength,
      rest_length = input$restLength,
      breaks_every = input$breaksEvery
    )
    dayNoteResult(result)
    output$dayNoteOutput <- shiny::renderText({
      result
    })
  })

  shiny::observeEvent(input$copyRandomTasks, {
    clipr::write_clip(randomTasksResult(), allow_non_interactive = TRUE)
    shiny::showNotification("Random Tasks copied to clipboard!", type = "message")
  })

  shiny::observeEvent(input$copyDayNote, {
    clipr::write_clip(dayNoteResult(), allow_non_interactive = TRUE)
    shiny::showNotification("Day Note copied to clipboard!", type = "message")
  })
}

shiny::shinyApp(ui, server)
