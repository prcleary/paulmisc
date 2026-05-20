library(shiny)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(
    bg = "#1a1a2e",
    fg = "#eaeaea",
    primary = "#4fc3f7",
    secondary = "#7c4dff",
    success = "#66bb6a",
    info = "#29b6f6",
    warning = "#ffa726",
    danger = "#ef5350",
    base_font = font_google("Roboto"),
    code_font = font_google("Roboto Mono"),
    heading_font = font_google("Roboto")
  ),

  tags$head(
    tags$style(HTML("
      body {
        background-color: #1a1a2e;
        font-family: 'Roboto', sans-serif;
      }
      .main-container {
        padding: 20px;
        min-height: 100vh;
      }
      .panel {
        background-color: #16213e;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 15px;
        border: 1px solid #2a2a4a;
      }
      .panel-header {
        color: #4fc3f7;
        font-size: 18px;
        font-weight: 600;
        margin-bottom: 15px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4fc3f7;
      }
      .form-control, .selectize-input {
        background-color: #0f0f23 !important;
        border: 1px solid #3a3a5a !important;
        color: #eaeaea !important;
        border-radius: 8px !important;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: #4fc3f7 !important;
        box-shadow: 0 0 0 2px rgba(79, 195, 247, 0.25) !important;
      }
      .btn-primary {
        background: linear-gradient(135deg, #4fc3f7 0%, #7c4dff 100%);
        border: none;
        border-radius: 8px;
        font-weight: 600;
        padding: 12px 24px;
        transition: all 0.3s ease;
      }
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(79, 195, 247, 0.4);
      }
      .btn-success {
        background: linear-gradient(135deg, #66bb6a 0%, #26a69a 100%);
        border: none;
        border-radius: 8px;
        font-weight: 600;
      }
      .sql-output {
        background-color: #0f0f23;
        border: 1px solid #3a3a5a;
        border-radius: 8px;
        padding: 15px;
        font-family: 'Roboto Mono', monospace;
        font-size: 14px;
        color: #66bb6a;
        white-space: pre-wrap;
        word-wrap: break-word;
        min-height: 200px;
        max-height: 400px;
        overflow-y: auto;
      }
      .error-panel {
        background-color: #2d1b1b;
        border: 1px solid #ef5350;
        border-radius: 8px;
        padding: 15px;
        min-height: 80px;
        max-height: 150px;
        overflow-y: auto;
      }
      .error-message {
        color: #ef5350;
        font-size: 14px;
        margin: 5px 0;
      }
      .success-message {
        color: #66bb6a;
        font-size: 14px;
      }
      .instructions {
        background-color: #1e3a5f;
        border-left: 4px solid #4fc3f7;
        padding: 15px;
        border-radius: 0 8px 8px 0;
        margin-bottom: 20px;
      }
      .instructions h5 {
        color: #4fc3f7;
        margin-bottom: 10px;
      }
      .instructions ul {
        margin: 0;
        padding-left: 20px;
        color: #b0b0b0;
      }
      .instructions li {
        margin-bottom: 5px;
      }
      .section-divider {
        border-top: 1px solid #3a3a5a;
        margin: 20px 0;
        padding-top: 15px;
      }
      .filter-row {
        background-color: #0f0f23;
        border-radius: 8px;
        padding: 10px;
        margin-bottom: 10px;
      }
      label {
        color: #b0b0b0;
        font-weight: 500;
        margin-bottom: 5px;
      }
      .app-title {
        color: #4fc3f7;
        font-size: 28px;
        font-weight: 700;
        text-align: center;
        margin-bottom: 5px;
      }
      .app-subtitle {
        color: #7a7a9a;
        text-align: center;
        margin-bottom: 20px;
      }
      .copy-btn-container {
        text-align: right;
        margin-top: 10px;
      }
      .keyword {
        color: #ff79c6;
      }
      .function-name {
        color: #8be9fd;
      }
      .string-val {
        color: #f1fa8c;
      }
      .number-val {
        color: #bd93f9;
      }
    "))
  ),

  div(class = "main-container",
      h1(class = "app-title", "Redshift SQL Query Builder"),
      p(class = "app-subtitle", "Build Amazon Redshift SQL queries without writing code"),

      div(class = "instructions",
          h5(icon("info-circle"), " Quick Start Guide"),
          tags$ul(
            tags$li("Enter your table name and optionally a schema name"),
            tags$li("Add column names separated by commas, or use * for all columns"),
            tags$li("Add WHERE conditions to filter your data"),
            tags$li("Use date filters for time-based queries"),
            tags$li("Add GROUP BY, ORDER BY, and LIMIT as needed"),
            tags$li("Click 'Generate Query' to create your SQL"),
            tags$li("Copy the generated query using the copy button")
          )
      ),

      fluidRow(
        column(5,
               div(class = "panel",
                   div(class = "panel-header", icon("table"), " Table Selection"),
                   textInput("schema_name", "Schema Name (optional)", placeholder = "e.g., public"),
                   textInput("table_name", "Table Name *", placeholder = "e.g., sales_data"),
                   textInput("table_alias", "Table Alias (optional)", placeholder = "e.g., s")
               ),

               div(class = "panel",
                   div(class = "panel-header", icon("columns"), " Column Selection"),
                   radioButtons("select_type", "Selection Type",
                                choices = c("All Columns (*)" = "all", "Specific Columns" = "specific", "Aggregate Functions" = "aggregate"),
                                inline = TRUE),
                   conditionalPanel(
                     condition = "input.select_type == 'specific'",
                     textAreaInput("columns", "Column Names (comma-separated)",
                                   placeholder = "e.g., id, name, created_at, amount",
                                   rows = 2),
                     checkboxInput("distinct", "SELECT DISTINCT", value = FALSE)
                   ),
                   conditionalPanel(
                     condition = "input.select_type == 'aggregate'",
                     selectInput("agg_function", "Aggregate Function",
                                 choices = c("COUNT" = "COUNT", "SUM" = "SUM", "AVG" = "AVG",
                                             "MIN" = "MIN", "MAX" = "MAX", "COUNT DISTINCT" = "COUNT_DISTINCT")),
                     textInput("agg_column", "Column for Aggregation", placeholder = "e.g., amount"),
                     textInput("agg_alias", "Result Alias (optional)", placeholder = "e.g., total_amount"),
                     textInput("group_columns", "Additional SELECT Columns (comma-separated)",
                               placeholder = "e.g., category, region")
                   )
               ),

               div(class = "panel",
                   div(class = "panel-header", icon("filter"), " WHERE Conditions"),

                   div(class = "filter-row",
                       h6("Condition 1", style = "color: #4fc3f7; margin-bottom: 10px;"),
                       fluidRow(
                         column(4, textInput("where_col1", "Column", placeholder = "column_name")),
                         column(4, selectInput("where_op1", "Operator",
                                               choices = c("=" = "=", "!=" = "!=", ">" = ">", "<" = "<",
                                                           ">=" = ">=", "<=" = "<=", "LIKE" = "LIKE",
                                                           "ILIKE" = "ILIKE", "IN" = "IN", "NOT IN" = "NOT IN",
                                                           "IS NULL" = "IS NULL", "IS NOT NULL" = "IS NOT NULL",
                                                           "BETWEEN" = "BETWEEN"))),
                         column(4, textInput("where_val1", "Value", placeholder = "value"))
                       ),
                       conditionalPanel(
                         condition = "input.where_op1 == 'BETWEEN'",
                         textInput("where_val1_end", "End Value (for BETWEEN)", placeholder = "end value")
                       )
                   ),

                   checkboxInput("add_condition2", "Add another condition", value = FALSE),
                   conditionalPanel(
                     condition = "input.add_condition2 == true",
                     div(class = "filter-row",
                         fluidRow(
                           column(12, selectInput("logic_op1", "Logic Operator", choices = c("AND", "OR"), width = "100px"))
                         ),
                         h6("Condition 2", style = "color: #4fc3f7; margin-bottom: 10px;"),
                         fluidRow(
                           column(4, textInput("where_col2", "Column", placeholder = "column_name")),
                           column(4, selectInput("where_op2", "Operator",
                                                 choices = c("=" = "=", "!=" = "!=", ">" = ">", "<" = "<",
                                                             ">=" = ">=", "<=" = "<=", "LIKE" = "LIKE",
                                                             "ILIKE" = "ILIKE", "IN" = "IN", "NOT IN" = "NOT IN",
                                                             "IS NULL" = "IS NULL", "IS NOT NULL" = "IS NOT NULL",
                                                             "BETWEEN" = "BETWEEN"))),
                           column(4, textInput("where_val2", "Value", placeholder = "value"))
                         ),
                         conditionalPanel(
                           condition = "input.where_op2 == 'BETWEEN'",
                           textInput("where_val2_end", "End Value (for BETWEEN)", placeholder = "end value")
                         )
                     )
                   ),

                   checkboxInput("add_condition3", "Add a third condition", value = FALSE),
                   conditionalPanel(
                     condition = "input.add_condition3 == true",
                     div(class = "filter-row",
                         fluidRow(
                           column(12, selectInput("logic_op2", "Logic Operator", choices = c("AND", "OR"), width = "100px"))
                         ),
                         h6("Condition 3", style = "color: #4fc3f7; margin-bottom: 10px;"),
                         fluidRow(
                           column(4, textInput("where_col3", "Column", placeholder = "column_name")),
                           column(4, selectInput("where_op3", "Operator",
                                                 choices = c("=" = "=", "!=" = "!=", ">" = ">", "<" = "<",
                                                             ">=" = ">=", "<=" = "<=", "LIKE" = "LIKE",
                                                             "ILIKE" = "ILIKE", "IN" = "IN", "NOT IN" = "NOT IN",
                                                             "IS NULL" = "IS NULL", "IS NOT NULL" = "IS NOT NULL",
                                                             "BETWEEN" = "BETWEEN"))),
                           column(4, textInput("where_val3", "Value", placeholder = "value"))
                         ),
                         conditionalPanel(
                           condition = "input.where_op3 == 'BETWEEN'",
                           textInput("where_val3_end", "End Value (for BETWEEN)", placeholder = "end value")
                         )
                     )
                   )
               ),

               div(class = "panel",
                   div(class = "panel-header", icon("calendar"), " Date Filters"),
                   checkboxInput("use_date_filter", "Enable Date Filter", value = FALSE),
                   conditionalPanel(
                     condition = "input.use_date_filter == true",
                     textInput("date_column", "Date Column Name", placeholder = "e.g., created_at"),
                     selectInput("date_filter_type", "Date Filter Type",
                                 choices = c("Date Range" = "range", "Last N Days" = "last_n_days",
                                             "Current Month" = "current_month", "Current Year" = "current_year",
                                             "Specific Date" = "specific")),
                     conditionalPanel(
                       condition = "input.date_filter_type == 'range'",
                       dateInput("date_start", "Start Date", value = Sys.Date() - 30),
                       dateInput("date_end", "End Date", value = Sys.Date())
                     ),
                     conditionalPanel(
                       condition = "input.date_filter_type == 'last_n_days'",
                       numericInput("last_n_days", "Number of Days", value = 30, min = 1)
                     ),
                     conditionalPanel(
                       condition = "input.date_filter_type == 'specific'",
                       dateInput("specific_date", "Specific Date", value = Sys.Date())
                     )
                   )
               ),

               div(class = "panel",
                   div(class = "panel-header", icon("sort"), " Sorting & Grouping"),
                   textInput("group_by", "GROUP BY (comma-separated columns)", placeholder = "e.g., category, region"),
                   textInput("having", "HAVING Condition (for aggregates)", placeholder = "e.g., COUNT(*) > 10"),
                   textInput("order_by", "ORDER BY (comma-separated columns)", placeholder = "e.g., created_at DESC, name ASC"),
                   numericInput("limit_rows", "LIMIT (number of rows)", value = NULL, min = 1),
                   numericInput("offset_rows", "OFFSET (skip rows)", value = NULL, min = 0)
               ),

               div(style = "text-align: center; margin-top: 20px;",
                   actionButton("generate", "Generate Query",
                                icon = icon("code"),
                                class = "btn-primary btn-lg",
                                style = "width: 100%;")
               )
        ),

        column(7,
               div(class = "panel",
                   div(class = "panel-header", icon("code"), " Generated SQL Query"),
                   div(class = "sql-output", id = "sql-display",
                       uiOutput("sql_query")
                   ),
                   div(class = "copy-btn-container",
                       actionButton("copy_btn", "Copy to Clipboard",
                                    icon = icon("copy"),
                                    class = "btn-success",
                                    onclick = "copyToClipboard()")
                   )
               ),

               div(class = "panel",
                   div(class = "panel-header", style = "color: #ef5350;", icon("exclamation-triangle"), " Validation Messages"),
                   div(class = "error-panel",
                       uiOutput("validation_messages")
                   )
               ),

               div(class = "panel",
                   div(class = "panel-header", icon("lightbulb"), " Redshift SQL Tips"),
                   div(style = "color: #b0b0b0; font-size: 13px;",
                       tags$ul(
                         tags$li(tags$strong("ILIKE"), " - Case-insensitive pattern matching (Redshift-specific)"),
                         tags$li(tags$strong("GETDATE()"), " - Returns current date and time"),
                         tags$li(tags$strong("DATEADD()"), " - Add intervals to dates: DATEADD(day, -30, GETDATE())"),
                         tags$li(tags$strong("DATEDIFF()"), " - Difference between dates"),
                         tags$li(tags$strong("TRUNC()"), " - Truncate timestamps: TRUNC(date_column)"),
                         tags$li(tags$strong("NVL()"), " - Replace NULL values: NVL(column, 'default')"),
                         tags$li(tags$strong("LISTAGG()"), " - Aggregate strings: LISTAGG(column, ', ')"),
                         tags$li("Use ", tags$strong("single quotes"), " for string values"),
                         tags$li("Use ", tags$strong("double quotes"), " for identifiers with special characters")
                       )
                   )
               )
        )
      )
  ),

  tags$script(HTML("
    function copyToClipboard() {
      var sqlText = document.getElementById('sql-display').innerText;
      navigator.clipboard.writeText(sqlText).then(function() {
        var btn = document.querySelector('.btn-success');
        var originalText = btn.innerHTML;
        btn.innerHTML = '<i class=\"fa fa-check\"></i> Copied!';
        setTimeout(function() {
          btn.innerHTML = originalText;
        }, 2000);
      });
    }
  "))
)

server <- function(input, output, session) {

  # Reactive values to store query and errors
  query_result <- reactiveValues(
    sql = "-- Click 'Generate Query' to build your SQL",
    errors = list(),
    has_errors = FALSE
  )

  # Generate query when button is clicked
  observeEvent(input$generate, {
    errors <- list()

    # Validate table name
    if (is.null(input$table_name) || trimws(input$table_name) == "") {
      errors <- c(errors, "Table name is required. Please enter a valid table name.")
    }

    # Validate columns for specific selection
    if (input$select_type == "specific") {
      if (is.null(input$columns) || trimws(input$columns) == "") {
        errors <- c(errors, "Column names are required when selecting specific columns.")
      }
    }

    # Validate aggregate function inputs
    if (input$select_type == "aggregate") {
      if (is.null(input$agg_column) || trimws(input$agg_column) == "") {
        errors <- c(errors, "Column name is required for aggregate functions.")
      }
    }

    # Validate WHERE conditions
    if (!is.null(input$where_col1) && trimws(input$where_col1) != "") {
      if (!(input$where_op1 %in% c("IS NULL", "IS NOT NULL"))) {
        if (is.null(input$where_val1) || trimws(input$where_val1) == "") {
          errors <- c(errors, "Value is required for WHERE condition 1.")
        }
        if (input$where_op1 == "BETWEEN" && (is.null(input$where_val1_end) || trimws(input$where_val1_end) == "")) {
          errors <- c(errors, "End value is required for BETWEEN operator in condition 1.")
        }
      }
    }

    if (input$add_condition2 && !is.null(input$where_col2) && trimws(input$where_col2) != "") {
      if (!(input$where_op2 %in% c("IS NULL", "IS NOT NULL"))) {
        if (is.null(input$where_val2) || trimws(input$where_val2) == "") {
          errors <- c(errors, "Value is required for WHERE condition 2.")
        }
        if (input$where_op2 == "BETWEEN" && (is.null(input$where_val2_end) || trimws(input$where_val2_end) == "")) {
          errors <- c(errors, "End value is required for BETWEEN operator in condition 2.")
        }
      }
    }

    if (input$add_condition3 && !is.null(input$where_col3) && trimws(input$where_col3) != "") {
      if (!(input$where_op3 %in% c("IS NULL", "IS NOT NULL"))) {
        if (is.null(input$where_val3) || trimws(input$where_val3) == "") {
          errors <- c(errors, "Value is required for WHERE condition 3.")
        }
        if (input$where_op3 == "BETWEEN" && (is.null(input$where_val3_end) || trimws(input$where_val3_end) == "")) {
          errors <- c(errors, "End value is required for BETWEEN operator in condition 3.")
        }
      }
    }

    # Validate date filter
    if (input$use_date_filter) {
      if (is.null(input$date_column) || trimws(input$date_column) == "") {
        errors <- c(errors, "Date column name is required when using date filters.")
      }
    }

    # Validate HAVING without GROUP BY
    if (!is.null(input$having) && trimws(input$having) != "") {
      if (is.null(input$group_by) || trimws(input$group_by) == "") {
        if (input$select_type != "aggregate") {
          errors <- c(errors, "HAVING clause requires a GROUP BY clause or aggregate functions.")
        }
      }
    }

    query_result$errors <- errors
    query_result$has_errors <- length(errors) > 0

    # Build query if no errors
    if (length(errors) == 0) {
      # Build SELECT clause
      select_clause <- "SELECT"

      if (input$select_type == "all") {
        select_clause <- paste(select_clause, "*")
      } else if (input$select_type == "specific") {
        if (input$distinct) {
          select_clause <- paste(select_clause, "DISTINCT")
        }
        columns <- trimws(input$columns)
        select_clause <- paste(select_clause, columns)
      } else if (input$select_type == "aggregate") {
        agg_parts <- c()

        # Add additional columns first
        if (!is.null(input$group_columns) && trimws(input$group_columns) != "") {
          agg_parts <- c(agg_parts, trimws(input$group_columns))
        }

        # Build aggregate function
        agg_col <- trimws(input$agg_column)
        if (input$agg_function == "COUNT_DISTINCT") {
          agg_expr <- paste0("COUNT(DISTINCT ", agg_col, ")")
        } else {
          agg_expr <- paste0(input$agg_function, "(", agg_col, ")")
        }

        if (!is.null(input$agg_alias) && trimws(input$agg_alias) != "") {
          agg_expr <- paste0(agg_expr, " AS ", trimws(input$agg_alias))
        }

        agg_parts <- c(agg_parts, agg_expr)
        select_clause <- paste(select_clause, paste(agg_parts, collapse = ",\n       "))
      }

      # Build FROM clause
      table_ref <- trimws(input$table_name)
      if (!is.null(input$schema_name) && trimws(input$schema_name) != "") {
        table_ref <- paste0(trimws(input$schema_name), ".", table_ref)
      }
      if (!is.null(input$table_alias) && trimws(input$table_alias) != "") {
        table_ref <- paste(table_ref, trimws(input$table_alias))
      }
      from_clause <- paste("FROM", table_ref)

      # Build WHERE clause
      where_conditions <- c()

      # Helper function to format value based on operator
      format_condition <- function(col, op, val, val_end = NULL) {
        col <- trimws(col)
        val <- trimws(val)

        if (op == "IS NULL") {
          return(paste(col, "IS NULL"))
        } else if (op == "IS NOT NULL") {
          return(paste(col, "IS NOT NULL"))
        } else if (op == "IN" || op == "NOT IN") {
          return(paste(col, op, paste0("(", val, ")")))
        } else if (op == "BETWEEN") {
          val_end <- trimws(val_end)
          return(paste(col, "BETWEEN", val, "AND", val_end))
        } else if (op == "LIKE" || op == "ILIKE") {
          if (!grepl("^'.*'$", val)) {
            val <- paste0("'", val, "'")
          }
          return(paste(col, op, val))
        } else {
          return(paste(col, op, val))
        }
      }

      # Condition 1
      if (!is.null(input$where_col1) && trimws(input$where_col1) != "") {
        cond1 <- format_condition(input$where_col1, input$where_op1, input$where_val1, input$where_val1_end)
        where_conditions <- c(where_conditions, cond1)
      }

      # Condition 2
      if (input$add_condition2 && !is.null(input$where_col2) && trimws(input$where_col2) != "") {
        cond2 <- format_condition(input$where_col2, input$where_op2, input$where_val2, input$where_val2_end)
        where_conditions <- c(where_conditions, paste(input$logic_op1, cond2))
      }

      # Condition 3
      if (input$add_condition3 && !is.null(input$where_col3) && trimws(input$where_col3) != "") {
        cond3 <- format_condition(input$where_col3, input$where_op3, input$where_val3, input$where_val3_end)
        where_conditions <- c(where_conditions, paste(input$logic_op2, cond3))
      }

      # Date filter
      if (input$use_date_filter && !is.null(input$date_column) && trimws(input$date_column) != "") {
        date_col <- trimws(input$date_column)
        date_cond <- NULL

        if (input$date_filter_type == "range") {
          date_cond <- paste0(date_col, " BETWEEN '", input$date_start, "' AND '", input$date_end, "'")
        } else if (input$date_filter_type == "last_n_days") {
          date_cond <- paste0(date_col, " >= DATEADD(day, -", input$last_n_days, ", TRUNC(GETDATE()))")
        } else if (input$date_filter_type == "current_month") {
          date_cond <- paste0("DATE_TRUNC('month', ", date_col, ") = DATE_TRUNC('month', GETDATE())")
        } else if (input$date_filter_type == "current_year") {
          date_cond <- paste0("DATE_TRUNC('year', ", date_col, ") = DATE_TRUNC('year', GETDATE())")
        } else if (input$date_filter_type == "specific") {
          date_cond <- paste0("TRUNC(", date_col, ") = '", input$specific_date, "'")
        }

        if (!is.null(date_cond)) {
          if (length(where_conditions) > 0) {
            where_conditions <- c(where_conditions, paste("AND", date_cond))
          } else {
            where_conditions <- c(where_conditions, date_cond)
          }
        }
      }

      where_clause <- ""
      if (length(where_conditions) > 0) {
        where_clause <- paste("WHERE", paste(where_conditions, collapse = "\n      "))
      }

      # GROUP BY clause
      group_by_clause <- ""
      if (!is.null(input$group_by) && trimws(input$group_by) != "") {
        group_by_clause <- paste("GROUP BY", trimws(input$group_by))
      } else if (input$select_type == "aggregate" && !is.null(input$group_columns) && trimws(input$group_columns) != "") {
        group_by_clause <- paste("GROUP BY", trimws(input$group_columns))
      }

      # HAVING clause
      having_clause <- ""
      if (!is.null(input$having) && trimws(input$having) != "") {
        having_clause <- paste("HAVING", trimws(input$having))
      }

      # ORDER BY clause
      order_by_clause <- ""
      if (!is.null(input$order_by) && trimws(input$order_by) != "") {
        order_by_clause <- paste("ORDER BY", trimws(input$order_by))
      }

      # LIMIT clause
      limit_clause <- ""
      if (!is.null(input$limit_rows) && !is.na(input$limit_rows)) {
        limit_clause <- paste("LIMIT", input$limit_rows)
      }

      # OFFSET clause
      offset_clause <- ""
      if (!is.null(input$offset_rows) && !is.na(input$offset_rows)) {
        offset_clause <- paste("OFFSET", input$offset_rows)
      }

      # Combine all clauses
      query_parts <- c(select_clause, from_clause)
      if (where_clause != "") query_parts <- c(query_parts, where_clause)
      if (group_by_clause != "") query_parts <- c(query_parts, group_by_clause)
      if (having_clause != "") query_parts <- c(query_parts, having_clause)
      if (order_by_clause != "") query_parts <- c(query_parts, order_by_clause)
      if (limit_clause != "") query_parts <- c(query_parts, limit_clause)
      if (offset_clause != "") query_parts <- c(query_parts, offset_clause)

      query_result$sql <- paste(query_parts, collapse = "\n")
    }
  })

  # Render SQL query
  output$sql_query <- renderUI({
    sql <- query_result$sql
    tags$pre(style = "margin: 0; white-space: pre-wrap;", sql)
  })

  # Render validation messages
  output$validation_messages <- renderUI({
    if (length(query_result$errors) == 0) {
      div(class = "success-message",
          icon("check-circle"), " Query is valid and ready to use.")
    } else {
      tagList(
        lapply(query_result$errors, function(err) {
          div(class = "error-message", icon("times-circle"), " ", err)
        })
      )
    }
  })
}

shinyApp(ui = ui, server = server)