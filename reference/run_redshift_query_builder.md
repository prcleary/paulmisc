# Run Redshift SQL Query Builder Shiny App

Launches an interactive Shiny application for building Amazon Redshift
SQL queries without writing code. The app provides an intuitive
interface with form-based inputs for constructing complex SQL queries
including table selection, column specifications, WHERE conditions, date
filters, GROUP BY, ORDER BY, and more.

## Usage

``` r
run_redshift_query_builder(background = FALSE, ...)
```

## Arguments

- background:

  Logical. If `TRUE`, runs the app in a background R process (requires
  the `callr` package), allowing continued use of the console. If
  `FALSE` (default), blocks the console until the app is stopped.

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) when
  `background = FALSE`.

## Value

If `background = FALSE`, invisibly returns the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). If
`background = TRUE`, returns a process handle from
[`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html) which can
be used to monitor or terminate the background process.

## Details

The Redshift SQL Query Builder includes:

- **Table Selection**: Specify schema, table name, and optional alias

- **Column Selection**: Choose all columns, specific columns, or
  aggregate functions (COUNT, SUM, AVG, MIN, MAX, COUNT DISTINCT)

- **WHERE Conditions**: Add up to 3 conditions with AND/OR logic,
  supporting operators like =, !=, \>, \<, LIKE, IN, BETWEEN, IS NULL,
  etc.

- **Date Filters**: Filter by date ranges, last N days, current
  month/year, or specific dates using Redshift functions

- **Sorting & Grouping**: GROUP BY, HAVING, ORDER BY, LIMIT, OFFSET

- **Validation**: Real-time error checking and helpful messages

- **Copy to Clipboard**: One-click copy of generated SQL

## Note

This app requires the `shiny` and `bslib` packages. Running in
background mode requires the `callr` package.

## Examples

``` r
if (interactive()) {
  # Run in foreground (blocks console)
  run_redshift_query_builder()
  
  # Run in background (requires callr)
  # app_process <- run_redshift_query_builder(background = TRUE)
  # To stop: app_process$kill()
}
```
