# Run Redshift SQL Query Builder Shiny App

Launches an interactive Shiny application for building Amazon Redshift
SQL queries without writing code. The app provides an intuitive
interface with form-based inputs for constructing complex SQL queries
including table selection, column specifications, WHERE conditions, date
filters, GROUP BY, ORDER BY, and more.

## Usage

``` r
run_redshift_query_builder(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Value

Invisibly returns the result of
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

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

This app requires the `shiny` and `bslib` packages.

## Examples

``` r
if (interactive()) {
  run_redshift_query_builder()
}
```
