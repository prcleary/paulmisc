
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redshift-sql-query-builder

<!-- badges: start -->

<!-- badges: end -->

The goal of redshift-sql-query-builder is to simplify creation of SQL
queries for Amazon Redshift.

It was completely vibe coded with Claude Opus 4.5 and has undergone
limited testing.

## Key Features

1.  **Table Selection Panel**
    - Schema name (optional)
    - Table name (required)
    - Table alias (optional)
2.  **Column Selection**
    - All columns (\*)
    - Specific columns with DISTINCT option
    - Aggregate functions (COUNT, SUM, AVG, MIN, MAX, COUNT DISTINCT)
3.  **WHERE Conditions**
    - Up to 3 conditions with AND/OR logic
    - Multiple operators: =, !=, \>, \<, \>=, \<=, LIKE, ILIKE, IN, NOT
      IN, IS NULL, IS NOT NULL, BETWEEN
4.  **Date Filters**
    - Date range
    - Last N days
    - Current month/year
    - Specific date
    - Uses Redshift-specific functions like DATEADD, TRUNC, GETDATE
5.  **Sorting & Grouping**
    - GROUP BY
    - HAVING
    - ORDER BY
    - LIMIT and OFFSET
6.  **User Interface**
    - Dark theme with Roboto font
    - Left panel: All inputs
    - Right panel: Generated SQL with syntax highlighting
    - Bottom panel: Validation messages with clear error explanations
    - Copy to clipboard button
    - Helpful Redshift SQL tips
7.  **Validation**
    - Checks required fields
    - Validates logical combinations (e.g., HAVING requires GROUP BY)
    - Displays user-friendly error messages

The query generates when you click “Generate Query”.
