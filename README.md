# paulmisc

> Collection of miscellaneous R functions of interest only to Paul

Currently just contains a simple Shiny app for planning my day and randomising my tasks

## Installation 

```r
remotes::install_github('prcleary/paulmisc')
```

## Usage

Run app with one of the following:

```r
shiny::runGitHub('prcleary/paulmisc', subdir = 'inst/apps/planner')  # for testing without installation
paulmisc::run_app('planner')  # if paulmisc package installed 
```
