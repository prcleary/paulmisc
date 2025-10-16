# paulmisc

> Collection of miscellaneous R functions of interest only to Paul

Currently just contains a few things used to manage my workload. 

## Installation 

```r
remotes::install_github('prcleary/paulmisc')
```

## Usage

Run app with one of the following:

- for testing daily planner Shiny app without installation:

```r
shiny::runGitHub('prcleary/paulmisc', subdir = 'inst/apps/planner')
```

- for testing the daily planner Shiny app following installation:  

```r
paulmisc::run_app('planner')
```

- for reading tasks from Nextcloud Tasks:

```r
tasks <- paulmisc::get_nextcloud_tasks('https://nextcloud.domain.tld/remote.php/dav', 'yourusername', 'yourpassword')
```
