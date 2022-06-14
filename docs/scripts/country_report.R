# script to create multiple reports for each country

# Author: Peter Boshe
# Version: 2022-06-14

# Packages
library(tidyverse)

# Parameters

# input

file_raw <- here::here("docs/data/clean_waste_base_data.rds")
report <- here::here("docs/reports/country_report.Rmd")

# output
out_dir <- here::here("docs/reports/country_reports")

# ============================================================================

## create a destination folder

if (file.exists(out_dir)) {
  cat("Folder already exists genius..")
} else {
  dir.create(out_dir)
}


## create a render function

render_fun <- function(country) {
  rmarkdown::render(
    input = report,
    params = list(country = country),
    output_dir = out_dir,
    output_file = glue::glue(
      "{country}-report.html"
    )
  )
}



## mapping function
waste_df <- read_rds(file_raw)
waste_df |>
  distinct(as.character(scan_country)) |>
  pull() |>
  map(render_fun)

## check the files
created_files <- list.files(out_dir, pattern = "report.html")


tibble(files = paste0(out_dir,"/", created_files)) |>
  mutate(file_info = file.info(files),
         now = Sys.time()) |>
  unnest(file_info) |>
  select(files,
         mtime,
         now)



  ## informational output of results





















