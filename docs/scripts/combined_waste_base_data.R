# script to combine the public pollutants data

# Author: Peter Boshe
# Version: 2022-05-27

# Packages
library(tidyverse)
library(janitor)
library(glue)

# Parameters
 # input
files_raw <- list.files(here::here("data-raw"), "*F.csv") # TODO need to find a scaleable way of obtaining the data
paths <- map_chr(files_raw, ~ glue("data-raw/", .x))
# HACK on reading multiple files with the same naming convention

 # output

file_out <- here::here("data/combined_waste_base_data.rds")

# ============================================================================

# Code

# read all files
all_files <- map(paths, read_csv)

# save combined data
all_files |>
  bind_rows(.id = "batch") |>
  write_rds(file_out)

# TODO must update all locations
