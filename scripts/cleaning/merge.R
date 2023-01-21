# Merge script
# Dzan 20 01 2023

library(dplyr)

# Variable order in merged CSV is sourced from here
source(file = 'scripts/cleaning/helper.R')

# Pull data from this folder
dir <- "data/premerged_data"

# List filenames of all CSVs, then rename them all without folder path and
# create a list of dataframes and send them all to env

files <- list.files(dir, pattern = "*.csv", full.names = TRUE)
namesEnv <- gsub("data/premerged_data/", "", gsub(".csv", "", files))
fileList <- lapply(setNames(files, namesEnv), read.csv)
list2env(fileList, envir = .GlobalEnv)

# Bind all dataframes from list
merged_data <- bind_rows(fileList)

# This is to add dates, based on week of the year
epiweeks <- read.csv(file = 'data/epiweeks_withdate.csv')
epiweeks$start_date <- as.Date(epiweeks$start_date)

# All the last columns (after age_group) are numeric. Also, assign new IDs

merged_data <-
     merged_data |> mutate(across(hsp_rate:subtype_c_rate, as.double)) |>
     mutate(id = row_number())

# Merge with epiweeks and reorder merged_data
merged_data <- merge(merged_data, epiweeks)
merged_data <- merged_data[order(merged_data$id), order_header_premerge]

# Write a CSV
readr::write_csv(merged_data, 'data/merged_data/merged_data.csv')