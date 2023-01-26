# Merge script
# Dzan 20 01 2023

library(dplyr)

# Variable order in merged CSV is sourced from here
source(file = 'scripts/cleaning/var_order_merged_csv.R')

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

# Merge with epiweeks and reorder merged_data
merged_data <- merge(merged_data, epiweeks)
merged_data <-
     merged_data[order(
          merged_data$start_date,
          merged_data$data_source,
          merged_data$age_group
     ), order_header_premerge_epiweeks]
merged_data <- merged_data |> mutate(id=row_number())

merged_data <- 

# Standardize term for age groups:
merged_data <- merged_data |> mutate(age_group = case_when(
     age_group == "Overall" ~ "ALL",
     age_group == "Total" ~ "ALL",
     TRUE ~ age_group
))

# Write a CSV
readr::write_csv(merged_data, 'data/merged_data/merged_data2.csv')
