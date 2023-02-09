# US Flu Cleaning
#install.packages('tidyverse')
#install.packages('stringr')

# Author: Dzan

library(tidyverse)
library(stringr)

us_flu <- read_csv("data/raw_data/US/FluSurveillance_Custom_Download_Data.csv")

# Not necessary to declare years here, specified during raw data extraction
# years <- c("2016-2017", "2017-2018", "2018-2019", "2022-2023")

# Aggregated sex and race data only
us_flu <- us_flu |> filter(`SEX CATEGORY` == "Overall", 
                      `RACE CATEGORY` == "Overall")

# Select only variables of interest and rename them for consistency
us_flu <- us_flu |> select(
     data_source = NETWORK,
     year = `MMWR-YEAR`,
     week = `MMWR-WEEK`,
     age_group = `AGE CATEGORY`,
     hsp_rate_flu = `WEEKLY RATE`
)

# Flu age stratified - rename age groups for easier merging with other age stratified files
us_flu <-
     us_flu |> mutate(age_group = str_replace_all(age_group, c("----" = "", " yr" = ""))) |>
     mutate(
          age_group = case_when(
               age_group == ">= 18" ~ "18+",
               age_group == "< 18" ~ "0-17",
               age_group == "5-11 " ~ "5-11",
               age_group == "0-<6 months" ~ "0-0.5",
               age_group == "6 months-4" ~ "0.5-4",
               age_group == "Overall" ~ "ALL",
               TRUE ~ age_group
          )
     )

# Drop empty rows
us_flu <- us_flu |> filter(hsp_rate_flu!="null")

# Export CSV
write_csv(us_flu, 'data/processed_data/US/US_Flu.csv')