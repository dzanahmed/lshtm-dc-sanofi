# US RSV Cleaning
# Author: Dzan

library(tidyverse)
library(stringr)

us_rsv <- read_csv("data/raw_data/US/Weekly_Rates_of_Laboratory-Confirmed_RSV_Hospitalizations_from_the_RSV-NET_Surveillance_System.csv")

years <- c("2016-2017", "2017-2018", "2018-2019", "2022-2023")

# Select only aggregated data from selected years
us_rsv <-
     us_rsv |> filter(
          `MMWR Week` != 'Overall', # We are not interested in yearly rates
          Season %in% years, #only seasons we are interested in
          State == "Entire Network (RSV-NET)",
          Sex == "Overall", # For now, we are not stratifying,
          Race == "Overall"
     )

# Select only variables of interest and rename them
us_rsv <- us_rsv |> select(
     data_source = State,
     year = Season,
     week = `MMWR Week`,
     age_group = `Age Category`,
     hsp_rate_rsv = Rate
)

# Classify Weeks and years as numeric vars
us_rsv <- us_rsv |> mutate(week = as.numeric(week)) |> mutate(year = case_when(
     week >= 40 ~ substr(year, start = 1, stop = 4),
     week < 25 ~ substr(year, start = 6, stop = 9)
)) |> mutate(year = as.numeric(year))

# RSV age stratified - rename age groups for easier merging with other age stratified files
us_rsv <- us_rsv |> 
     mutate(age_group = str_replace_all(age_group, c("----" = "", " years" = ""))) |> 
     mutate(age_group = case_when(age_group=="18+ (Adults)" ~ "18+", 
                                  age_group=="0-17 (Children)" ~ "0-17",
                                  age_group=="1-<2" ~ "1-2",
                                  age_group=="0-<6 months" ~ "0-0.5",
                                  age_group=="6-<12 months" ~ "0.5-1",
                                  age_group=="Overall" ~ "ALL",
                                  TRUE ~ age_group))

# Export CSV
write_csv(us_rsv, 'data/processed_data/US/US_RSV.csv')