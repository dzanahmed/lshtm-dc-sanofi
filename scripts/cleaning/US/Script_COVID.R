# US COVID-19 Cleaning
#install.packages('tidyverse')
#install.packages('stringr')

# Author: Dzan

library(tidyverse)

us_covid <- read_csv("data/raw_data/US/COVID-19Surveillance_All_Data.csv")

years <- c(2022, 2023)

# Selet only aggregated data from selected years
us_covid <- us_covid |> filter(`NETWORK` == "COVID-NET",
                           `SEX` == "Overall",
                           `RACE` == "Overall",
                           `MMWR-YEAR` %in% years)

# Select only variables of interest and rename them for consistency
us_covid <- us_covid |> select(
     data_source = NETWORK,
     year = `MMWR-YEAR`,
     week = `MMWR-WEEK`,
     age_group = `AGE CATEGORY`,
     hsp_rate_covid19 = `WEEKLY RATE`
) 

# Rename age groups for easier merging with other age stratified files
us_covid <-
        us_covid |>  mutate(age_group = str_replace_all(age_group, " yr", "")) |>
        mutate(age_group = case_when(
                        age_group == ">= 18" ~ "18+",
                        age_group == "< 18" ~ "0-17",
                        age_group == "5-11 " ~ "5-11",
                        age_group == "0-<6 months" ~ "0-0.5",
                        age_group == "6 months-4" ~ "0.5-4",
                        age_group == "Overall" ~ "ALL",
                        TRUE ~ age_group
                )
        )

# Drop empty values
us_covid <- us_covid |> filter(hsp_rate_covid19!="null")

# Export CSV
write_csv(us_covid, 'data/processed_data/US/US_COVID.csv')