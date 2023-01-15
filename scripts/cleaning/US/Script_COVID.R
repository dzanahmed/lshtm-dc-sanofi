# US COVID-19 Cleaning

# 2023-01-15 
# Author: Dzan

library(readr)
library(dplyr)

db_covid <- read_csv("data/raw_data/US/COVID-19Surveillance_All_Data.csv")

years <- c(2022, 2023)

db_covid <- db_covid |> filter(`NETWORK` == "COVID-NET",
                           `SEX` == "Overall",
                           `RACE` == "Overall",
                           `MMWR-YEAR` %in% years)

# Stratified by age

db_covid_age <- db_covid |> filter(`AGE CATEGORY`!="Overall")

#    Dzan: Further work is needed here to organize age groups.

unique(db_covid_age$`AGE CATEGORY`) # Preview groups

# Some columns will have to be dropped

# Overall

db_covid_overall <- db_covid |> filter(`AGE CATEGORY`=="Overall")

# Dzan: NAs in Age category are for weeks that haven't happened yet

# Export CSV --------------------------------------------------------------

write_csv(db_covid_age, 'data/processed_data/US/US_CDC_COVID_age.csv')

write_csv(db_covid_overall, 'data/processed_data/US/US_CDC_COVID_overall.csv')