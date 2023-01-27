# US RSV Cleaning

# 2023-01-15
# Author: Dzan

library(readr)
library(dplyr)

us_rsv <- read_csv("data/raw_data/US/Weekly_Rates_of_Laboratory-Confirmed_RSV_Hospitalizations_from_the_RSV-NET_Surveillance_System.csv"
     )

years <- c("2016-2017", "2017-2018", "2018-2019", "2022-2023")

us_rsv <-
     us_rsv |> filter(
          `MMWR Week` != 'Overall', # We are not interested in yearly rates
          Season %in% years, #only seasons we are interested in
          State == "Entire Network (RSV-NET)",
          Sex == "Overall", # For now, we are not stratifying,
          Race == "Overall"
     )

# Stratified by age
us_rsv_age <- us_rsv |> filter(`Age Category` != "Overall")

#    Dzan: Further work is needed here to organize age groups.
unique(us_rsv_age$`Age Category`) # Preview age groups

# Overall
us_rsv_overall <- us_rsv |> filter(`Age Category` == "Overall")

# Export CSV --------------------------------------------------------------

write_csv(us_rsv_age, 'data/processed_data/US/US_CDC_RSV_age.csv')

write_csv(us_rsv_overall, 'data/processed_data/US/US_CDC_RSV_overall.csv')