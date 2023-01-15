# US RSV Cleaning

# 2023-01-15 
# Author: Dzan

library(readr)
library(dplyr)

db_rsv <- read_csv("G:/.shortcut-targets-by-id/1k5jIYGQi1Tv9w4D3u-cE4D3upV3qxt7V/Data Challenge - Sanofi shared/FluNet Data & Data Dictionary/US/Weekly_Rates_of_Laboratory-Confirmed_RSV_Hospitalizations_from_the_RSV-NET_Surveillance_System.csv")

# Not necessary here, specified during export
years <- c("2016-2017", "2017-2018", "2018-2019", "2022-2023")


db_rsv <- db_rsv |> filter(`MMWR Week` != 'Overall', # We are not interested in yearly rates
                 Season %in% years, #only seasons we are interested in
                 State == "Entire Network (RSV-NET)",
                 Sex == "Overall", # For now, we are not stratifying,
                 Race == "Overall"
)

# Stratified by age

db_rsv_age <- db_rsv |> filter(`Age Category`!="Overall")

#    Dzan: Further work is needed here to organize age groups.

unique(db_rsv_age$`Age Category`) # Preview groups

#    Some columns will have to be dropped

# Overall

db_rsv_overall <- db_rsv |> filter(`Age Category`=="Overall")


# Export CSV --------------------------------------------------------------

write_csv(db_rsv_age, 'US_CDC_RSV_age.csv')

write_csv(db_rsv_overall, 'US_CDC_RSV_overall.csv')

