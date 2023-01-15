# US Flu Cleaning

# 2023-01-15 
# Author: Dzan

library(readr)
library(dplyr)

db_flu <- read_csv("data/raw_data/US/FluSurveillance_Custom_Download_Data.csv")

# Not necessary here, specified during export
# years <- c("2016-2017", "2017-2018", "2018-2019", "2022-2023")

db_flu <- db_flu |> filter(`SEX CATEGORY` == "Overall", 
                      `RACE CATEGORY` == "Overall")

# Stratified by age

db_flu_age <- db_flu |> filter(`AGE CATEGORY`!="Overall")

#    Dzan: Further work is needed here to organize age groups.

unique(db_flu_age$`AGE CATEGORY`) # Preview groups

#    Some columns will have to be dropped

# Overall

db_flu_overall <- db_flu |> filter(`AGE CATEGORY`=="Overall")


# Export CSV --------------------------------------------------------------

write_csv(db_flu_age, 'data/processed_data/US/US_CDC_Flu_age.csv')

write_csv(db_flu_overall, 'data/processed_data/US/US_CDC_Flu_overall.csv')

