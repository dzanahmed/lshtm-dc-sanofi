# US Flu Cleaning

# 2023-01-15 
# Author: Dzan

library(readr)
library(dplyr)

us_flu <- read_csv("data/raw_data/US/FluSurveillance_Custom_Download_Data.csv")

# Not necessary here, specified during raw data extraction
# years <- c("2016-2017", "2017-2018", "2018-2019", "2022-2023")

us_flu <- us_flu |> filter(`SEX CATEGORY` == "Overall", 
                      `RACE CATEGORY` == "Overall")

# Stratified by age

us_flu_age <- us_flu |> filter(`AGE CATEGORY`!="Overall")

#    Dzan: Further work is needed here to organize age groups.

unique(us_flu_age$`AGE CATEGORY`) # Preview groups

#    Some columns will have to be dropped

# Overall

us_flu_overall <- us_flu |> filter(`AGE CATEGORY`=="Overall")


# Export CSV --------------------------------------------------------------

write_csv(us_flu_age, 'data/processed_data/US/US_CDC_Flu_age.csv')

write_csv(us_flu_overall, 'data/processed_data/US/US_CDC_Flu_overall.csv')

