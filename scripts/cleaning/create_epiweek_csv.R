# create csv file with epi week start date, epi year, and epi week

library(tidyverse)

#----------------------------------------------------------------
# import FluNet data

# FluNet
VIW_FNT <- read_csv("data/raw_data/FluNet/VIW_FNT.csv")

# filter out data for MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK
mmwr_weekstartdate <- VIW_FNT %>% 
  filter(MMWR_YEAR >= 2016) %>% 
  select(MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK) %>% 
  distinct()

# sort data by epi week start date
mmwr_weekstartdate <- mmwr_weekstartdate[order(mmwr_weekstartdate$MMWR_WEEKSTARTDATE),]

# create additional rows for first weeks of 2023
mmwr_2023 <-
     data.frame(
          MMWR_WEEKSTARTDATE = c(
               "2023-01-01",
               "2023-01-08",
               "2023-01-22",
               "2023-01-29",
               "2023-02-05",
               "2023-02-12"
          ),
          MMWR_YEAR = 2023,
          MMWR_WEEK = c(1, 2, 3, 4, 5, 6)
     )

# add additional row for 2023 to larger dataset
mmwr_weekstartdate <- rbind(mmwr_weekstartdate, mmwr_2023)

# rename them to suit the structure of merged_data
colnames(mmwr_weekstartdate) <- c("start_date", "year", "week")

write_csv(mmwr_weekstartdate, file = "data/epiweeks_withdate.csv")
