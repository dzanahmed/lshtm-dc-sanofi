# create csv file with epi week start date, epi year, and epi week

library(tidyr)
library(tidyverse)
library(dplyr)
library(readr)

setwd("~/Desktop/LSHTM/2491. Data Challenge/Project Analysis")

#----------------------------------------------------------------
# import FluNet data

# FluNet
VIW_FNT <- read_csv("VIW_FNT.csv")

# filter out data for MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK
mmwr_weekstartdate <- VIW_FNT %>% 
  filter((MMWR_YEAR >= 2016 & MMWR_YEAR < 2020) | MMWR_YEAR==2022) %>% 
  select(MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK) %>% 
  distinct()

# sort data by epi week start date
mmwr_weekstartdate <- mmwr_weekstartdate[order(mmwr_weekstartdate$MMWR_WEEKSTARTDATE),]

# create additional row for first week of 2023
mmwr_2023 <- data.frame(MMWR_WEEKSTARTDATE = "2023-01-01",
                        MMWR_YEAR = 2023,
                        MMWR_WEEK = 1)

# add additional row for 2023 to larger dataset
mmwr_weekstartdate <- rbind(mmwr_weekstartdate, mmwr_2023)
write_csv(mmwr_weekstartdate, file = "epiweeks_withdate.csv")