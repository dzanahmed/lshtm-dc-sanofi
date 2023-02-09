# import WHO FluNet FluID RSV data and collate into one dataset for 
  # Australia
#install.packages('tidyverse')
library(tidyverse)

#setwd("~/Desktop/LSHTM/2491. Data Challenge/Project Analysis")

#----------------------------------------------------------------
# import FluID and FluNet data
# FluID
VIW_FID_EPI <- read_csv("data/raw_data/FluNet/VIW_FID_EPI.csv")

# FluNet
VIW_FNT <- read_csv("data/raw_data/FluNet/VIW_FNT.csv")

#----------------------------------------------------------------
# australia
#fluID data
australia_fid <- VIW_FID_EPI %>% 
                filter(COUNTRY_CODE=='AUS' & ((MMWR_YEAR >= 2016 & MMWR_YEAR < 2020) | MMWR_YEAR == 2022)) %>% 
                select(HEMISPHERE, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK, AGEGROUP_CODE, SARI_INPATIENTS)

australia_fid <- australia_fid[order(australia_fid$MMWR_WEEKSTARTDATE),]
# SARI inpatients is blank for all values -- ignore this dataset

#fluNet data
australia_fnt <- VIW_FNT %>% 
                    filter(COUNTRY_CODE=='AUS' & ((MMWR_YEAR >= 2016 & MMWR_YEAR < 2020) | MMWR_YEAR == 2022)) %>% 
                    select(HEMISPHERE, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK, SPEC_PROCESSED_NB, RSV, INF_A, INF_B, INF_ALL)

australia_fnt <- australia_fnt[order(australia_fnt$MMWR_WEEKSTARTDATE),]

#standardize var names according to data template
australia_fnt$data_source <- "fluNet"

australia_fnt <- australia_fnt %>% 
                  rename(hemisphere = HEMISPHERE,
                         country = COUNTRY_AREA_TERRITORY, 
                         week = MMWR_WEEK,
                         year = MMWR_YEAR,
                         hsp_abs_flu = INF_ALL,
                         hsp_abs_rsv = RSV,
                         subtype_a_abs = INF_A,
                         subtype_b_abs = INF_B)

# add population numbers to calculate rates per 100,000
# see pop data here: https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/jun-2022#data-downloads
# population and components of change - national xlsx
aus_pop_2016 <- mean(24103.4, 24190.9, 24297.9, 24385.9)*1000
aus_pop_2017 <- mean(24512.6, 24594.2, 24692.0, 24761.4)*1000
aus_pop_2018 <- mean(24884.6, 24966.6, 25071.2, 25150.5)*1000
aus_pop_2019 <- mean(25269.8, 25340.2, 25443.9, 25526.8)*1000
aus_pop_2022 <- mean(25890.8, 25978.9)

australia_fnt$denominator <- ifelse(australia_fnt$year==2016, aus_pop_2016,
                                    ifelse(australia_fnt$year==2017, aus_pop_2017,
                                           ifelse(australia_fnt$year==2018, aus_pop_2018,
                                                  ifelse(australia_fnt$year==2019, aus_pop_2019,
                                                         ifelse(australia_fnt$year==2022, aus_pop_2022, NA)))))
# calculate rate per 100,000
# RSV rate
australia_fnt$hsp_rate_rsv = (australia_fnt$hsp_abs_rsv / australia_fnt$denominator)*100000

# Flu rate
australia_fnt$hsp_rate_flu = (australia_fnt$hsp_abs_flu / australia_fnt$denominator)*100000

# reorder vars
var_order <- c("data_source", "country", "hemisphere", "week", "year", "denominator", "hsp_rate_flu", "hsp_rate_rsv", "hsp_abs_flu", "hsp_abs_rsv", "subtype_a_abs", "subtype_b_abs")
australia_fnt <- australia_fnt[,var_order]

# save and export
write_csv(australia_fnt, file = "australia_fluNet.csv")