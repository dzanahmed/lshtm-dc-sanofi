# import WHO FluNet FluID RSV data and collate into one dataset for 
  # France

#install.packages('tidyverse')
library(tidyverse)

#amend as appropriate if required
#setwd("~/Desktop/LSHTM/Github/lshtm-dc-sanofi")

#----------------------------------------------------------------
# import FluID and FluNet data
# FluID
VIW_FID_EPI <- read_csv("data/raw_data/FluNet/VIW_FID_EPI.csv")

# FluNet
VIW_FNT <- read_csv("data/raw_data/FluNet/VIW_FNT.csv")

#----------------------------------------------------------------
# france
france_fid <- VIW_FID_EPI %>% 
  filter(COUNTRY_CODE=='FRA' & ((MMWR_YEAR >= 2016 & MMWR_YEAR < 2020) | MMWR_YEAR == 2022)) %>% 
  select(HEMISPHERE, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK, AGEGROUP_CODE, SARI_INPATIENTS)

france_fid <- france_fid[order(france_fid$MMWR_WEEKSTARTDATE),]
# SARI inpatients is blank for all values -- ignore this dataset

france_fnt <- VIW_FNT %>% 
  filter(COUNTRY_CODE=='FRA' & ((MMWR_YEAR >= 2016 & MMWR_YEAR < 2020) | MMWR_YEAR == 2022)) %>% 
  select(HEMISPHERE, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK, ORIGIN_SOURCE, SPEC_PROCESSED_NB, RSV, INF_A, INF_B, INF_ALL)

france_fnt <- france_fnt[order(france_fnt$MMWR_WEEKSTARTDATE),]

#standardize var names according to data template
france_fnt$data_source <- "fluNet"

france_fnt <- france_fnt %>% 
              rename(hemisphere = HEMISPHERE,
                     country = COUNTRY_AREA_TERRITORY, 
                     week = MMWR_WEEK,
                     year = MMWR_YEAR,
                     hsp_abs_flu = INF_ALL,
                     hsp_abs_rsv = RSV,
                     subtype_a_abs = INF_A,
                     subtype_b_abs = INF_B,
                     origin_source = ORIGIN_SOURCE)

# include population to calculate rate per 100,000
france_fnt$denominator <- ifelse(france_fnt$year==2016, 66688564,
                                ifelse(france_fnt$year==2017,66883321,
                                       ifelse(france_fnt$year==2018, 67125071,
                                              ifelse(france_fnt$year==2019, 67356052,
                                                     ifelse(france_fnt$year==2022, 67885000, NA))))) 

#france population data
# 2016: 66688564
# 2017: 66883321
# 2018: 67125071
# 2019: 67356052 #(https://www.insee.fr/en/statistiques/serie/001641584)
# 2022: 67885000 #average of the estimated population at the beginning of each month in 2022 (https://www.insee.fr/en/statistiques/serie/001641607)

# calculate rate per 100,000
# RSV rate
france_fnt$hsp_rate_rsv = (france_fnt$hsp_abs_rsv / france_fnt$denominator)*100000

# Flu rate
france_fnt$hsp_rate_flu = (france_fnt$hsp_abs_flu / france_fnt$denominator)*100000

# reorder vars
var_order <- c("data_source", "country", "hemisphere", "week", "year", "origin_source", "hsp_rate_flu", "hsp_rate_rsv", "hsp_abs_flu", "hsp_abs_rsv", "denominator", "subtype_a_abs", "subtype_b_abs")
france_fnt <- france_fnt[,var_order]

# save and export
write_csv(france_fnt, file = "data/processed_data/FRA/france_fluNet.csv")