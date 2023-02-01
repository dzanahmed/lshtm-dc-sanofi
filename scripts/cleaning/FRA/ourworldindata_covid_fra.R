# import OurWorldinData COVID-19 data and clean data for
# France

#library(tidyverse)

#----------------------------------------------------------------
# import dataset
covid19 <- read_csv("data/raw_data/FRA/weekly-hospital-admissions-covid.csv")

# FluNet
VIW_FNT <- read_csv("data/raw_data/FluNet/VIW_FNT.csv")

#----------------------------------------------------------------
# filter for france data
france_covid19 <- covid19 %>% 
  filter(Entity=='France' & Day >= '2022-01-01') 

# merge in epiweeks to link across datasets
epiweeks <- VIW_FNT %>% 
            subset(select = c(MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK)) %>% 
            filter(MMWR_YEAR == 2022) %>% 
            distinct()

epiweeks <- epiweeks[order(epiweeks$MMWR_WEEKSTARTDATE),]

france_covid19 <- merge(france_covid19, epiweeks, by.x = "Day", by.y = "MMWR_WEEKSTARTDATE")

# standardize var names according to data template
france_covid19$data_source <- "sante publique"
france_covid19$hemisphere <- "NH"

france_covid19 <- france_covid19 %>% 
                  rename(hsp_abs_covid19 = `Weekly new hospital admissions`,
                         country = Entity,
                         year = MMWR_YEAR,
                         week = MMWR_WEEK)

# include population to calculate rate per 100,000
france_covid19$denominator <- 67885000

# covid hospitalization rate per 100,000
france_covid19$hsp_rate_covid19 <- (france_covid19$hsp_abs_covid19 / france_covid19$denominator)*100000

var_order <- c("data_source", "country", "hemisphere", "week", "year", "denominator", "hsp_rate_covid19", "hsp_abs_covid19")
france_covid19 <- france_covid19[,var_order]

# save and export
write_csv(france_covid19, file = "data/processed_data/FRA/france_covid19.csv")