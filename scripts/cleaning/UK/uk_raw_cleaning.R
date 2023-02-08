#install.packages('readODS')
library(readODS)
library(tidyverse)
library(readr)

#set wd - amend as required
setwd('~/lshtm-dc-sanofi')

### Extract flu & covid rates 22/23

#Read ODS file
hsp_rates_flu_covid_22_23_all_ages <- read_ods('data/raw_data/UK/Weekly_Influenza_and_COVID19_report_data_w4_report.ods',
                 sheet = 31) #selects relevant sheet

#Select table for flu and COVID
hsp_rates_flu_covid_22_23_all_ages <- hsp_rates_flu_covid_22_23_all_ages[8:59,2:4]
colnames(hsp_rates_flu_covid_22_23_all_ages) <- c('week','hsp_flu_rate','hsp_covid_rate') #change colnames
hsp_rates_flu_covid_22_23_all_ages$age <- 'ALL' #add age column
hsp_rates_flu_covid_22_23_all_ages$week <- as.numeric(hsp_rates_flu_covid_22_23_all_ages$week) #change column class
hsp_rates_flu_covid_22_23_all_ages <- hsp_rates_flu_covid_22_23_all_ages %>% mutate(year = # create year column based on week column 
                                                  case_when(week >= 3 ~ 2022,
                                                            week < 3 ~ 2023))

write_csv(hsp_rates_flu_covid_22_23_all_ages,'data/processed_data/UK/hsp_rates_cov_flu_2223_updated.csv') #output csv for next step

### FLU DATA ###
hsp_rates_flu <- read_ods('data/raw_data/UK/Weekly_Influenza_and_COVID19_report_data_w4_report.ods',
                                               sheet = 32) #historical flu datasheet

#Extract table from sheet
hsp_rates_flu <- hsp_rates_flu[8:59,2:8]
colnames(hsp_rates_flu) <- c('week','2022/23','2021/22','2020/21','2019/20','2018/19','2017/18') #update column names for each season

hsp_rates_flu <- hsp_rates_flu %>% pivot_longer(!week,names_to = 'season',values_to = 'hsp_flu_rate') #pivot to one season column

hsp_rates_flu <- hsp_rates_flu %>% drop_na(hsp_flu_rate)
hsp_rates_flu$week <- as.numeric(hsp_rates_flu$week)

#add year column
hsp_rates_flu <- hsp_rates_flu %>% mutate(year = case_when(
     season == '2017/18' & week >= 40 ~ 2017,
     season == '2017/18' & week <= 20 ~ 2018,
     season == '2018/19' & week >= 40 ~ 2018,
     season == '2018/19' & week <= 20 ~ 2019,
     season == '2022/23' & week >= 40 ~ 2022,
     season == '2022/23' & week <= 20 ~ 2023,
))

hsp_rates_flu <- hsp_rates_flu %>% drop_na(year)
hsp_rates_flu$season <- NULL
hsp_rates_flu$age <- 'ALL'

write.csv(hsp_rates_flu,'data/processed_data/UK/flu_rates_all_ages_updated.csv') #output csv for next clearning step

#rsv data 2017-2023
rsv <- read_ods('data/raw_data/UK/Weekly_Influenza_and_COVID19_report_data_w4_report.ods',
                                               sheet = 40) #selects sheet for historical RSV data

rsv <- rsv[9:60,2:11] 
rsv <- rsv[,c(1:2,6:10)] #selects relevant tables
colnames(rsv) <- c('week','22/23','21/22','20/21','19/20','18/19','17/18') #rename columns to seasons
rsv <- rsv %>% pivot_longer(!week,names_to = 'season',values_to = 'hsp_rsv_rate') #create single column for all seasons
rsv <- rsv %>% drop_na(hsp_rsv_rate)
rsv$week <- as.numeric(rsv$week)

#add year column
rsv <- rsv %>% mutate(year = case_when(
     season == '17/18' & week >= 40 ~ 2017,
     season == '17/18' & week <= 20 ~ 2018,
     season == '18/19' & week >= 40 ~ 2018,
     season == '18/19' & week <= 20 ~ 2019,
     season == '22/23' & week >= 40 ~ 2022,
     season == '22/23' & week <= 20 ~ 2023,
))

rsv <- rsv %>% drop_na(year)
rsv$season <- NULL
rsv$age <- 'ALL'

write_csv(rsv,'data/processed_data/UK/rsv_all_yrs_all_ages_updated.csv') #output csv for next step

