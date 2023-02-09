###### CREATE EPI TABLES ######
#install.packages('tidyverse')
library(tidyverse)

#set working directory if needed
#setwd('~/lshtm-dc-sanofi')

### 2016 ###
epi_dates <- seq(as.Date("2016/01/03"), as.Date("2016/12/25"), "7 days")
epi_wk_table_16 <- as.data.frame(epi_dates)
epi_wk_table_16$epi_wk_no <- seq(1,52,1)
epi_wk_table_16$year <- 2016

### 2017 ###
epi_dates <- seq(as.Date("2017/01/01"), as.Date("2017/12/24"), "7 days")
epi_wk_table_17 <- as.data.frame(epi_dates)
epi_wk_table_17$epi_wk_no <- seq(1,52,1)
epi_wk_table_17$year <- 2017

### 2018 ###
epi_dates <- seq(as.Date("2017/12/31"), as.Date("2018/12/23"), "7 days")
epi_wk_table_18 <- as.data.frame(epi_dates)
epi_wk_table_18$epi_wk_no <- seq(1,52,1)
epi_wk_table_18$year <- 2018

### 2019 ###
epi_dates <- seq(as.Date("2018/12/30"), as.Date("2019/12/22"), "7 days")
epi_wk_table_19 <- as.data.frame(epi_dates)
epi_wk_table_19$epi_wk_no <- seq(1,52,1)
epi_wk_table_19$year <- 2019

### 2022 ###
epi_dates <- seq(as.Date("2022/01/02"), as.Date("2022/12/25"), "7 days")
epi_wk_table_22 <- as.data.frame(epi_dates)
epi_wk_table_22$epi_wk_no <- seq(1,52,1)
epi_wk_table_22$year <- 2022

### 2023 ###
epi_dates <- seq(as.Date("2023/01/01"), as.Date("2023/12/24"), "7 days")
epi_wk_table_23 <- as.data.frame(epi_dates)
epi_wk_table_23$epi_wk_no <- seq(1,52,1)
epi_wk_table_23$year <- 2023

epi_wk_table <- rbind(epi_wk_table_16,
                      epi_wk_table_17,
                      epi_wk_table_18,
                      epi_wk_table_19,
                      epi_wk_table_22,
                      epi_wk_table_23)

write.csv(epi_wk_table,'data/epi_weeks.csv',row.names = F)
