### GER HOSP VS. LAB FIGURE ###
library(tidyverse)
library(ggplot2)
library(cowplot)
setwd('~/lshtm-dc-sanofi')

#read in merged data csv - USING MERGED CSV 2 FOR NOW
data <- read.csv('data/merged_data/merged_data2.csv')

#filter for germany and all age groups
ger_data <- data %>% filter(country == 'DE' & age_group == 'ALL')

## Filter by season
ger_data$epi_dates <- as.Date(ger_data$epi_dates)
ger_data <- ger_data %>% mutate(season =
                                   case_when(epi_dates >= '2016-10-02' & epi_dates <= '2017-05-14' ~ '16/17',
                                             epi_dates >= '2017-10-01' & epi_dates <= '2018-05-13' ~ '17/18',
                                             epi_dates >= '2018-09-30' & epi_dates <= '2019-05-12' ~ '18/19',
                                             epi_dates >= '2022-10-02' & epi_dates <= '2023-01-01' ~ '22/23'
                                   )
)

# Drop rows outside of usual seasons
ger_data <- ger_data %>% drop_na(season)

#create plot for flu
ggplot() + geom_line(data=ger_data,aes(x=epi_dates,y=cases_rate_flu,group = season),color='red') + 
     geom_line(data=ger_data,aes(x=epi_dates,y=hsp_rate_flu,group=season),color='blue') +
     facet_wrap(.~season,scales='free')

#create plot for rsv
ggplot() + geom_line(data=ger_data,aes(x=epi_dates,y=cases_rate_rsv,group = season),color='red') + 
     geom_line(data=ger_data,aes(x=epi_dates,y=hsp_rate_rsv,group=season),color='blue') +
     facet_wrap(.~season,scales='free')

#create plot for covid
epi_dates_2223 <- ger_data %>% filter(epi_dates >= '2022-10-02' & epi_dates <= '2023-01-01')
epi_dates_2223 <- epi_dates_2223$epi_dates
ggplot() + geom_line(data=ger_data,aes(x=epi_dates,y=cases_rate_covid19),color='red') + 
     geom_line(data=ger_data,aes(x=epi_dates,y=hsp_rate_covid19),color='blue') +
     scale_x_date(limits = c(min(epi_dates_2223), max = max(epi_dates_2223))) +
     scale_y_sqrt()
