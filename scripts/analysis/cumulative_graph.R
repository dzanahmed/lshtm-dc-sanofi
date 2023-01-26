#import required libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)

#set working directory
setwd('~/lshtm-dc-sanofi')

#read in data
all_data <- read.csv('data/merged_data/merged_data.csv')

#filter for france for analysis
fra_data <- all_data %>% filter(country == 'FR' & data_source == 'FluNet - Sentinel')
names(fra_data)
fra_data <- fra_data %>% select('start_date','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')
fra_data$start_date <- as.Date(fra_data$start_date)


#add column for flu season

fra_data <- fra_data %>% mutate(season =
     case_when(start_date >= '2016-10-02' & start_date <= '2017-05-14' ~ '16/17',
               start_date >= '2017-10-01' & start_date <= '2018-05-13' ~ '17/18',
               start_date >= '2018-09-30' & start_date <= '2019-05-12' ~ '18/19',
               start_date >= '2022-10-02' & start_date <= '2023-01-01' ~ '22/23'
     )
)

fra_szn_data <- fra_data %>% drop_na(season)

#calculate cumulative sums
fra_szn_data <- fra_szn_data %>%
     group_by(season) %>%
     mutate(cum_flu = cumsum(hsp_abs_flu))

#add seasonal week
fra_szn_data <- fra_szn_data %>% mutate(season_week = 
                                             case_when(week >= 40 ~ week-39,
                                                       week <= 20 ~ week+12))

ggplot() + geom_col(data=fra_szn_data,aes(x=season_week,y=cum_flu,fill=season),position='dodge')

fra_szn_data <- fra_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))

### UK GRAPH ###

uk_data <- all_data %>% filter(country == 'UK' & age_group == 'ALL')

uk_data <- uk_data %>% select('start_date','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')
uk_data$start_date <- as.Date(uk_data$start_date)


#add column for flu season

uk_data <- uk_data %>% mutate(season =
                                     case_when(start_date >= '2016-10-02' & start_date <= '2017-05-14' ~ '16/17',
                                               start_date >= '2017-10-01' & start_date <= '2018-05-13' ~ '17/18',
                                               start_date >= '2018-09-30' & start_date <= '2019-05-12' ~ '18/19',
                                               start_date >= '2022-10-02' & start_date <= '2023-01-01' ~ '22/23'
                                     )
)

uk_szn_data <- uk_data %>% drop_na(season)

#calculate cumulative sums
uk_szn_data <- uk_szn_data %>%
     group_by(season) %>%
     mutate(cum_flu = cumsum(hsp_abs_flu))

#add seasonal week
uk_szn_data <- uk_szn_data %>% mutate(season_week = 
                                             case_when(week >= 40 ~ week-39,
                                                       week <= 20 ~ week+12))

uk_szn_data <- uk_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))

ggplot() + geom_col(data=uk_szn_data,aes(x=season_week,y=cum_flu,fill=season),position='dodge') +
     scale_y_log10()

### AUS ###

aus_data <- all_data %>% filter(country == 'AUS' & age_group == 'ALL' & data_source == 'fluNet')

aus_data <- aus_data %>% select('start_date','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')
aus_data$start_date <- as.Date(aus_data$start_date)


#add column for flu season

aus_data <- aus_data %>% mutate(season =
                                   case_when(week >= 16 & week <= 40 & year == 2016 ~ '16/17',
                                             week >= 16 & week <= 40 & year == 2017 ~ '17/18',
                                             week >= 16 & week <= 40 & year == 2018  ~ '18/19',
                                             week >= 16 & week <= 40 & year == 2022 ~ '22/23'
                                   )
)

aus_szn_data <- aus_data %>% drop_na(season)

#calculate cumulative sums
aus_szn_data <- aus_szn_data %>%
     group_by(season) %>%
     mutate(cum_flu = cumsum(hsp_abs_flu))

aus_szn_data <- aus_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))

ggplot() + geom_area(data=aus_szn_data,aes(x=week,y=cum_flu,fill= fct_reorder(season, cum_flu, .desc = TRUE)),position='identity') +
     scale_y_log10() +
     theme(legend.position = 'bottom')

#create season list
szns <- c('16/17','17/18','18/19','22/23')

fra_1617 <- fra_szn_data %>% filter(season == '16/17')

fra_1617 <- fra_1617 %>% mutate(cumul_flu = cumsum(hsp_abs_flu))
half_max_val <- as.integer(max(fra_1617$cumul_flu))/2
med_index <- which(fra_1617$cumul_flu > half_max_val)[1]



ggplot() + geom_col(data=fra_1617,aes(x=start_date,y=cumul_flu)) +
     geom_vline(xintercept=fra_1617$start_date[med_index],color='red')

??geom_vline

max(fra_1617$cumul_flu)

?find

1437/2

for (i in szns) {
     szn_data <- fra_szn_data %>%
          filter(season == i)
     paste0(cum_sum_flu,i) <- szn_data %>% mutate(cumsum(hsp_abs_flu))
     cum_sum_rsv <- szn_data %>% mutate(cumsum(hsp_abs_rsv))
}

cumsum(fra_szn_data$hsp_abs_flu)

test <- fra_szn_data %>% group_by(season)

?cumsum

cumulative_case_counts <- fra_szn_data %>%
     group_by(season) %>% 
     count(week) %>%# count of rows per day (returned in column "n")   
     mutate(                         
          cumulative_cases = cumsum(fra_szn_data$hsp_abs_flu)       # new column of the cumulative number of rows at each date
     )
