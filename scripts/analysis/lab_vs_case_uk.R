setwd('~/lshtm-dc-sanofi')
library(tidyverse)
library(ggplot2)
library(cowplot)

#read in data
uk_lab <- read.csv('data/processed_data/UK/lab_rates_uk.csv')
all_data <- read.csv('data/merged_data/merged_data.csv')
epi_weeks <- read.csv('data/epi_weeks.csv')

#filter UK data from merged data set
uk_data <- all_data %>% filter(country == 'UK' & age_group == 'ALL')
uk_hsp_flu_rates <- uk_data %>% select('year','week','hsp_rate_flu','start_date')

#pivot lab data
uk_lab_pivot <- uk_lab %>% pivot_longer(!week,names_to='flu_season',values_to='rate_positive')

#create function to add year to uk lab data
uk_lab_pivot <- uk_lab_pivot %>% mutate(year = 
     case_when(flu_season == 'rate_18_19' & week > 20 ~ 2018,
               flu_season == 'rate_18_19' & week <= 20 ~ 2019,
               flu_season == 'rate_22_23' & week > 20 ~ 2022,
               flu_season == 'rate_22_23' & week <= 20 ~ 2023)
)

uk_lab_pivot <- uk_lab_pivot %>% mutate(season = 
                                             case_when(flu_season == 'rate_18_19'  ~ '18/19',
                                                       flu_season == 'rate_22_23'  ~ '22/23')
)

#merge data sets to get uk lab and hospital rates in one dataframe
uk_lab_hosp <- merge(x=uk_hsp_flu_rates,y=uk_lab_pivot,by=c('week','year'))

#change start date to 'Date' data type
uk_lab_hosp$start_date <- as.Date(uk_lab_hosp$start_date)

#filter for each season
uk_lab_hosp_18_19 <- uk_lab_hosp %>% filter(season == '18/19')
uk_lab_hosp_22_23 <- uk_lab_hosp %>% filter(season == '22/23')

#set up epi week x axes
epi_wks_18 <- epi_weeks %>% filter(epi_wk_no >= 40 & year == 2018)
epi_wks_19 <- epi_weeks %>% filter(epi_wk_no <= 20 & year == 2019)
class(epi_wks_18)
epi_1819 <- rbind(input_data=epi_wks_18,data_to_bind=epi_wks_19)

epi_dates <- as.Date(epi_1819$epi_dates)
epi_week_nos <- as.character(epi_1819$epi_wk_no)

#plot lab versus hospital rates for the UK
hosp_1819 <- ggplot() + 
     geom_line(data=uk_lab_hosp_18_19,aes(x=start_date,y=hsp_rate_flu)) +
     ggtitle('Hospital Cases 18/19') +
     xlab('Epi Week') +
     ylab('Rate per 100,000 person') +
     scale_x_date(date_breaks = epi_dates,
                  limits = c(min(epi_dates), max = max(epi_dates)),
                  labels = epi_week_nos)

hosp_1819
lab_1819 <- ggplot() + 
     geom_line(data=uk_lab_hosp_18_19,aes(x=start_date,y=rate_positive)) +
     ggtitle('Lab Cases 18/19') +
     xlab('Epi Week') +
     ylab('Rate per 100,000 persons')

lab_1819
hosp_2223 <- ggplot() + 
     geom_line(data=uk_lab_hosp_22_23,aes(x=start_date,y=hsp_rate_flu)) +
     ggtitle('Hospital Cases 22/23') +
     xlab('Epi Week') +
     ylab('Rate per 100,000 persons')
lab_2223 <- ggplot() + 
     geom_line(data=uk_lab_hosp_22_23,aes(x=start_date,y=rate_positive)) +
     ggtitle('Lab Cases 22/23') +
     xlab('Epi Week') +
     ylab('Rate per 100,000 persons')

plot_1819 <- plot_grid(hosp_1819,lab_1819,nrow=1)
plot_2223 <- plot_grid(hosp_2223,lab_2223,nrow=1)
plot_grid(plot_1819,plot_2223,nrow=2)


