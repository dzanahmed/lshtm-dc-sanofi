# set working directory
setwd("~/lshtm-dc-sanofi")

# set up packages required for analysis
require(ggplot2)
require(tidyverse) 

#read merged data file
data <- read.csv('data/merged_data/first_output_2022_01_19.csv')
head(data) 

#read epi file
epi_weeks <- read.csv('epiweeks_withdate.csv')


#join merged data and epi weeks

data <- merge(x=data,y=epi_weeks,by.x = c('week','year'), by.y = c('MMWR_WEEK','MMWR_YEAR'),all.x=TRUE)
data$MMWR_WEEKSTARTDATE <- as.Date(data$MMWR_WEEKSTARTDATE) #convert to datetime

class(data$MMWR_WEEKSTARTDATE)

# test plot with week data

uk_data <- data %>% filter(data$country == 'UK')

uk_data_22 <- uk_data %>% filter(uk_data$year == 2022 | uk_data$year == 2023)
uk_data_22 <- uk_data_22 %>% filter(uk_data_22$age_group == 'ALL')

ggplot() + geom_line(data=uk_data_22,aes(x=week,y=hsp_abs_covid),color='dark blue') +
     geom_line(data=uk_data_22,aes(x=week,y=hsp_abs_flu),color='red') +
     geom_line(data=uk_data_22,aes(x=week,y=hsp_abs_rsv),color='orange') +
     geom_col(data=uk_data_22,aes(x=week,y=hsp_abs),alpha=0.2) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for UK in 2022/23 Season') +
     theme_bw()

# test plot with seasons as x axis
