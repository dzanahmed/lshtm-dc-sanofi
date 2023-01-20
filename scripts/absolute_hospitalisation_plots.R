# set working directory
setwd("~/lshtm-dc-sanofi")

# set up packages required for analysis
require(ggplot2)
require(tidyverse) 

#read merged data file
data <- read.csv('data/merged_data/merged_data.csv')
head(data) 

#change start_date to data
data$start_date <- as.Date(data$start_date)

class(data$start_date)

#set up data sets by season

# 2017 - start wk - 2017-10-01 --> 2018-05-13
data_17 <- data %>% filter(start_date >= '2017-10-01')
data_17 <- data_17 %>% filter(start_date <= '2018-05-13')

#test plot seasonal data

uk_data_17 <- data_17 %>% filter(country == 'UK')
uk_data_17 <- uk_data_17 %>% filter(age_group == 'ALL')

# create Epi Week Labels

date_breaks <- uk_data_17$start_date
epi <- uk_data_17$week

ggplot() + geom_line(data=uk_data_17,aes(x=start_date,y=hsp_abs_flu),color='red') +
     geom_line(data=uk_data_17,aes(x=start_date,y=hsp_abs_rsv),color='orange') +
     geom_col(data=uk_data_17,aes(x=start_date,y=hsp_abs),alpha=0.2) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for UK in 2017/18 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


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

