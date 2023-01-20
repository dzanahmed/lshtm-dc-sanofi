# set working directory
setwd("~/lshtm-dc-sanofi")

# set up packages required for analysis
require(ggplot2)
require(tidyverse) 
require(cowplot)
require(rvest)

#read merged data file
data <- read.csv('data/merged_data/merged_data.csv')
head(data) 

#change start_date to data
data$start_date <- as.Date(data$start_date)

class(data$start_date)

#set up data sets by season

# 2016 - start wk - 2016-10-02 --> 2017-05-14
data_16 <- data %>% filter(start_date >= '2016-10-02')
data_16 <- data_16 %>% filter(start_date <= '2017-05-14')

# 2017 - start wk - 2017-10-01 --> 2018-05-13
data_17 <- data %>% filter(start_date >= '2017-10-01')
data_17 <- data_17 %>% filter(start_date <= '2018-05-13')

# 2018 - start wk - 2018-09-30 --> 2019-05-12
data_18 <- data %>% filter(start_date >= '2018-09-30')
data_18 <- data_18 %>% filter(start_date <= '2019-05-12')

# 2019 - start wk - 2019-09-29 --> 2020-05-11
data_19 <- data %>% filter(start_date >= '2019-09-29')
data_19 <- data_19 %>% filter(start_date <= '2020-05-11')

# 2022 - start wk - 2022-10-02 --> 2023-01-01
data_22 <- data %>% filter(start_date >= '2022-10-02')
data_22 <- data_22 %>% filter(start_date <= '2023-01-01')

####### UK DATA ######

## 2017/18 SEASON ##
uk_data_17 <- data_17 %>% filter(country == 'UK')
uk_data_17 <- uk_data_17 %>% filter(age_group == 'ALL')

# create Epi Week Labels
date_breaks <- uk_data_17$start_date
epi <- uk_data_17$week

#plot graph
uk_17_18_szn <- ggplot() + geom_line(data=uk_data_17,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=uk_data_17,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=uk_data_17,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for UK in 2017/18 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none')

uk_17_18_szn

## 2018/19 SEASON ##
uk_data_18 <- data_18 %>% filter(country == 'UK')
uk_data_18 <- uk_data_18 %>% filter(age_group == 'ALL')

# create Epi Week Labels
date_breaks <- uk_data_18$start_date
epi <- uk_data_18$week

#plot graph
uk_18_19_szn <- ggplot() + geom_line(data=uk_data_18,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=uk_data_18,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=uk_data_18,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for UK in 2018/19 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none')
uk_18_19_szn

## 2019/20 SEASON ##
uk_data_19 <- data_19 %>% filter(country == 'UK')
uk_data_19 <- uk_data_19 %>% filter(age_group == 'ALL')

# create Epi Week Labels
date_breaks <- uk_data_19$start_date
epi <- uk_data_19$week

#plot graph
uk_19_20_szn <- ggplot() + geom_line(data=uk_data_19,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=uk_data_19,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=uk_data_19,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for UK in 2019/20 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none')
uk_19_20_szn

## 2022/23 SEASON ##
uk_data_22 <- data_22 %>% filter(country == 'UK')
uk_data_22 <- uk_data_22 %>% filter(age_group == 'ALL')

# create Epi Week Labels using online epi calendar
url <- 'https://www.hpsc.ie/notifiablediseases/resources/epidemiologicalweeks/'
webpage <- read_html(url)

epi_tables <- html_table(webpage,header=T)
table_22 <- as.data.frame(epi_tables[[2]])
class(table_22$`Week`)
table_23 <- epi_tables[[3]]

class(table_22$Week)

szn_22 <- table_22 %>% dplyr::filter(table_22$Week >= 40)
szn_23 <- table_23 %>% dplyr::filter(table_22$Week <= 20)

#fix error for one date
szn_23$`Start date` <- gsub("30/05/2023", "30/04/2023",szn_23$`Start date`)
szn_23
epi_cal_22_23 <- rbind(szn_22,szn_23)

date_breaks <- as.Date(epi_cal_22_23$`Start date`,'%d/%m/%Y')
epi <- epi_cal_22_23$Week

date_breaks
epi

#plot graph
uk_22_23_szn <- ggplot() + geom_line(data=uk_data_22,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=uk_data_22,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_line(data=uk_data_22,aes(x=start_date,y=hsp_abs_covid,color='SARS_CoV_2')) +
     geom_col(data=uk_data_22,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS_CoV_2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for UK in 2022/23 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,
                  limits = c(min(date_breaks), max = max(date_breaks)),
                  labels=epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'bottom')
uk_22_23_szn

?scale_x_date
pre_covid <- plot_grid(uk_17_18_szn,uk_18_19_szn,uk_19_20_szn,nrow = 3)
plot_grid(pre_covid,uk_22_23_szn,rel_heights = c(2,1),nrow=2)
