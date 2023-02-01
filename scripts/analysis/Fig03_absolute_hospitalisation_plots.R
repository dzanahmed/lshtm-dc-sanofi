# set up packages required for analysis
require(ggplot2)
require(tidyverse) 
require(cowplot)
require(rvest)
require(scales)

#read merged data file
data <- read.csv('data/merged_data/merged_data.csv')
head(data) 

#change start_date to data
## changed to epi_dates using merged_data2.csv
data$epi_dates <- as.Date(data$epi_dates)

class(data$epi_dates)

## Theme Settings
viruses <- c('Influenza' = 'red', 'RSV' = 'orange', 'SARS-CoV-2' = 'blue', 'Total' = 'pink')

#set up datasets by season for NH

# 2016 - start wk - 2016-10-02 --> 2017-05-14
data_16 <- data %>% filter(epi_dates >= '2016-10-02')
data_16 <- data_16 %>% filter(epi_dates <= '2017-05-14')

# 2017 - start wk - 2017-10-01 --> 2018-05-13
data_17 <- data %>% filter(epi_dates >= '2017-10-01')
data_17 <- data_17 %>% filter(epi_dates <= '2018-05-13')

# 2018 - start wk - 2018-09-30 --> 2019-05-12
data_18 <- data %>% filter(epi_dates >= '2018-09-30')
data_18 <- data_18 %>% filter(epi_dates <= '2019-05-12')

# 2022 - start wk - 2022-10-02 --> 2023-01-01 + APPEND until end of SZN
data_22 <- data %>% filter(epi_dates >= '2022-10-02')
data_22 <- data_22 %>% filter(epi_dates <= '2023-01-01')
data_22[nrow(data_22) + 1,] = c(99999999,NA,'UK','NH',20,2023,'ALL',0,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,'2023-05-14')
data_22[nrow(data_22) + 1,] = c(99999999,NA,'DE','NH',20,2023,'ALL',0,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,'2023-05-14')
data_22[nrow(data_22) + 1,] = c(99999999,NA,'US','NH',20,2023,'ALL',0,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,'2023-05-14')
data_22[nrow(data_22) + 1,] = c(99999999,'FluNet - Sentinel','FR','NH',20,2023,'ALL',0,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,'2023-05-14')
#data_22[nrow(data_22) + 1,] = c(99999999,NA,'UK','NH',20,2023,'ALL',0,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,'2023-05-14')
#data_22[nrow(data_22) + 1,] = c(99999999,NA,'UK','NH',20,2023,'ALL',0,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,'2023-05-14')

####### UK DATA #######

## 2017/18 SEASON ##
uk_data_17 <- data_17 %>% filter(country == 'UK')
uk_data_17 <- uk_data_17 %>% filter(age_group == 'ALL')

#plot graph
uk_17_18_szn <- ggplot() + geom_line(data=uk_data_17,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=uk_data_17,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=uk_data_17,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,30) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7) #try this instead of epi weeks, old code kept until agreed
                       #date_breaks,
                  #labels=epi
                  ) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(legend.position = 'none')

uk_17_18_szn

## 2018/19 SEASON ##
uk_data_18 <- data_18 %>% filter(country == 'UK')
uk_data_18 <- uk_data_18 %>% filter(age_group == 'ALL')

#plot graph
uk_18_19_szn <- ggplot() + geom_line(data=uk_data_18,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=uk_data_18,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=uk_data_18,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab('Hospitalisations per 100,000 Persons') +
     theme_bw() +
     ylim(0,30) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
uk_18_19_szn

## 2022/23 SEASON ##
uk_data_22 <- data_22 %>% filter(country == 'UK')
uk_data_22 <- uk_data_22 %>% filter(age_group == 'ALL')
uk_data_22$hsp_rate <- as.numeric(uk_data_22$hsp_rate)
uk_data_22$epi_dates <- as.Date(uk_data_22$epi_dates)
uk_data_22$hsp_rate_covid19 <- as.numeric(uk_data_22$hsp_rate_covid19)
uk_data_22$hsp_rate_rsv <- as.numeric(uk_data_22$hsp_rate_rsv)
uk_data_22$hsp_rate_flu <- as.numeric(uk_data_22$hsp_rate_flu)

#plot graph
uk_22_23_szn_leg <- ggplot() + geom_line(data=uk_data_22,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=uk_data_22,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_line(data=uk_data_22,aes(x=epi_dates,y=hsp_rate_covid19,color='SARS_CoV_2')) +
     geom_col(data=uk_data_22,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS_CoV_2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) + 
     theme_bw() + 
     ylim(0,30) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank())
uk_22_23_szn_leg
#drop legend
uk_22_23_szn <- uk_22_23_szn_leg + theme(legend.position = 'none')
     
uk_22_23_szn

legend <- get_legend(uk_22_23_szn_leg + theme(legend.position = 'left'))

uk_final <- plot_grid(legend,uk_17_18_szn,uk_18_19_szn,uk_22_23_szn,nrow=1,rel_widths = c(0.8,1.2,1,1))

uk_final

####### GERMANY DATA ######

## 2016/17 SEASON ##

ger_data_16 <- data_16 %>% dplyr::filter(country == 'DE')
ger_data_16 <- ger_data_16 %>% dplyr::filter(age_group == 'ALL')
ger_data_16 <- ger_data_16 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
ger_16_17_szn <- ggplot() + geom_line(data=ger_data_16,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=ger_data_16,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=ger_data_16,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2016/17 Season') +
     theme_bw() +
     ylim(0,45) +
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     #theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
ger_16_17_szn

## 2017/18 SEASON ##

ger_data_17 <- data_17 %>% dplyr::filter(country == 'DE')
ger_data_17 <- ger_data_17 %>% dplyr::filter(age_group == 'ALL')

ger_data_17 <- ger_data_17 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
ger_17_18_szn <- ggplot() + geom_line(data=ger_data_17,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=ger_data_17,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=ger_data_17,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2017/18 Season') +
     theme_bw() +
     ylim(0,45) +
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
ger_17_18_szn

## 2018/19 SEASON ##

ger_data_18 <- data_18 %>% dplyr::filter(country == 'DE')
ger_data_18 <- ger_data_18 %>% dplyr::filter(age_group == 'ALL')

ger_data_18 <- ger_data_18 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
ger_18_19_szn <- ggplot() + geom_line(data=ger_data_18,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=ger_data_18,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=ger_data_18,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2018/19 Season') +
     theme_bw() +
     ylim(0,45) + 
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
ger_18_19_szn

## 2022/23 SEASON ##

ger_data_22 <- data_22 %>% dplyr::filter(country == 'DE')
ger_data_22 <- ger_data_22 %>% dplyr::filter(age_group == 'ALL')
ger_data_22$hsp_rate <- as.numeric(ger_data_22$hsp_rate)
ger_data_22$epi_dates <- as.Date(ger_data_22$epi_dates)
ger_data_22$hsp_rate_covid19 <- as.numeric(ger_data_22$hsp_rate_covid19)
ger_data_22$hsp_rate_rsv <- as.numeric(ger_data_22$hsp_rate_rsv)
ger_data_22$hsp_rate_flu <- as.numeric(ger_data_22$hsp_rate_flu)

ger_data_22 <- ger_data_22 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
ger_22_23_szn <- ggplot() + geom_line(data=ger_data_22,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=ger_data_22,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_line(data=ger_data_22,aes(x=epi_dates,y=hsp_rate_covid19,color='SARS_CoV_2')) +
     geom_col(data=ger_data_22,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS_CoV_2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2022/23 Season') +
     theme_bw() +
     ylim(0,45) + 
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
ger_22_23_szn

#cowplot
ger_graph <- plot_grid(ger_16_17_szn,ger_17_18_szn,ger_18_19_szn,ger_22_23_szn,nrow=1,rel_widths = c(1.2,1,1,1))
ger_graph

###### FRA DATA ######

## 2016/17 SEASON ##

fra_data_16 <- data_16 %>% dplyr::filter(country == 'FR' & age_group == 'ALL' & data_source == 'FluNet - Sentinel')
fra_data_16 <- fra_data_16 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
fra_16_17_szn <- ggplot() + geom_line(data=fra_data_16,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=fra_data_16,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=fra_data_16,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2016/17 Season') +
     theme_bw() +
     #ylim(0,) +
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     #theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
fra_16_17_szn

## 2017/18 SEASON ##

fra_data_17 <- data_17 %>% dplyr::filter(country == 'FR' & age_group == 'ALL' & data_source == 'FluNet - Sentinel')

fra_data_17 <- fra_data_17 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
fra_17_18_szn <- ggplot() + geom_line(data=fra_data_17,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=fra_data_17,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=fra_data_17,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     theme_bw() +
    # ylim(0,45) +
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
fra_17_18_szn

## 2018/19 SEASON ##

fra_data_18 <- data_18 %>% dplyr::filter(country == 'FR' & age_group == 'ALL' & data_source == 'FluNet - Sentinel')

fra_data_18 <- fra_data_18 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
fra_18_19_szn <- ggplot() + geom_line(data=fra_data_18,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=fra_data_18,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=fra_data_18,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     theme_bw() +
     #ylim(0,45) + 
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
fra_18_19_szn

## 2022/23 SEASON ##

fra_data_22 <- data_22 %>% dplyr::filter(country == 'FR' & age_group == 'ALL' & data_source == 'FluNet - Sentinel')
fra_data_22$hsp_rate <- as.numeric(fra_data_22$hsp_rate)
fra_data_22$epi_dates <- as.Date(fra_data_22$epi_dates)
fra_data_22$hsp_rate_covid19 <- as.numeric(fra_data_22$hsp_rate_covid19)
fra_data_22$hsp_rate_rsv <- as.numeric(fra_data_22$hsp_rate_rsv)
fra_data_22$hsp_rate_flu <- as.numeric(fra_data_22$hsp_rate_flu)

fra_data_22 <- fra_data_22 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
fra_22_23_szn <- ggplot() + geom_line(data=fra_data_22,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=fra_data_22,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_line(data=fra_data_22,aes(x=epi_dates,y=hsp_rate_covid19,color='SARS_CoV_2')) +
     geom_col(data=fra_data_22,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS_CoV_2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     theme_bw() +
     #ylim(0,45) + 
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     #theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
fra_22_23_szn

#cowplot
fra_graph <- plot_grid(fra_16_17_szn,fra_17_18_szn,fra_18_19_szn,fra_22_23_szn,nrow=1,rel_widths = c(1.2,1,1,1))
fra_graph

###### USA DATA ######

## 2016/17 SEASON ##

usa_data_16 <- data_16 %>% dplyr::filter(country == 'US')
usa_data_16 <- usa_data_16 %>% dplyr::filter(age_group == 'ALL')

usa_data_16 <- usa_data_16 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
usa_16_17_szn <- ggplot() + geom_line(data=usa_data_16,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=usa_data_16,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=usa_data_16,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     xlab('Epi Week') +
     ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for USA in 2016/17 Season') +
     theme_bw() +
     ylim(0,25) +
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     #theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     #theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
usa_16_17_szn

## 2017/18 SEASON ##

usa_data_17 <- data_17 %>% dplyr::filter(country == 'US')
usa_data_17 <- usa_data_17 %>% dplyr::filter(age_group == 'ALL')

usa_data_17 <- usa_data_17 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
usa_17_18_szn <- ggplot() + geom_line(data=usa_data_17,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=usa_data_17,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=usa_data_17,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for USA in 2017/18 Season') +
     theme_bw() +
     ylim(0,25) +
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_text(size=10)) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
usa_17_18_szn

## 2018/19 SEASON ##

usa_data_18 <- data_18 %>% dplyr::filter(country == 'US')
usa_data_18 <- usa_data_18 %>% dplyr::filter(age_group == 'ALL')

usa_data_18 <- usa_data_18 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
usa_18_19_szn <- ggplot() + geom_line(data=usa_data_18,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=usa_data_18,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=usa_data_18,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for USA in 2018/19 Season') +
     theme_bw() +
     ylim(0,25) +
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_text(size=10)) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')

usa_18_19_szn

## 2022/23 SEASON ##

usa_data_22 <- data_22 %>% dplyr::filter(country == 'US')
usa_data_22 <- usa_data_22 %>% dplyr::filter(age_group == 'ALL')
usa_data_22$hsp_rate <- as.numeric(usa_data_22$hsp_rate)
usa_data_22$epi_dates <- as.Date(usa_data_22$epi_dates)
usa_data_22$hsp_rate_covid19 <- as.numeric(usa_data_22$hsp_rate_covid19)
usa_data_22$hsp_rate_rsv <- as.numeric(usa_data_22$hsp_rate_rsv)
usa_data_22$hsp_rate_flu <- as.numeric(usa_data_22$hsp_rate_flu)

usa_data_22 <- usa_data_22 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

#plot graph
usa_22_23_szn <- ggplot() + geom_line(data=usa_data_22,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=usa_data_22,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_line(data=usa_data_22,aes(x=epi_dates,y=hsp_rate_covid19,color='SARS_CoV_2')) +
     geom_col(data=usa_data_22,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS_CoV_2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     theme_bw() +
     ylim(0,25) +
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_text(size=10)) +
     #theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.text.y = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
usa_22_23_szn

#cowplot
usa_graph <- plot_grid(usa_16_17_szn,usa_17_18_szn,usa_18_19_szn,usa_22_23_szn,nrow=1,rel_widths = c(1.1,1,1,1))
usa_graph


#save_plot(
 #       file = 'output/Fig 03 - Hospitalization by country/usa_hospitalisation_totals.jpeg',
  #      plot = usa_graph,
   #     base_width = 15,
    #    base_height = 9
#)

#create overall plot titles
title_1617 <- ggdraw() + 
     draw_label("2016/17 Season",fontface = 'bold',x = 0.25,hjust = 0) +theme(plot.margin = margin(0, 0, 0, 0))
title_1718 <- ggdraw() + 
     draw_label("2017/18 Season",fontface = 'bold',x = 0.25,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))
title_1819 <- ggdraw() + 
     draw_label("2018/19 Season",fontface = 'bold',x = 0.25,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))
title_2223 <- ggdraw() + 
     draw_label("2022/23 Season",fontface = 'bold',x = 0.25,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))

title_grid <- plot_grid(title_1617,title_1718,title_1819,title_2223,nrow=1)

#create country labels
title_uk <- ggdraw() + 
     draw_label("UK",fontface = 'bold',x = 0.25,hjust = 0,angle = 90)+theme(plot.margin = margin(0, 0, 0, 0))
title_ger <- ggdraw() + 
     draw_label("GER",fontface = 'bold',x = 0.25,hjust = 0,angle =90)+theme(plot.margin = margin(0, 0, 0, 0))
title_fra <- ggdraw() + 
     draw_label("FRA",fontface = 'bold',x = 0.25,hjust = 0,angle =90)+theme(plot.margin = margin(0, 0, 0, 0))
title_usa <- ggdraw() + 
     draw_label("USA",fontface = 'bold',x = 0.25,hjust = 0,angle=90)+theme(plot.margin = margin(0, 0, 0, 0))

country_grid <- plot_grid(title_uk,title_ger,title_fra,title_usa,nrow=4)
country_grid
## plot three countries together
graph_grid <- plot_grid(uk_final,ger_graph,fra_graph,usa_graph,nrow=4,rel_heights = c(1,1,1,1.2))

season_plot <- plot_grid(title_grid,graph_grid,nrow=2,rel_heights = c(0.1,1))

plot_grid(country_grid,season_plot,ncol=2,rel_widths = c(0.1,2))
