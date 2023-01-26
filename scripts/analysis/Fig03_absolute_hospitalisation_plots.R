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

####### UK DATA #######

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

pre_covid <- plot_grid(uk_17_18_szn,uk_18_19_szn,uk_19_20_szn,nrow = 3)
uk_final <- plot_grid(pre_covid,uk_22_23_szn,rel_heights = c(2,1),nrow=2)


save_plot(
        file = 'output/Fig 03 - Hospitalization by country/uk_hospitalisation_totals.jpeg',
        plot = uk_final,
        base_width = 15,
        base_height = 9
)

####### GERMANY DATA ######

## 2016/17 SEASON ##

ger_data_16 <- data_16 %>% dplyr::filter(country == 'DE')
ger_data_16 <- ger_data_16 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with GER population for 2016 == 82.5 million (from Statistisches Bundesamt)
ger_data_16$denominator <- 82500000

#calculate absolute values for flu and rsv based on rates data
flu_rate_16 <- ger_data_16$hsp_rate_flu
flu_abs_16 <- floor(flu_rate_16*82500000/100000)
ger_data_16$hsp_abs_flu <- flu_abs_16

rsv_rate_16 <- ger_data_16$hsp_rate_rsv
rsv_abs_16 <- floor(rsv_rate_16*(82500000/100000))
ger_data_16$hsp_abs_rsv <- rsv_abs_16

hosp_total <- flu_abs_16 + rsv_abs_16
ger_data_16$hsp_abs <- hosp_total

# create Epi Week Labels
date_breaks <- ger_data_16$start_date
epi <- ger_data_16$week

#plot graph
ger_16_17_szn <- ggplot() + geom_line(data=ger_data_16,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=ger_data_16,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=ger_data_16,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2016/17 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     ylim(0,60000) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none')
ger_16_17_szn

## 2017/18 SEASON ##

ger_data_17 <- data_17 %>% dplyr::filter(country == 'DE')
ger_data_17 <- ger_data_17 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with GER population for 2017 == 82.7 million (from Statistisches Bundesamt)
ger_data_17$denominator <- 82700000

#calculate absolute values for flu and rsv based on rates data
flu_rate_17 <- ger_data_17$hsp_rate_flu
flu_abs_17 <- floor(flu_rate_17*82700000/100000)
ger_data_17$hsp_abs_flu <- flu_abs_17

rsv_rate_17 <- ger_data_17$hsp_rate_rsv
rsv_abs_17 <- floor(rsv_rate_17*(82700000/100000))
ger_data_17$hsp_abs_rsv <- rsv_abs_17

hosp_total <- flu_abs_17 + rsv_abs_17
ger_data_17$hsp_abs <- hosp_total

# create Epi Week Labels
date_breaks <- ger_data_17$start_date
epi <- ger_data_17$week

#plot graph
ger_17_18_szn <- ggplot() + geom_line(data=ger_data_17,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=ger_data_17,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=ger_data_17,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2017/18 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     ylim(0,60000) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none')
ger_17_18_szn

## 2018/19 SEASON ##

ger_data_18 <- data_18 %>% dplyr::filter(country == 'DE')
ger_data_18 <- ger_data_18 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with GER population for 2018 == 82.9 million (from Statistisches Bundesamt)
ger_data_18$denominator <- 82900000

#calculate absolute values for flu and rsv based on rates data
flu_rate_18 <- ger_data_18$hsp_rate_flu
flu_abs_18 <- floor(flu_rate_18*82900000/100000)
ger_data_18$hsp_abs_flu <- flu_abs_18

rsv_rate_18 <- ger_data_18$hsp_rate_rsv
rsv_abs_18 <- floor(rsv_rate_18*(82900000/100000))
ger_data_18$hsp_abs_rsv <- rsv_abs_18

hosp_total <- flu_abs_18 + rsv_abs_18
ger_data_18$hsp_abs <- hosp_total

# create Epi Week Labels
date_breaks <- ger_data_18$start_date
epi <- ger_data_18$week

#plot graph
ger_18_19_szn <- ggplot() + geom_line(data=ger_data_18,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=ger_data_18,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=ger_data_18,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2018/19 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     ylim(0,60000) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none')
ger_18_19_szn

## 2019/20 SEASON ##

ger_data_19 <- data_19 %>% dplyr::filter(country == 'DE')
ger_data_19 <- ger_data_19 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with GER population for 2018 == 83.1 million (from Statistisches Bundesamt)
ger_data_19$denominator <- 83100000

#calculate absolute values for flu and rsv based on rates data
flu_rate_19 <- ger_data_19$hsp_rate_flu
flu_abs_19 <- floor(flu_rate_19*83100000/100000)
ger_data_19$hsp_abs_flu <- flu_abs_19

rsv_rate_19 <- ger_data_19$hsp_rate_rsv
rsv_abs_19 <- floor(rsv_rate_19*(83100000/100000))
ger_data_19$hsp_abs_rsv <- rsv_abs_19

hosp_total <- flu_abs_19 + rsv_abs_19
ger_data_19$hsp_abs <- hosp_total

# create Epi Week Labels - use UK here to get full season
date_breaks <- uk_data_19$start_date
epi <- uk_data_19$week

#plot graph
ger_19_20_szn <- ggplot() + geom_line(data=ger_data_19,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=ger_data_19,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=ger_data_19,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2019/20 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,
                  limits = c(min(date_breaks), max = max(date_breaks)),
                  labels=epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none') +
     ylim(0,60000) 
ger_19_20_szn

## 2022/23 SEASON ##

ger_data_22 <- data_22 %>% dplyr::filter(country == 'DE')
ger_data_22 <- ger_data_22 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with GER population for 2018 == 84.3 million (from Statistisches Bundesamt)
ger_data_22$denominator <- 84300000

#calculate absolute values for flu and rsv based on rates data
flu_rate_22 <- ger_data_22$hsp_rate_flu
flu_abs_22 <- floor(flu_rate_22*84300000/100000)
ger_data_22$hsp_abs_flu <- flu_abs_22

rsv_rate_22 <- ger_data_22$hsp_rate_rsv
rsv_abs_22 <- floor(rsv_rate_22*(84300000/100000))
ger_data_22$hsp_abs_rsv <- rsv_abs_22

covid_rate_22 <- ger_data_22$hsp_rate_covid19
covid_abs_22 <- floor(covid_rate_22*(84300000/100000))
ger_data_22$hsp_abs_covid <- covid_abs_22

hosp_total <- flu_abs_22 + rsv_abs_22 + covid_abs_22
ger_data_22$hsp_abs <- hosp_total

# create Epi Week Labels - use epi cal data to get full season
date_breaks <- as.Date(epi_cal_22_23$`Start date`,'%d/%m/%Y')
epi <- epi_cal_22_23$Week

#plot graph
ger_22_23_szn <- ggplot() + geom_line(data=ger_data_22,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=ger_data_22,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_line(data=ger_data_22,aes(x=start_date,y=hsp_abs_covid,color='SARS_CoV_2')) +
     geom_col(data=ger_data_22,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS_CoV_2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for GER in 2022/23 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,
                  limits = c(min(date_breaks), max = max(date_breaks)),
                  labels=epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     ylim(0,700000) +
     theme(legend.position = 'bottom')
ger_22_23_szn

#cowplot
pre_covid_ger <- plot_grid(ger_16_17_szn,ger_17_18_szn,ger_18_19_szn,ger_19_20_szn,nrow=4)
ger_graph <- plot_grid(pre_covid_ger,ger_22_23_szn,nrow=2,rel_heights = c(2.2,1))
ger_graph


save_plot(
        file = 'output/Fig 03 - Hospitalization by country/ger_hospitalisation_totals.jpeg',
        plot = ger_graph,
        base_width = 15,
        base_height = 9
)

###### USA DATA ######

## 2016/17 SEASON ##

usa_data_16 <- data_16 %>% dplyr::filter(country == 'US')
usa_data_16 <- usa_data_16 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with USA population for 2016 == 323.1 million
usa_data_16$denominator <- 323100000

#calculate absolute values for flu and rsv based on rates data
flu_rate_16 <- usa_data_16$hsp_rate_flu
flu_abs_16 <- floor(flu_rate_16*323100000/100000)
usa_data_16$hsp_abs_flu <- flu_abs_16

rsv_rate_16 <- usa_data_16$hsp_rate_rsv
rsv_abs_16 <- floor(rsv_rate_16*(323100000/100000))
usa_data_16$hsp_abs_rsv <- rsv_abs_16

hosp_total <- flu_abs_16 + rsv_abs_16
usa_data_16$hsp_abs <- hosp_total

# create Epi Week Labels
date_breaks <- usa_data_16$start_date
epi <- usa_data_16$week

#plot graph
usa_16_17_szn <- ggplot() + geom_line(data=usa_data_16,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=usa_data_16,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=usa_data_16,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for USA in 2016/17 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     ylim(0,70000) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none')
usa_16_17_szn

## 2017/18 SEASON ##

usa_data_17 <- data_17 %>% dplyr::filter(country == 'US')
usa_data_17 <- usa_data_17 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with USA population for 2017 == 325.1 million
usa_data_17$denominator <- 325100000

#calculate absolute values for flu and rsv based on rates data
flu_rate_17 <- usa_data_17$hsp_rate_flu
flu_abs_17 <- floor(flu_rate_17*325100000/100000)
usa_data_17$hsp_abs_flu <- flu_abs_17

rsv_rate_17 <- usa_data_17$hsp_rate_rsv
rsv_abs_17 <- floor(rsv_rate_17*(325100000/100000))
usa_data_17$hsp_abs_rsv <- rsv_abs_17

hosp_total <- flu_abs_17 + rsv_abs_17
usa_data_17$hsp_abs <- hosp_total

# create Epi Week Labels
date_breaks <- usa_data_17$start_date
epi <- usa_data_17$week

#plot graph
usa_17_18_szn <- ggplot() + geom_line(data=usa_data_17,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=usa_data_17,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=usa_data_17,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for USA in 2017/18 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     ylim(0,70000) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none')
usa_17_18_szn

## 2018/19 SEASON ##

usa_data_18 <- data_18 %>% dplyr::filter(country == 'US')
usa_data_18 <- usa_data_18 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with USA population for 2018 == 326.8 million
usa_data_18$denominator <- 326800000

#calculate absolute values for flu and rsv based on rates data
flu_rate_18 <- usa_data_18$hsp_rate_flu
flu_abs_18 <- floor(flu_rate_18*326800000/100000)
usa_data_18$hsp_abs_flu <- flu_abs_18

rsv_rate_18 <- usa_data_18$hsp_rate_rsv
rsv_abs_18 <- floor(rsv_rate_18*(326800000/100000))
usa_data_18$hsp_abs_rsv <- rsv_abs_18

hosp_total <- flu_abs_18 + rsv_abs_18
usa_data_18$hsp_abs <- hosp_total

# create Epi Week Labels
date_breaks <- usa_data_18$start_date
epi <- usa_data_18$week

#plot graph
usa_18_19_szn <- ggplot() + geom_line(data=usa_data_18,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=usa_data_18,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_col(data=usa_data_18,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for USA in 2018/19 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,labels=epi) +
     ylim(0,70000) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     theme(legend.position = 'none')

usa_18_19_szn

## 2019/20 SEASON ## There is no data for this!!

#usa_data_19 <- data_19 %>% dplyr::filter(country == 'US')
#usa_data_19 <- usa_data_19 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with USA population for 2019 == 328.3 million
#usa_data_19$denominator <- 328300000

#calculate absolute values for flu and rsv based on rates data
#flu_rate_19 <- usa_data_19$hsp_rate_flu
#flu_abs_19 <- floor(flu_rate_19*328300000/100000)
#usa_data_19$hsp_abs_flu <- flu_abs_19

#rsv_rate_19 <- usa_data_19$hsp_rate_rsv
#rsv_abs_19 <- floor(rsv_rate_19*(328300000/100000))
#usa_data_19$hsp_abs_rsv <- rsv_abs_19

#hosp_total <- flu_abs_19 + rsv_abs_19
#usa_data_19$hsp_abs <- hosp_total

# create Epi Week Labels - use UK here to get full season
#date_breaks <- uk_data_19$start_date
#epi <- uk_data_19$week

#plot graph
#usa_19_20_szn <- ggplot() + geom_line(data=usa_data_19,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     #geom_line(data=usa_data_19,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     #geom_col(data=usa_data_19,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     #scale_color_manual("", 
     #                   breaks = c('Influenza','RSV'),
     #                   values = c("Influenza"="red", "RSV"="orange")) +
     #scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     #xlab('Epi Week') +
     #ylab('No. Hospitalisations') +
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for USA in 2019/20 Season') +
     #theme_bw() +
     #scale_x_date(breaks = date_breaks,labels=epi) +
     #ylim(0,60000) +
     #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     #theme(legend.position = 'none')
#usa_19_20_szn

## 2022/23 SEASON ##

usa_data_22 <- data_22 %>% dplyr::filter(country == 'US')
usa_data_22 <- usa_data_22 %>% dplyr::filter(age_group == 'ALL')

#replace denominator with USA population for 2022 == 333.3 million
usa_data_22$denominator <- 333300000

#calculate absolute values for flu and rsv based on rates data
flu_rate_22 <- usa_data_22$hsp_rate_flu
flu_abs_22 <- floor(flu_rate_22*333300000/100000)
usa_data_22$hsp_abs_flu <- flu_abs_22

rsv_rate_22 <- usa_data_22$hsp_rate_rsv
rsv_abs_22 <- floor(rsv_rate_22*(333300000/100000))
usa_data_22$hsp_abs_rsv <- rsv_abs_22

covid_rate_22 <- usa_data_22$hsp_rate_covid19
covid_abs_22 <- floor(covid_rate_22*(333300000/100000))
usa_data_22$hsp_abs_covid <- covid_abs_22

hosp_total <- flu_abs_22 + rsv_abs_22 + covid_abs_22
usa_data_22$hsp_abs <- hosp_total

# create Epi Week Labels - use epi cal data to get full season
date_breaks <- as.Date(epi_cal_22_23$`Start date`,'%d/%m/%Y')
epi <- epi_cal_22_23$Week

#plot graph
usa_22_23_szn <- ggplot() + geom_line(data=usa_data_22,aes(x=start_date,y=hsp_abs_flu,color='Influenza')) +
     geom_line(data=usa_data_22,aes(x=start_date,y=hsp_abs_rsv,color='RSV')) +
     geom_line(data=usa_data_22,aes(x=start_date,y=hsp_abs_covid,color='SARS_CoV_2')) +
     geom_col(data=usa_data_22,aes(x=start_date,y=hsp_abs,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS_CoV_2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey')) +
     xlab('Epi Week') +
     ylab('No. Hospitalisations') +
     ggtitle('Total Hospitalisations and Hospitalisations by Virus for USA in 2022/23 Season') +
     theme_bw() +
     scale_x_date(breaks = date_breaks,
                  limits = c(min(date_breaks), max = max(date_breaks)),
                  labels=epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     ylim(0,75000) +
     theme(legend.position = 'bottom')
usa_22_23_szn

#cowplot
pre_covid_usa <- plot_grid(usa_16_17_szn,usa_17_18_szn,usa_18_19_szn,nrow=3)
usa_graph <- plot_grid(pre_covid_usa,usa_22_23_szn,nrow=2,rel_heights = c(2.2,1))
usa_graph


save_plot(
        file = 'output/Fig 03 - Hospitalization by country/usa_hospitalisation_totals.jpeg',
        plot = usa_graph,
        base_width = 15,
        base_height = 9
)
