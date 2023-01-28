# set up packages required for analysis
require(ggplot2)
require(tidyverse) 
require(cowplot)
require(rvest)
require(scales)

#read merged data file
data <- read.csv('data/merged_data/merged_data2.csv')
head(data) 

#change start_date to data
## changed to epi_dates using merged_data2.csv
data$epi_dates <- as.Date(data$epi_dates)

class(data$epi_dates)

#set up data sets by season for NH

# 2016 - start wk - 2016-10-02 --> 2017-05-14
data_16 <- data %>% filter(epi_dates >= '2016-10-02')
data_16 <- data_16 %>% filter(epi_dates <= '2017-05-14')

# 2017 - start wk - 2017-10-01 --> 2018-05-13
data_17 <- data %>% filter(epi_dates >= '2017-10-01')
data_17 <- data_17 %>% filter(epi_dates <= '2018-05-13')

# 2018 - start wk - 2018-09-30 --> 2019-05-12
data_18 <- data %>% filter(epi_dates >= '2018-09-30')
data_18 <- data_18 %>% filter(epi_dates <= '2019-05-12')

# 2022 - start wk - 2022-10-02 --> 2023-01-01
data_22 <- data %>% filter(epi_dates >= '2022-10-02')
data_22 <- data_22 %>% filter(epi_dates <= '2023-01-01')

####### UK DATA #######

## 2017/18 SEASON ##
uk_data_17 <- data_17 %>% filter(country == 'UK')
uk_data_17 <- uk_data_17 %>% filter(age_group == 'ALL')

# create Epi Week Labels
date_breaks <- uk_data_17$epi_dates
epi <- uk_data_17$week

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
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for UK in 2017/18 Season') +
     theme_bw() +
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

# create Epi Week Labels
date_breaks <- uk_data_18$epi_dates
epi <- uk_data_18$week

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
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for UK in 2018/19 Season') +
     theme_bw() +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
uk_18_19_szn

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
uk_22_23_szn_leg <- ggplot() + geom_line(data=uk_data_22,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=uk_data_22,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_line(data=uk_data_22,aes(x=epi_dates,y=hsp_rate_covid19,color='SARS_CoV_2')) +
     geom_col(data=uk_data_22,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS_CoV_2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) + 
     theme_bw() + 
     scale_x_date(breaks = '1 month',date_labels = '%b') +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.title.y = element_blank())

#drop legend
uk_22_23_szn <- uk_22_23_szn_leg + theme(legend.position = 'none')
     
uk_22_23_szn

uk_16_17 <- ggplot() + geom_line() + theme_bw()

legend <- get_legend(uk_22_23_szn_leg + theme(legend.position = 'left'))

uk_final <- plot_grid(legend,uk_17_18_szn,uk_18_19_szn,uk_22_23_szn,nrow=1)

uk_final
#save_plot(
        #file = 'output/Fig 03 - Hospitalization by country/uk_hospitalisation_totals.jpeg',
        #plot = uk_final,
        #base_width = 15,
        #base_height = 9
#)

####### GERMANY DATA ######

## 2016/17 SEASON ##

ger_data_16 <- data_16 %>% dplyr::filter(country == 'DE')
ger_data_16 <- ger_data_16 %>% dplyr::filter(age_group == 'ALL')
ger_data_16 <- ger_data_16 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

# create Epi Week Labels
date_breaks <- ger_data_16$epi_dates
epi <- ger_data_16$week

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

# create Epi Week Labels
date_breaks <- ger_data_17$epi_dates
epi <- ger_data_17$week

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
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
ger_17_18_szn

## 2018/19 SEASON ##

ger_data_18 <- data_18 %>% dplyr::filter(country == 'DE')
ger_data_18 <- ger_data_18 %>% dplyr::filter(age_group == 'ALL')

ger_data_18 <- ger_data_18 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

# create Epi Week Labels
date_breaks <- ger_data_18$epi_dates
epi <- ger_data_18$week

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
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
ger_18_19_szn

## 2022/23 SEASON ##

ger_data_22 <- data_22 %>% dplyr::filter(country == 'DE')
ger_data_22 <- ger_data_22 %>% dplyr::filter(age_group == 'ALL')

ger_data_22 <- ger_data_22 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

# create Epi Week Labels - use epi cal data to get full season
date_breaks <- as.Date(epi_cal_22_23$`Start date`,'%d/%m/%Y')
epi <- epi_cal_22_23$Week

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
     scale_x_date(breaks = scales::pretty_breaks(n=5),date_labels = '%b') +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
ger_22_23_szn

#cowplot
ger_graph <- plot_grid(ger_16_17_szn,ger_17_18_szn,ger_18_19_szn,ger_22_23_szn,nrow=1)
ger_graph


#save_plot(
        #file = 'output/Fig 03 - Hospitalization by country/ger_hospitalisation_totals.jpeg',
        #plot = ger_graph,
        #base_width = 15,
        #base_height = 9
#)

###### USA DATA ######

## 2016/17 SEASON ##

usa_data_16 <- data_16 %>% dplyr::filter(country == 'US')
usa_data_16 <- usa_data_16 %>% dplyr::filter(age_group == 'ALL')

usa_data_16 <- usa_data_16 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

# create Epi Week Labels
date_breaks <- usa_data_16$epi_dates
epi <- usa_data_16$week

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

# create Epi Week Labels
date_breaks <- usa_data_17$epi_dates
epi <- usa_data_17$week

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
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_text(size=10)) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
usa_17_18_szn

## 2018/19 SEASON ##

usa_data_18 <- data_18 %>% dplyr::filter(country == 'US')
usa_data_18 <- usa_data_18 %>% dplyr::filter(age_group == 'ALL')

usa_data_18 <- usa_data_18 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

# create Epi Week Labels
date_breaks <- usa_data_18$epi_dates
epi <- usa_data_18$week

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
     scale_x_date(breaks = scales::pretty_breaks(n=7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_text(size=10)) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')

usa_18_19_szn

## 2022/23 SEASON ##

usa_data_22 <- data_22 %>% dplyr::filter(country == 'US')
usa_data_22 <- usa_data_22 %>% dplyr::filter(age_group == 'ALL')

usa_data_22 <- usa_data_22 %>% 
     rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))

# create Epi Week Labels - use epi cal data to get full season
date_breaks <- as.Date(epi_cal_22_23$`Start date`,'%d/%m/%Y')
epi <- epi_cal_22_23$Week

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
     #ggtitle('Total Hospitalisations and Hospitalisations by Virus for USA in 2022/23 Season') +
     theme_bw() +
     scale_x_date(breaks = scales::pretty_breaks(n=5),date_labels = '%b') +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size =10)) +
     theme(axis.title.x = element_text(size=10)) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')
usa_22_23_szn

#cowplot
usa_graph <- plot_grid(usa_16_17_szn,usa_17_18_szn,usa_18_19_szn,usa_22_23_szn,nrow=1,rel_widths = c(1,1,1,1))
usa_graph


save_plot(
        file = 'output/Fig 03 - Hospitalization by country/usa_hospitalisation_totals.jpeg',
        plot = usa_graph,
        base_width = 15,
        base_height = 9
)

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
title_usa <- ggdraw() + 
     draw_label("USA",fontface = 'bold',x = 0.25,hjust = 0,angle=90)+theme(plot.margin = margin(0, 0, 0, 0))

country_grid <- plot_grid(title_uk,title_ger,title_usa,nrow=3)
country_grid
## plot three countries together
graph_grid <- plot_grid(uk_final,ger_graph,usa_graph,nrow=3,rel_heights = c(1,1,1.2))

season_plot <- plot_grid(title_grid,graph_grid,nrow=2,rel_heights = c(0.1,1))

plot_grid(country_grid,season_plot,ncol=2,rel_widths = c(0.1,2))
