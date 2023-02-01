###### HOSPITALISATIONS FIG - SOUTHERN HEMISPHERE ######

#set up
setwd('~/lshtm-dc-sanofi')
require(ggplot2)
require(tidyverse) 
require(cowplot)
require(rvest)
require(scales)

#read merged data file
data <- read.csv('data/merged_data/merged_data.csv')

#change start_date to data
## changed to epi_dates using merged_data2.csv
data$epi_dates <- as.Date(data$epi_dates)

## Theme Settings
viruses <- c('Influenza' = 'red',
             'RSV' = 'orange',
             'SARS-CoV-2' = 'blue',
             'Total' = 'pink')

### CHILE ###
chi_data <- data %>% filter(country == 'CL' & age_group == 'ALL')
chi_data <- chi_data %>% rowwise() %>% 
     mutate(hsp_rate = sum(hsp_rate_flu,hsp_rate_rsv,hsp_rate_covid19,na.rm=T))
chi_data <- chi_data %>% mutate(season =
                                     case_when(week >= 16 & week <= 45 & year == 2016 ~ '16/17',
                                               week >= 16 & week <= 45 & year == 2017 ~ '17/18',
                                               week >= 16 & week <= 45 & year == 2018  ~ '18/19',
                                               week >= 16 & week <= 45 & year == 2019  ~ '19/20',
                                               week >= 16 & week <= 45 & year == 2022 ~ '22/23'
                                     )
)
chi_data <- chi_data %>% drop_na(season)

#create data sets for plotting
chi_16 <- chi_data %>% filter(year == 2016)
chi_17 <- chi_data %>% filter(year == 2017)
chi_18 <- chi_data %>% filter(year == 2018)
chi_19 <- chi_data %>% filter(year == 2019)
chi_22 <- chi_data %>% filter(year == 2022)

#generate individual plots, then merge
chi_plot_16 <- ggplot() + geom_line(data=chi_16,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=chi_16,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=chi_16,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,55) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(legend.position = 'none')

chi_plot_17 <- ggplot() + geom_line(data=chi_17,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=chi_17,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=chi_17,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,55) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.text.y = element_blank()) +
     theme(legend.position = 'none')

chi_plot_18 <- ggplot() + geom_line(data=chi_18,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=chi_18,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=chi_18,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,55) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.text.y = element_blank()) +
     theme(legend.position = 'none')

chi_plot_19 <- ggplot() + geom_line(data=chi_19,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=chi_19,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=chi_19,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,55) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.text.y = element_blank()) +
     theme(legend.position = 'none')

chi_plot_22 <- ggplot() + geom_line(data=chi_22,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=chi_22,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_line(data=chi_22,aes(x=epi_dates,y=hsp_rate_covid19,color='SARS-CoV-2')) +
     geom_col(data=chi_22,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS-CoV-2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS-CoV-2' = 'blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,55) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.text.y = element_blank()) +
     theme(legend.position = 'none')

chi_grid <- plot_grid(chi_plot_16,chi_plot_17,chi_plot_18,chi_plot_19,chi_plot_22,nrow=1,rel_widths = c(1.1,1,1,1,1))
chi_grid

### AUSTRALIA ###
aus_data <- data %>% filter(country == 'AUS' & age_group == 'ALL')
aus_data <- aus_data %>% mutate(season =
                                     case_when(week >= 16 & week <= 45 & year == 2016 ~ '16/17',
                                               week >= 16 & week <= 45 & year == 2017 ~ '17/18',
                                               week >= 16 & week <= 45 & year == 2018  ~ '18/19',
                                               week >= 16 & week <= 45 & year == 2019  ~ '19/20',
                                               week >= 16 & week <= 45 & year == 2022 ~ '22/23'
                                     )
)
aus_data <- aus_data %>% drop_na(season)

#create data sets for plotting
aus_16 <- aus_data %>% filter(year == 2016)
aus_17 <- aus_data %>% filter(year == 2017)
aus_18 <- aus_data %>% filter(year == 2018)
aus_19 <- aus_data %>% filter(year == 2019)
aus_22 <- aus_data %>% filter(year == 2022)

#generate individual plots, then merge
aus_plot_16 <- ggplot() + geom_line(data=aus_16,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=aus_16,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=aus_16,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,22) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=10)) +
     theme(legend.position = 'none')

aus_plot_17 <- ggplot() + geom_line(data=aus_17,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=aus_17,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=aus_17,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,22) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.text.y = element_blank()) +
     theme(legend.position = 'none')

aus_plot_18 <- ggplot() + geom_line(data=aus_18,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=aus_18,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=aus_18,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,22) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.text.y = element_blank()) +
     theme(legend.position = 'none')

aus_plot_19 <- ggplot() + geom_line(data=aus_19,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=aus_19,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_col(data=aus_19,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange")) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,22) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.text.y = element_blank()) +
     theme(legend.position = 'none')

aus_plot_22 <- ggplot() + geom_line(data=aus_22,aes(x=epi_dates,y=hsp_rate_flu,color='Influenza')) +
     geom_line(data=aus_22,aes(x=epi_dates,y=hsp_rate_rsv,color='RSV')) +
     geom_line(data=aus_22,aes(x=epi_dates,y=hsp_rate_covid19,color='SARS-CoV-2')) +
     geom_col(data=aus_22,aes(x=epi_dates,y=hsp_rate,fill='Total'),alpha=0.5) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV','SARS-CoV-2'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS-CoV-2' = 'blue')) +
     scale_fill_manual('',breaks=c('Total'),values=c('Total'='pink')) +
     #xlab('Epi Week') +
     #ylab(expression(paste('Hospitalisations per \n 100,000 Persons'))) +
     theme_bw() +
     ylim(0,22) +
     scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(axis.text.y = element_blank()) +
     theme(legend.position = 'none')
aus_plot_22

aus_grid <- plot_grid(aus_plot_16,aus_plot_17,aus_plot_18,aus_plot_19,aus_plot_22,nrow=1,rel_widths = c(1.1,1,1,1,1))
aus_grid

## OVERALL GRID ##

#create overall plot titles
title_16 <- ggdraw() + 
     draw_label("2016 Season",fontface = 'bold',x = 0.25,hjust = 0) +theme(plot.margin = margin(0, 0, 0, 0))
title_17 <- ggdraw() + 
     draw_label("2017 Season",fontface = 'bold',x = 0.25,hjust = 0) +theme(plot.margin = margin(0, 0, 0, 0))
title_18 <- ggdraw() + 
     draw_label("2018 Season",fontface = 'bold',x = 0.25,hjust = 0) +theme(plot.margin = margin(0, 0, 0, 0))
title_19 <- ggdraw() + 
     draw_label("2019 Season",fontface = 'bold',x = 0.25,hjust = 0) +theme(plot.margin = margin(0, 0, 0, 0))
title_22 <- ggdraw() + 
     draw_label("2022 Season",fontface = 'bold',x = 0.25,hjust = 0) +theme(plot.margin = margin(0, 0, 0, 0))

title_grid <- plot_grid(title_16,title_17,title_18,title_19,title_22,nrow=1)

#create country labels
title_aus <- ggdraw() + 
     draw_label("AUS",fontface = 'bold',x = 0.25,hjust = 0,angle = 90)+theme(plot.margin = margin(0, 0, 0, 0))
title_chi <- ggdraw() + 
     draw_label("CHI",fontface = 'bold',x = 0.25,hjust = 0,angle =90)+theme(plot.margin = margin(0, 0, 0, 0))

country_grid <- plot_grid(title_aus,title_chi,nrow=2)

## plot three countries together
graph_grid <- plot_grid(aus_grid,chi_grid,nrow=2,rel_heights = c(1,1.2))

season_plot <- plot_grid(title_grid,graph_grid,nrow=2,rel_heights = c(0.1,1))

plot_grid(country_grid,season_plot,ncol=2,rel_widths = c(0.1,2))
