### GER HOSP VS. LAB FIGURE ###
library(tidyverse)
library(ggplot2)
library(cowplot)
setwd('~/lshtm-dc-sanofi')

#read in merged data csv - USING MERGED CSV 2 FOR NOW
data <- read.csv('data/merged_data/merged_data.csv')

#create dummy data for complete 22/23 season
data[nrow(data) + 1,] = c(99999999,NA,'DE','NH',20,2023,'ALL',0,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,'2023-05-14')

data$hsp_rate <- as.numeric(data$hsp_rate)
data$epi_dates <- as.Date(data$epi_dates)
data$hsp_rate_covid19 <- as.numeric(data$hsp_rate_covid19)
data$hsp_rate_rsv <- as.numeric(data$hsp_rate_rsv)
data$hsp_rate_flu <- as.numeric(data$hsp_rate_flu)
data$cases_rate_covid19 <- as.numeric(data$cases_rate_covid19)
data$cases_rate_rsv <- as.numeric(data$cases_rate_rsv)
data$cases_rate_flu <- as.numeric(data$cases_rate_flu)

#filter for germany and all age groups
ger_data <- data %>% filter(country == 'DE' & age_group == 'ALL')

## Filter by season
ger_data$epi_dates <- as.Date(ger_data$epi_dates)
ger_data <- ger_data %>% mutate(season =
                                   case_when(epi_dates >= '2016-10-02' & epi_dates <= '2017-05-14' ~ '16/17',
                                             epi_dates >= '2017-10-01' & epi_dates <= '2018-05-13' ~ '17/18',
                                             epi_dates >= '2018-09-30' & epi_dates <= '2019-05-12' ~ '18/19',
                                             epi_dates >= '2022-10-02' & epi_dates <= '2023-05-14' ~ '22/23'
                                   )
)

# Drop rows outside of usual seasons
ger_data <- ger_data %>% drop_na(season)

#filter to only relevant columns for flu
ger_data_flu <- ger_data %>%
     select('week','year','hsp_rate_flu','cases_rate_flu','epi_dates','season')
ger_data_flu <- ger_data_flu %>% pivot_longer(cols = c('hsp_rate_flu','cases_rate_flu'),names_to = 'hos_lab_case',values_to = 'rate')

#create plot for flu
flu_plot <- ggplot() + geom_line(data=ger_data_flu,aes(x=epi_dates,y=rate,col=hos_lab_case),position='identity',size=1.5) + 
     facet_wrap(.~season,scales = 'free_x') +
     theme_bw() +
     xlab('Month') +
     ylab(expression(paste('Rate per 100,000 persons'))) +
     theme(axis.title.y = element_text(size=9)) +
     theme(legend.position = 'none') +
     theme(strip.background =element_rect(fill="white"))

#filter to only relevant columns for rsv
ger_data_rsv <- ger_data %>%
     select('week','year','hsp_rate_rsv','cases_rate_rsv','epi_dates','season')
ger_data_rsv <- ger_data_rsv %>% pivot_longer(cols = c('hsp_rate_rsv','cases_rate_rsv'),names_to = 'hos_lab_case',values_to = 'rate')

#create plot for rsv
rsv_plot <- ggplot() + geom_line(data=ger_data_rsv,aes(x=epi_dates,y=rate,col=hos_lab_case),position='identity',size=1.5) + 
     facet_wrap(.~season,scales = 'free_x') +
     theme_bw() +
     xlab('Month') +
     ylab(expression(paste('Rate per 100,000 persons'))) +
     theme(axis.title.y = element_blank()) +
     theme(strip.background =element_rect(fill="white")) +
     theme(legend.position = 'none') 

rsv_plot

#filter to only relevant columns for rsv
ger_data_covid <- ger_data %>%
     select('week','year','hsp_rate_covid19','cases_rate_covid19','epi_dates','season')
ger_data_covid <- ger_data_covid %>% pivot_longer(cols = c('hsp_rate_covid19','cases_rate_covid19'),names_to = 'hos_lab_case',values_to = 'rate')
ger_data_covid <- ger_data_covid %>% drop_na(rate)

#create plot for covid
covid_plot <- ggplot() + geom_line(data=ger_data_covid,aes(x=epi_dates,y=rate,col=hos_lab_case),position='identity',size=1.5) + 
     facet_wrap(.~season,scales='free') +
     scale_y_sqrt() +
     theme_bw() +
     xlab('Month') +
     ylab(expression(paste('Rate per 100,000 persons'))) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'bottom') +
     theme(strip.background =element_rect(fill="white")) +
     scale_color_discrete(name="",
                         breaks=c('hsp_rate_covid19','cases_rate_covid19'),
                         labels=c('Hospital Case','Laboratory Case'))

covid_plot_no_leg <- covid_plot + theme(legend.position = 'none')
covid_plot_no_leg

#create legend title
legend <- get_legend(covid_plot)

#create virus labels
title_flu <- ggdraw() + 
     draw_label("Influenza",fontface = 'bold',x = 0.4,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))
title_rsv <- ggdraw() + 
     draw_label("RSV",fontface = 'bold',x = 0.5,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))
title_covid <- ggdraw() + 
     draw_label("SARS-CoV-2",fontface = 'bold',x = 0.35,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))

virus_grid <- plot_grid(title_flu,title_rsv,title_covid,nrow=1)

#plot grid
graph_grid <- plot_grid(flu_plot,rsv_plot,covid_plot_no_leg,nrow=1)
whole_grid <- plot_grid(graph_grid,legend,nrow = 2, rel_heights = c(1,0.1))
with_title_grid <- plot_grid(virus_grid,whole_grid,nrow=2,rel_heights = c(0.1,1))
with_title_grid

ggsave('output/lab_case_figure.png',with_title_grid,width = 9, height = 4,bg = 'white')
