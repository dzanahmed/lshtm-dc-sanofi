#import required libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)

#set working directory
setwd('~/lshtm-dc-sanofi')

#read in data
all_data <- read.csv('data/merged_data/merged_data2.csv')

### THEMATIC SETTINGS FOR GRAPHS - FLU ###
seasons <- c('16/17','17/18','18/19','22/23')
col_seasons <- c('16/17' = 'plum',
                 '17/18' = 'medium orchid',
                 '18/19' = 'dark magenta',
                 '19/20' = 'dark slate blue',
                 '22/23' = 'midnight blue')

#filter for france for analysis
fra_data <- all_data %>% filter(country == 'FR' & data_source == 'FluNet - Sentinel')
names(fra_data)
fra_data <- fra_data %>% select('epi_dates','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')
fra_data$epi_dates <- as.Date(fra_data$epi_dates)


#add column for flu season

fra_data <- fra_data %>% mutate(season =
     case_when(epi_dates >= '2016-10-02' & epi_dates <= '2017-05-14' ~ '16/17',
               epi_dates >= '2017-10-01' & epi_dates <= '2018-05-13' ~ '17/18',
               epi_dates >= '2018-09-30' & epi_dates <= '2019-05-12' ~ '18/19',
               epi_dates >= '2022-10-02' & epi_dates <= '2023-01-01' ~ '22/23'
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

fra_flu_cum <- ggplot() + geom_line(data=fra_szn_data,
                    aes(x=season_week,y=cum_flu,col=season),size=2) + 
     scale_color_manual(values = col_seasons) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative\n Case Count') +
     ggtitle('FRA') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_text(size=15)) +
     theme(axis.title.y = element_text(size=15)) +
     theme(legend.position = 'none')

fra_flu_cum

fra_szn_data <- fra_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))

### UK GRAPH ###

uk_data <- all_data %>% filter(country == 'UK' & age_group == 'ALL')

uk_data <- uk_data %>% select('epi_dates','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')
uk_data$epi_dates <- as.Date(uk_data$epi_dates)


#add column for flu season

uk_data <- uk_data %>% mutate(season =
                                     case_when(epi_dates >= '2016-10-02' & epi_dates <= '2017-05-14' ~ '16/17',
                                               epi_dates >= '2017-10-01' & epi_dates <= '2018-05-13' ~ '17/18',
                                               epi_dates >= '2018-09-30' & epi_dates <= '2019-05-12' ~ '18/19',
                                               epi_dates >= '2022-10-02' & epi_dates <= '2023-01-01' ~ '22/23'
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

uk_flu_cum <- ggplot() + geom_line(data=uk_szn_data,
                     aes(x=season_week,y=cum_flu,col=season),size=2) + 
     scale_color_manual(values = col_seasons) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative Case Count') +
     ggtitle('UK') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_text(size=15)) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')

### AUS ###

aus_data <- all_data %>% filter(country == 'AUS' & age_group == 'ALL' & data_source == 'fluNet')

aus_data <- aus_data %>% select('epi_dates','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')
aus_data$epi_dates <- as.Date(aus_data$epi_dates)


#add column for flu season

aus_data <- aus_data %>% mutate(season =
                                   case_when(week >= 16 & week <= 45 & year == 2016 ~ '16/17',
                                             week >= 16 & week <= 45 & year == 2017 ~ '17/18',
                                             week >= 16 & week <= 45 & year == 2018  ~ '18/19',
                                             week >= 16 & week <= 45 & year == 2019  ~ '19/20',
                                             week >= 16 & week <= 40 & year == 2022 ~ '22/23'
                                   )
)

aus_szn_data <- aus_data %>% drop_na(season)

#calculate cumulative sums
aus_szn_data <- aus_szn_data %>%
     group_by(season) %>%
     mutate(cum_flu = cumsum(hsp_abs_flu))

aus_szn_data <- aus_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))

aus_flu_cum <- ggplot() + geom_line(data=aus_szn_data,
                     aes(x=week,y=cum_flu,col=season),size=2) + 
     scale_color_manual(values = col_seasons) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab(paste('Cumulative\n Case Count')) +
     ggtitle('AUS') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_text(size=15)) +
     theme(legend.position = 'none')

### CHILE ###

chi_data <- all_data %>% filter(country == 'CL' & age_group == 'ALL' & data_source == 'EPI MINSAL - FluNet (Flu cases)')

chi_data <- chi_data %>% select('epi_dates','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')
chi_data$epi_dates <- as.Date(chi_data$epi_dates)


#add column for flu season

chi_data <- chi_data %>% mutate(season =
                                     case_when(week >= 16 & week <= 45 & year == 2016 ~ '16/17',
                                               week >= 16 & week <= 45 & year == 2017 ~ '17/18',
                                               week >= 16 & week <= 45 & year == 2018  ~ '18/19',
                                               week >= 16 & week <= 45 & year == 2019  ~ '19/20',
                                               week >= 16 & week <= 45 & year == 2022 ~ '22/23'
                                     )
)

chi_szn_data <- chi_data %>% drop_na(season)

#calculate cumulative sums
chi_szn_data <- chi_szn_data %>%
     group_by(season) %>%
     mutate(cum_flu = cumsum(hsp_abs_flu))

chi_szn_data <- chi_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))

chi_flu_cum_leg <- ggplot() + geom_line(data=chi_szn_data,
                     aes(x=week,y=cum_flu,col=season),size=2) + 
     scale_color_manual(values = col_seasons) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative\n Case Count') +
     ggtitle('CHI') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'bottom')

chi_flu_cum <- chi_flu_cum_leg + theme(legend.position = 'none')

## Get Legend ##
legend <- get_legend(chi_flu_cum_leg)

sh_grid <- plot_grid(aus_flu_cum,chi_flu_cum, nrow=1,rel_widths = c(1,0.9))
nh_grid <- plot_grid(fra_flu_cum,uk_flu_cum,nrow=1,rel_widths = c(1,0.9))
flu_grid <- plot_grid(sh_grid,nh_grid,legend,nrow=3,rel_heights = c(1,1,0.1))

### RSV GRAPHS ###

### THEMATIC SETTINGS FOR GRAPHS - RSV ###
seasons <- c('16/17','17/18','18/19','22/23')
col_seasons_rsv <- c('16/17' = 'sky blue',
                 '17/18' = 'corn flower blue',
                 '18/19' = 'dodger blue',
                 '19/20' = 'dark slate grey',
                 '22/23' = 'midnight blue')

## FRANCE ##

#calculate cumulative sums
fra_szn_data_rsv <- fra_szn_data %>%
     group_by(season) %>%
     mutate(cum_rsv = cumsum(hsp_abs_rsv))

#add seasonal week
fra_szn_data_rsv <- fra_szn_data_rsv %>% mutate(season_week = 
                                             case_when(week >= 40 ~ week-39,
                                                       week <= 20 ~ week+12))

fra_szn_data_rsv <- fra_szn_data_rsv %>% group_by(season) %>% mutate(max=max(cum_rsv))

fra_rsv_cum <- ggplot() + geom_line(data=fra_szn_data_rsv,
                                    aes(x=season_week,y=cum_rsv,col=season),size=2) + 
     scale_color_manual(values = col_seasons_rsv) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative\n Case Count') +
     ggtitle('FRA') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_text(size=15)) +
     theme(axis.title.y = element_text(size=15)) +
     theme(legend.position = 'none')

fra_rsv_cum

### UK GRAPH ###

#calculate cumulative sums
uk_szn_data_rsv <- uk_szn_data %>%
     group_by(season) %>%
     mutate(cum_rsv = cumsum(hsp_abs_rsv))

#add seasonal week
uk_szn_data_rsv <- uk_szn_data_rsv %>% mutate(season_week = 
                                           case_when(week >= 40 ~ week-39,
                                                     week <= 20 ~ week+12))

uk_szn_data_rsv <- uk_szn_data_rsv %>% group_by(season) %>% mutate(max=max(cum_rsv))

uk_rsv_cum <- ggplot() + geom_line(data=uk_szn_data_rsv,
                                   aes(x=season_week,y=cum_rsv,col=season),size=2) + 
     scale_color_manual(values = col_seasons_rsv) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative Case Count') +
     ggtitle('UK') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_text(size=15)) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')

### AUS ###

#calculate cumulative sums
aus_szn_data_rsv <- aus_szn_data %>%
     group_by(season) %>%
     mutate(cum_rsv = cumsum(hsp_abs_rsv))

aus_szn_data_rsv <- aus_szn_data_rsv %>% group_by(season) %>% mutate(max=max(cum_rsv))

aus_rsv_cum <- ggplot() + geom_line(data=aus_szn_data_rsv,
                                    aes(x=week,y=cum_rsv,col=season),size=2) + 
     scale_color_manual(values = col_seasons_rsv) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab(paste('Cumulative\n Case Count')) +
     ggtitle('AUS') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_text(size=15)) +
     theme(legend.position = 'none')

### CHILE ###

#calculate cumulative sums
chi_szn_data_rsv <- chi_szn_data %>%
     group_by(season) %>%
     mutate(cum_rsv = cumsum(hsp_abs_rsv)) 

chi_szn_data_rsv <- chi_szn_data_rsv %>% group_by(season) %>% mutate(max=max(cum_rsv))

chi_rsv_cum_leg <- ggplot() + geom_line(data=chi_szn_data_rsv,
                                        aes(x=week,y=cum_rsv,col=season),size=2) + 
     scale_color_manual(values = col_seasons_rsv) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative\n Case Count') +
     ggtitle('CHI') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'bottom')

chi_rsv_cum <- chi_rsv_cum_leg + theme(legend.position = 'none')

## Get Legend ##
legend_rsv <- get_legend(chi_rsv_cum_leg)

sh_grid_rsv <- plot_grid(aus_rsv_cum,chi_rsv_cum, nrow=1,rel_widths = c(1,0.9))
nh_grid_rsv <- plot_grid(fra_rsv_cum,uk_rsv_cum,nrow=1,rel_widths = c(1,0.9))
rsv_grid <- plot_grid(sh_grid_rsv,nh_grid_rsv,legend_rsv,nrow=3,rel_heights = c(1,1,0.1))

### Create Title Frames ###
title_flu <- ggdraw() + 
     draw_label("Influenza",fontface = 'bold',x = 0.25,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))

title_rsv <- ggdraw() + 
     draw_label("RSV",fontface = 'bold',x = 0.25,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))

title_grid <- plot_grid(title_flu, title_rsv)

## Virus Grid
virus_grid <- plot_grid(flu_grid,rsv_grid,nrow=1)

### Final Grid ###
plot_grid(title_grid,virus_grid,nrow=2, rel_heights = c(0.1,1))

### CALCULATE MEDIAN VALUES BY SEASON ###
# AUS FLU
med_rows <- aus_szn_data %>% 
     filter(cum_flu > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_flu))

median_case_table <- merge(aus_szn_data,med_rows,
                           by.x = c('season','cum_flu'),
                           by.y = c('season','median_case_no'))

median_case_table %>% select(1:4)

#AUS RSV
med_rows <- aus_szn_data_rsv %>% 
     filter(cum_rsv > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_rsv))

median_case_table <- merge(aus_szn_data_rsv,med_rows,
                           by.x = c('season','cum_rsv'),
                           by.y = c('season','median_case_no'))

median_case_table %>% select(1:4)

# CHI FLU
med_rows <- chi_szn_data %>% 
     filter(cum_flu > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_flu))

median_case_table <- merge(chi_szn_data,med_rows,
                           by.x = c('season','cum_flu'),
                           by.y = c('season','median_case_no'))

median_case_table %>% select(1:4)

#CHI RSV
med_rows <- chi_szn_data_rsv %>% 
     filter(cum_rsv > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_rsv))

median_case_table <- merge(chi_szn_data_rsv,med_rows,
                           by.x = c('season','cum_rsv'),
                           by.y = c('season','median_case_no'))

median_case_table %>% select(1:4)

# FRA FLU
med_rows <- fra_szn_data %>% 
     filter(cum_flu > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_flu))

median_case_table <- merge(fra_szn_data,med_rows,
                           by.x = c('season','cum_flu'),
                           by.y = c('season','median_case_no'))

median_case_table %>% select(1:4)

# FRA RSV
med_rows <- fra_szn_data_rsv %>% 
     filter(cum_rsv > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_rsv))

median_case_table <- merge(fra_szn_data_rsv,med_rows,
                           by.x = c('season','cum_rsv'),
                           by.y = c('season','median_case_no'))

median_case_table %>% select(1:4)

# UK FLU
med_rows <- uk_szn_data %>% 
     filter(cum_flu > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_flu))

median_case_table <- merge(uk_szn_data,med_rows,
                           by.x = c('season','cum_flu'),
                           by.y = c('season','median_case_no'))

median_case_table %>% select(1:4)

# UK RSV
med_rows <- uk_szn_data_rsv %>% 
     filter(cum_rsv > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_rsv))

median_case_table <- merge(uk_szn_data_rsv,med_rows,
                           by.x = c('season','cum_rsv'),
                           by.y = c('season','median_case_no'))

median_case_table %>% select(1:4)

                                                         