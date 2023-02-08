### SUPPLEMENTARY FIGURE - CUMULATIVE GRAPH ###
### MEDIAN CASE WEEK TABLE ###

#import required libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(forcats)

#set working directory - amend as required
setwd('~/lshtm-dc-sanofi')

#read in data
all_data <- read.csv('data/merged_data/merged_data.csv')

### THEMATIC SETTINGS FOR GRAPHS###
col_seasons_nh <- c('2016/17' = '#8f90b3',
                 '2017/18' = '#EEB702',
                 '2018/19' = '#758B59',
                 '2022/23' = '#DC143C')

col_seasons_sh <- c('2016' = '#8f90b3',
                    '2017' = '#EEB702',
                    '2018' = '#758B59',
                    '2019' = '#dc9334',
                    '2022' = '#DC143C')

##### FLU GRAPHS #####

### FRANCE ###
#filter for France for analysis
fra_data <- all_data %>% filter(country == 'FR' & data_source == 'FluNet - Sentinel')
fra_data <- fra_data %>% select('epi_dates','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')
fra_data$epi_dates <- as.Date(fra_data$epi_dates)

#add column for flu season
fra_data <- fra_data %>% mutate(season =
     case_when(epi_dates >= '2016-10-02' & epi_dates <= '2017-05-14' ~ '2016/17',
               epi_dates >= '2017-10-01' & epi_dates <= '2018-05-13' ~ '2017/18',
               epi_dates >= '2018-09-30' & epi_dates <= '2019-05-12' ~ '2018/19',
               epi_dates >= '2022-10-02' & epi_dates <= '2023-01-01' ~ '2022/23'
     )
)

#Drop rows with NA for season as not used for analysis
fra_szn_data <- fra_data %>% drop_na(season)

#calculate cumulative sums
fra_szn_data <- fra_szn_data %>%
     group_by(season) %>%
     mutate(cum_flu = cumsum(hsp_abs_flu))

#add seasonal week for x-axis
fra_szn_data <- fra_szn_data %>% mutate(season_week = 
                                             case_when(week >= 40 ~ week-39,
                                                       week <= 20 ~ week+12))

#Plot graph (with legend to extract for later)
fra_flu_cum_leg <- ggplot() + geom_line(data=fra_szn_data,
                    aes(x=season_week,y=cum_flu,col=season),size=1.5) + 
     scale_color_manual(values = col_seasons_nh) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative\n Case Count') +
     ggtitle('FRA') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_text(size=15)) +
     theme(legend.position = 'right') +
     labs(col = 'Season')

## extract legend then remove it from the graph
legend_nh <- get_legend(fra_flu_cum_leg)
fra_flu_cum <- fra_flu_cum_leg + theme(legend.position = 'none')

### UK GRAPH ###
#Extract UK data, selecting relevant columns
uk_data <- all_data %>% filter(country == 'UK' & age_group == 'ALL')
uk_data <- uk_data %>% select('epi_dates','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')

#Change to date class
uk_data$epi_dates <- as.Date(uk_data$epi_dates)

#add column for flu season
uk_data <- uk_data %>% mutate(season =
                                     case_when(epi_dates >= '2016-10-02' & epi_dates <= '2017-05-14' ~ '2016/17',
                                               epi_dates >= '2017-10-01' & epi_dates <= '2018-05-13' ~ '2017/18',
                                               epi_dates >= '2018-09-30' & epi_dates <= '2019-05-12' ~ '2018/19',
                                               epi_dates >= '2022-10-02' & epi_dates <= '2023-01-01' ~ '2022/23'
                                     )
)

#Drop season rows with NA values as not for analysis
uk_szn_data <- uk_data %>% drop_na(season)

#calculate cumulative sums
uk_szn_data <- uk_szn_data %>%
     group_by(season) %>%
     mutate(cum_flu = cumsum(hsp_abs_flu))

#add seasonal week
uk_szn_data <- uk_szn_data %>% mutate(season_week = 
                                             case_when(week >= 40 ~ week-39,
                                                       week <= 20 ~ week+12))

#plot UK cumulatative graph
uk_flu_cum <- ggplot() + geom_line(data=uk_szn_data,
                     aes(x=season_week,y=cum_flu,col=season),size=1.5) + 
     scale_color_manual(values = col_seasons_nh) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative Case Count') +
     ggtitle('UK') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')

### AUS ###
#Filter data and relevant columns
aus_data <- all_data %>% filter(country == 'AUS' & age_group == 'ALL')
aus_data <- aus_data %>% select('epi_dates','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')

#Correct Date class
aus_data$epi_dates <- as.Date(aus_data$epi_dates)

#add column for flu season
aus_data <- aus_data %>% mutate(season =
                                   case_when(week >= 16 & week <= 45 & year == 2016 ~ '2016',
                                             week >= 16 & week <= 45 & year == 2017 ~ '2017',
                                             week >= 16 & week <= 45 & year == 2018  ~ '2018',
                                             week >= 16 & week <= 45 & year == 2019  ~ '2019',
                                             week >= 16 & week <= 40 & year == 2022 ~ '2022'
                                   )
)

#Drop season rows with NA as not included in analysis
aus_szn_data <- aus_data %>% drop_na(season)

#calculate cumulative sums
aus_szn_data <- aus_szn_data %>%
     group_by(season) %>%
     mutate(cum_flu = cumsum(hsp_abs_flu))

#Plot graph
aus_flu_cum <- ggplot() + geom_line(data=aus_szn_data,
                     aes(x=week,y=cum_flu,col=season),size=1.5) + 
     scale_color_manual(values = col_seasons_sh) +
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
#Select Chile data and relevant columns
chi_data <- all_data %>% filter(country == 'CL' & age_group == 'ALL')
chi_data <- chi_data %>% select('epi_dates','week','year','hsp_abs','hsp_abs_flu','hsp_abs_rsv','hsp_abs_covid')

#Correct Date class
chi_data$epi_dates <- as.Date(chi_data$epi_dates)

#add column for flu season
chi_data <- chi_data %>% mutate(season =
                                     case_when(week >= 16 & week <= 45 & year == 2016 ~ '2016',
                                               week >= 16 & week <= 45 & year == 2017 ~ '2017',
                                               week >= 16 & week <= 45 & year == 2018  ~ '2018',
                                               week >= 16 & week <= 45 & year == 2019  ~ '2019',
                                               week >= 16 & week <= 45 & year == 2022 ~ '2022'
                                     )
)

#Drop season with rows = NA as excluded from analysis
chi_szn_data <- chi_data %>% drop_na(season)

#calculate cumulative sums
chi_szn_data <- chi_szn_data %>%
     group_by(season) %>%
     mutate(cum_flu = cumsum(hsp_abs_flu))

#Plot Chile Cumultative Graph with Legend
chi_flu_cum_leg <- ggplot() + geom_line(data=chi_szn_data,
                     aes(x=week,y=cum_flu,col=season),size=1.5) + 
     scale_color_manual(values = col_seasons_sh) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative\n Case Count') +
     ggtitle('CHI') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'right') +
     labs(col = 'Season')

## Get Legend and then remove from the graph ##
legend_sh <- get_legend(chi_flu_cum_leg)
chi_flu_cum <- chi_flu_cum_leg + theme(legend.position = 'none')

## Plot Flu Grid ##
sh_grid <- plot_grid(aus_flu_cum,chi_flu_cum, nrow=1,rel_widths = c(1,0.9))
nh_grid <- plot_grid(fra_flu_cum,uk_flu_cum,nrow=1,rel_widths = c(1,0.9))
flu_grid <- plot_grid(sh_grid,nh_grid,nrow=2,rel_heights = c(1,1))

##### RSV GRAPHS #####

## FRANCE ##
#calculate cumulative sums
fra_szn_data_rsv <- fra_szn_data %>%
     group_by(season) %>%
     mutate(cum_rsv = cumsum(hsp_abs_rsv))

#add seasonal week
fra_szn_data_rsv <- fra_szn_data_rsv %>% mutate(season_week = 
                                             case_when(week >= 40 ~ week-39,
                                                       week <= 20 ~ week+12))

#Plot graph
fra_rsv_cum <- ggplot() + geom_line(data=fra_szn_data_rsv,
                                    aes(x=season_week,y=cum_rsv,col=season),size=1.5) + 
     scale_color_manual(values = col_seasons_nh) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative\n Case Count') +
     ggtitle('FRA') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')

### UK GRAPH ###
#calculate cumulative sums
uk_szn_data_rsv <- uk_szn_data %>%
     group_by(season) %>%
     mutate(cum_rsv = cumsum(hsp_abs_rsv))

#add seasonal week
uk_szn_data_rsv <- uk_szn_data_rsv %>% mutate(season_week = 
                                           case_when(week >= 40 ~ week-39,
                                                     week <= 20 ~ week+12))

#Plot graph
uk_rsv_cum <- ggplot() + geom_line(data=uk_szn_data_rsv,
                                   aes(x=season_week,y=cum_rsv,col=season),size=1.5) + 
     scale_color_manual(values = col_seasons_nh) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative Case Count') +
     ggtitle('UK') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')

### AUS ###
#calculate cumulative sums
aus_szn_data_rsv <- aus_szn_data %>%
     group_by(season) %>%
     mutate(cum_rsv = cumsum(hsp_abs_rsv))

#Plot graph
aus_rsv_cum <- ggplot() + geom_line(data=aus_szn_data_rsv,
                                    aes(x=week,y=cum_rsv,col=season),size=1.5) + 
     scale_color_manual(values = col_seasons_sh) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab(paste('Cumulative\n Case Count')) +
     ggtitle('AUS') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')

### CHILE ###
#calculate cumulative sums
chi_szn_data_rsv <- chi_szn_data %>%
     group_by(season) %>%
     mutate(cum_rsv = cumsum(hsp_abs_rsv)) 

#Plot graph
chi_rsv_cum <- ggplot() + geom_line(data=chi_szn_data_rsv,
                                        aes(x=week,y=cum_rsv,col=season),size=1.5) + 
     scale_color_manual(values = col_seasons_sh) +
     theme_bw() +
     xlab('Seasonal Week No.') +
     ylab('Cumulative\n Case Count') +
     ggtitle('CHI') +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_blank()) +
     theme(axis.title.y = element_blank()) +
     theme(legend.position = 'none')

## Plot RSV Grid
sh_grid_rsv <- plot_grid(aus_rsv_cum,chi_rsv_cum, nrow=1,rel_widths = c(1,0.9))
nh_grid_rsv <- plot_grid(fra_rsv_cum,uk_rsv_cum,nrow=1,rel_widths = c(1,0.9))
rsv_grid <- plot_grid(sh_grid_rsv,nh_grid_rsv,nrow=2,rel_heights = c(1,1))

### Construct final figure using cowplot ###

### Create Title Frames ###
title_flu <- ggdraw() + 
     draw_label("Influenza",fontface = 'bold',x = 0.02,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))

title_rsv <- ggdraw() + 
     draw_label("RSV",fontface = 'bold',x = 0.02,hjust = 0)+theme(plot.margin = margin(0, 0, 0, 0))

x_lab_grid <- ggdraw() + draw_label('Seasonal Week Number') + theme(plot.margin = margin(0, 0, 0, 0))

title_grid <- plot_grid(title_flu, title_rsv)

## Legend Grid
legend_grid <- plot_grid(legend_sh,legend_nh,nrow=2)

## Virus Grid
virus_grid <- plot_grid(flu_grid,rsv_grid,legend_grid,nrow=1,rel_widths = c(1,1,0.2))
penultim_grid <- plot_grid(title_grid,virus_grid,nrow=2, rel_heights = c(0.1,1))

## Final Grid ##
final_grid <- plot_grid(penultim_grid,x_lab_grid,nrow=2,rel_heights = c(1,0.1))
final_grid

## Save figure ##
ggsave('output/Fig 02 - Hospitalization rates by season/cumul_figure.png',final_grid,width = 9,height=4.5,bg='white')

### CALCULATE MEDIAN VALUES BY SEASON - for Tables ###

#create maxima columns by country and virus
## Flu
fra_szn_data <- fra_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))
uk_szn_data <- uk_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))
aus_szn_data <- aus_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))
chi_szn_data <- chi_szn_data %>% group_by(season) %>% mutate(max=max(cum_flu))

## RSV
fra_szn_data_rsv <- fra_szn_data_rsv %>% group_by(season) %>% mutate(max=max(cum_rsv))
uk_szn_data_rsv <- uk_szn_data_rsv %>% group_by(season) %>% mutate(max=max(cum_rsv))
aus_szn_data_rsv <- aus_szn_data_rsv %>% group_by(season) %>% mutate(max=max(cum_rsv))
chi_szn_data_rsv <- chi_szn_data_rsv %>% group_by(season) %>% mutate(max=max(cum_rsv))

# AUS FLU
## Find median week for flu per season
med_rows <- aus_szn_data %>% 
     filter(cum_flu > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_flu))

## Get all data for median weeks per season
median_case_table <- merge(aus_szn_data,med_rows,
                           by.x = c('season','cum_flu'),
                           by.y = c('season','median_case_no'))

#Select relevant columns for table
median_case_table %>% select(1:4)

#AUS RSV
## Find median week for rsv per season
med_rows <- aus_szn_data_rsv %>% 
     filter(cum_rsv > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_rsv))

## Get all data for median weeks per season
median_case_table <- merge(aus_szn_data_rsv,med_rows,
                           by.x = c('season','cum_rsv'),
                           by.y = c('season','median_case_no'))

#Select relevant columns for table
median_case_table %>% select(1:4)

# CHI FLU
## Find median week for flu per season
med_rows <- chi_szn_data %>% 
     filter(cum_flu > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_flu))

## Get all data for median weeks per season
median_case_table <- merge(chi_szn_data,med_rows,
                           by.x = c('season','cum_flu'),
                           by.y = c('season','median_case_no'))

#Select relevant columns for table
median_case_table %>% select(1:4)

#CHI RSV
## Find median week for rsv per season
med_rows <- chi_szn_data_rsv %>% 
     filter(cum_rsv > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_rsv))

## Get all data for median weeks per season
median_case_table <- merge(chi_szn_data_rsv,med_rows,
                           by.x = c('season','cum_rsv'),
                           by.y = c('season','median_case_no'))

#Select relevant columns for table
median_case_table %>% select(1:4)

## NORTHERN HEMISPHERE ##
# FRA FLU
## Find median week for flu per season
med_rows <- fra_szn_data %>% 
     filter(cum_flu > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_flu))

## Get all data for median weeks per season
median_case_table <- merge(fra_szn_data,med_rows,
                           by.x = c('season','cum_flu'),
                           by.y = c('season','median_case_no'))

#Select relevant columns for table
median_case_table %>% select(1:4)

# FRA RSV
## Find median week for rsv per season
med_rows <- fra_szn_data_rsv %>% 
     filter(cum_rsv > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_rsv))

## Get all data for median weeks per season
median_case_table <- merge(fra_szn_data_rsv,med_rows,
                           by.x = c('season','cum_rsv'),
                           by.y = c('season','median_case_no'))

#Select relevant columns for table
median_case_table %>% select(1:4)

# UK FLU
## Find median week for flu per season
med_rows <- uk_szn_data %>% 
     filter(cum_flu > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_flu))

## Get all data for median weeks per season
median_case_table <- merge(uk_szn_data,med_rows,
                           by.x = c('season','cum_flu'),
                           by.y = c('season','median_case_no'))

#Select relevant columns for table
median_case_table %>% select(1:4)

# UK RSV
## Find median week for rsv per season
med_rows <- uk_szn_data_rsv %>% 
     filter(cum_rsv > (max/2)) %>% 
     group_by(season) %>% 
     summarise(median_case_no = min(cum_rsv))

## Get all data for median weeks per season
median_case_table <- merge(uk_szn_data_rsv,med_rows,
                           by.x = c('season','cum_rsv'),
                           by.y = c('season','median_case_no'))

#Select relevant columns for table
median_case_table %>% select(1:4)

                                                         