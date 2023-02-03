#------------------------------------------------------------#
# Analysis Graph #4                                          #
# Age breakdown for each virus (Influenza, RSV, COVID-19)    #                                                     
# Countries:                                                 #                                                 
#    USA                                                     #
#------------------------------------------------------------#

#setwd("~/Desktop/LSHTM/Github/lshtm-dc-sanofi")

library(tidyverse)
library(patchwork)

# load data
data <- read.csv("data/merged_data/merged_data.csv")

# load epiweeks data
#epiweeks <- read.csv("data/epi_weeks.csv")

#------------------------------------------------------------
# setting up data for analysis

# convert start_date to date
data$epi_dates <- as.Date(data$epi_dates)

# merge in epiweeks data for all years
#data <- merge(data, epiweeks, by.x = c("year", "week", "start_date"), by.y = c("year", "epi_wk_no", "epi_dates"), all.y = T)

#set up data sets by season (taking this bit of code from Jess)

# 2016 - start wk - 2016-10-02 --> 2017-05-14
data_16 <- data %>% filter(epi_dates >= '2016-10-02')
data_16 <- data_16 %>% filter(epi_dates <= '2017-05-14')

# 2017 - start wk - 2017-10-01 --> 2018-05-13
data_17 <- data %>% filter(epi_dates >= '2017-10-01')
data_17 <- data_17 %>% filter(epi_dates <= '2018-05-13')

# 2018 - start wk - 2018-09-30 --> 2019-05-12
data_18 <- data %>% filter(epi_dates >= '2018-09-30')
data_18 <- data_18 %>% filter(epi_dates <= '2019-05-12')

# 2019 - start wk - 2019-09-29 --> 2020-05-11
data_19 <- data %>% filter(epi_dates >= '2019-09-29')
data_19 <- data_19 %>% filter(epi_dates <= '2020-05-11')

# 2022 - start wk - 2022-10-02 --> 2023-01-01
data_22 <- data %>% filter(epi_dates >= '2022-10-02')
data_22 <- data_22 %>% filter(epi_dates <= '2023-01-01')

#------------------------------------------------------------
# Setting up US data frames

# creating separate data frames for each season and virus
# flu
usa_flu_16 <- data_16 %>% 
     filter(country == "US" & is.na(hsp_rate_flu) == FALSE & (age_group=="0-4" | age_group=="5-17" | age_group=="18-49" | age_group=="50-64" | age_group=="65+" | age_group=="ALL")) %>% 
     select(id, data_source, country, hemisphere, epi_dates, week, year, age_group, hsp_rate_flu)

usa_flu_17 <- data_17 %>% 
     filter(country == "US" & is.na(hsp_rate_flu) == FALSE & (age_group=="0-4" | age_group=="5-17" | age_group=="18-49" | age_group=="50-64" | age_group=="65+" | age_group=="ALL")) %>% 
     select(id, data_source, country, hemisphere, epi_dates, week, year, age_group, hsp_rate_flu)

usa_flu_18 <- data_18 %>% 
     filter(country == "US" & is.na(hsp_rate_flu) == FALSE & (age_group=="0-4" | age_group=="5-17" | age_group=="18-49" | age_group=="50-64" | age_group=="65+" | age_group=="ALL")) %>% 
     select(id, data_source, country, hemisphere, epi_dates, week, year, age_group, hsp_rate_flu)

usa_flu_22 <- data_22 %>% 
     filter(country == "US" & is.na(hsp_rate_flu) == FALSE & (age_group=="0-4" | age_group=="5-17" | age_group=="18-49" | age_group=="50-64" | age_group=="65+" | age_group=="ALL")) %>% 
     select(id, data_source, country, hemisphere, epi_dates, week, year, age_group, hsp_rate_flu)

# rsv
# no under 18 data for 2016
# usa_rsv_16 <- data_16 %>% 
#      filter(country == "US" & is.na(hsp_rate_rsv) == FALSE & (age_group=="0-0.5" | age_group=="0.5-1" | age_group=="1-2" | age_group=="2-4" | age_group=="5-17" | age_group=="18-49" | age_group=="50-64" | age_group=="65+" | age_group=="ALL")) %>% 
#      select(id, data_source, country, hemisphere, epi_dates, week, year, age_group, hsp_rate_rsv)

# no under 18 data for 2017
# usa_rsv_17 <- data_17 %>% 
#      filter(country == "US" & is.na(hsp_rate_rsv) == FALSE & (age_group=="0-0.5" | age_group=="0.5-1" | age_group=="1-2" | age_group=="2-4" | age_group=="5-17" | age_group=="18-49" | age_group=="50-64" | age_group=="65+" | age_group=="ALL")) %>% 
#      select(id, data_source, country, hemisphere, epi_dates, week, year, age_group, hsp_rate_rsv)

usa_rsv_18 <- data_18 %>% 
     filter(country == "US" & is.na(hsp_rate_rsv) == FALSE & (age_group=="0-0.5" | age_group=="0.5-1" | age_group=="1-2" | age_group=="2-4" | age_group=="5-17" | age_group=="18-49" | age_group=="50-64" | age_group=="65+" | age_group=="ALL")) %>% 
     select(id, data_source, country, hemisphere, epi_dates, week, year, age_group, hsp_rate_rsv)

usa_rsv_22 <- data_22 %>% 
     filter(country == "US" & is.na(hsp_rate_rsv) == FALSE & (age_group=="0-0.5" | age_group=="0.5-1" | age_group=="1-2" | age_group=="2-4" | age_group=="5-17" | age_group=="18-49" | age_group=="50-64" | age_group=="65+" | age_group=="ALL")) %>% 
     select(id, data_source, country, hemisphere, epi_dates, week, year, age_group, hsp_rate_rsv)

# covid-19
usa_covid19_22 <- data_22 %>% 
               filter(country == "US" & is.na(hsp_rate_covid19) == FALSE & (age_group=="0-0.5" | age_group=="0.5-4" | age_group=="5-17" | age_group=="18-49" | age_group=="50-64" | age_group=="65+" | age_group=="ALL")) %>% 
               select(id, data_source, country, hemisphere, epi_dates, week, year, age_group, hsp_rate_covid19)

#------------------------------------------------------------
# USA Flu Plots

# flu plot (2016 - 17) 

usa_flu_16_17_plot <- ggplot() + 
     geom_line(data = filter(usa_flu_16, age_group=="0-4"), aes(x = epi_dates, y = hsp_rate_flu, color = '0-4 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_16, age_group=="5-17"), aes(x = epi_dates, y = hsp_rate_flu, color='5-17 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_16, age_group=="18-49"), aes(x = epi_dates, y = hsp_rate_flu, color='18-49 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_16, age_group=="50-64"), aes(x = epi_dates, y = hsp_rate_flu, color='50-64 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_16, age_group=="65+"), aes(x = epi_dates, y = hsp_rate_flu, color='65+ yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_16, age_group=="ALL"), aes(x = epi_dates, y = hsp_rate_flu, color='Total'), linetype="dashed", size = 1.25) +
     scale_color_manual("", 
                         breaks = c('0-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                         values = c("0-4 yr"="deepskyblue2", "5-17 yr"="orange", '18-49 yr'='forestgreen', '50-64 yr'='purple', '65+ yr'='indianred', 'Total'='black')) +
     xlab('Epi Week') +
     ylab('Hospitalisations per 100,000 persons') +
     ggtitle(label = 'A. Influenza', subtitle = '2016-17') +
     theme_bw() +
     scale_x_date(
          date_breaks = "2 weeks",
          date_labels = '%U',
          limits = c(as.Date(min(usa_flu_16$epi_dates) - 2), as.Date(max(usa_flu_16$epi_dates)))
     ) +
     ylim(0, 50) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
     theme(legend.position='none')

# flu plot (2017 - 18) 

usa_flu_17_18_plot <- ggplot() + 
     geom_line(data = filter(usa_flu_17, age_group=="0-4"), aes(x = epi_dates, y = hsp_rate_flu, color = '0-4 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_17, age_group=="5-17"), aes(x = epi_dates, y = hsp_rate_flu, color='5-17 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_17, age_group=="18-49"), aes(x = epi_dates, y = hsp_rate_flu, color='18-49 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_17, age_group=="50-64"), aes(x = epi_dates, y = hsp_rate_flu, color='50-64 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_17, age_group=="65+"), aes(x = epi_dates, y = hsp_rate_flu, color='65+ yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_17, age_group=="ALL"), aes(x = epi_dates, y = hsp_rate_flu, color='Total'), linetype="dashed", size = 1.25) +
     scale_color_manual("", 
                        breaks = c('0-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                        values = c("0-4 yr"="deepskyblue2", "5-17 yr"="orange", '18-49 yr'='forestgreen', '50-64 yr'='purple', '65+ yr'='indianred', 'Total'='black')) +
     xlab('Epi Week') +
     ylab("") +
     theme_bw() +
     ggtitle(label = '', subtitle = '2017-18') + 
     scale_x_date(
          date_breaks = "2 weeks",
          date_labels = '%U',
          limits = c(as.Date(min(usa_flu_17$epi_dates) - 2), as.Date(max(usa_flu_17$epi_dates)))
     ) +
     ylim(0, 50) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.text.y=element_blank()) +
     theme(legend.position='none')

# flu plot (2018 - 19) 

usa_flu_18_19_plot <- ggplot() + 
     geom_line(data = filter(usa_flu_18, age_group=="0-4"), aes(x = epi_dates, y = hsp_rate_flu, color = '0-4 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_18, age_group=="5-17"), aes(x = epi_dates, y = hsp_rate_flu, color='5-17 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_18, age_group=="18-49"), aes(x = epi_dates, y = hsp_rate_flu, color='18-49 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_18, age_group=="50-64"), aes(x = epi_dates, y = hsp_rate_flu, color='50-64 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_18, age_group=="65+"), aes(x = epi_dates, y = hsp_rate_flu, color='65+ yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_18, age_group=="ALL"), aes(x = epi_dates, y = hsp_rate_flu, color='Total'), linetype="dashed", size = 1.25) +
     scale_color_manual("", 
                        breaks = c('0-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                        values = c("0-4 yr"="deepskyblue2", "5-17 yr"="orange", '18-49 yr'='forestgreen', '50-64 yr'='purple', '65+ yr'='indianred', 'Total'='black')) +
     xlab('Epi Week') +
     ylab('') +
     ggtitle(label = '', subtitle = '2018-19') +
     theme_bw() +
     scale_x_date(
          date_breaks = "2 weeks",
          date_labels = '%U',
          limits = c(as.Date(min(usa_flu_18$epi_dates) + 6), as.Date(max(usa_flu_18$epi_dates) + 5))
     ) +
     ylim(0, 50) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.text.y=element_blank()) +
     theme(legend.position='none')

# flu plot (2022 - 23) 

usa_flu_22_23_plot <- ggplot() + 
     geom_line(data = filter(usa_flu_22, age_group=="0-4"), aes(x = epi_dates, y = hsp_rate_flu, color = '0-4 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_22, age_group=="5-17"), aes(x = epi_dates, y = hsp_rate_flu, color='5-17 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_22, age_group=="18-49"), aes(x = epi_dates, y = hsp_rate_flu, color='18-49 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_22, age_group=="50-64"), aes(x = epi_dates, y = hsp_rate_flu, color='50-64 yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_22, age_group=="65+"), aes(x = epi_dates, y = hsp_rate_flu, color='65+ yr'), size = 1.25) +
     geom_line(data = filter(usa_flu_22, age_group=="ALL"), aes(x = epi_dates, y = hsp_rate_flu, color='Total'), linetype="dashed", size = 1.25) +
     scale_color_manual("", 
                        breaks = c('0-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                        values = c("0-4 yr"="deepskyblue2", "5-17 yr"="orange", '18-49 yr'='forestgreen', '50-64 yr'='purple', '65+ yr'='indianred', 'Total'='black')) +
     xlab('Epi Week') +
     ylab("") +
     ggtitle(label = '', subtitle = '2022-23') +
     theme_bw() +
     scale_x_date(
          date_breaks = "2 weeks",
          date_labels = '%U',
          limits = c(as.Date(min(usa_flu_22$epi_dates) - 2), as.Date("2023-04-23"))
     ) +
     ylim(0, 50) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.text.y=element_blank())

# arrange plots
usa_flu_plots <- usa_flu_16_17_plot + 
     usa_flu_17_18_plot + 
     usa_flu_18_19_plot + 
     usa_flu_22_23_plot +
     plot_layout(ncol = 4)

# save plot
ggsave(filename = 'output/Fig 04 - Hospitalization by age group/usa_flu_hosp_agegroup.pdf', plot = usa_flu_plots, width=15, height=9)

#------------------------------------------------------------
# USA RSV Plots

# rsv plot (2018 - 19) 

usa_rsv_18_19_plot <- ggplot() + 
     geom_line(data = filter(usa_rsv_18, age_group=="0-0.5"), aes(x = epi_dates, y = hsp_rate_rsv, color = '0-<6 mos'), size = 1.25) +
     geom_line(data = filter(usa_rsv_18, age_group=="0.5-1"), aes(x = epi_dates, y = hsp_rate_rsv, color='6-<12 mos'), size = 1.25) +
     geom_line(data = filter(usa_rsv_18, age_group=="1-2"), aes(x = epi_dates, y = hsp_rate_rsv, color='1-<2 yr'), size = 1.25) +
     geom_line(data = filter(usa_rsv_18, age_group=="2-4"), aes(x = epi_dates, y = hsp_rate_rsv, color='2-4 yr'), size = 1.25) +
     geom_line(data = filter(usa_rsv_18, age_group=="5-17"), aes(x = epi_dates, y = hsp_rate_rsv, color='5-17 yr'), size = 1.25) +
     geom_line(data = filter(usa_rsv_18, age_group=="ALL"), aes(x = epi_dates, y = hsp_rate_rsv, color='Total'), linetype="dashed", size = 1.25) +
     scale_color_manual("", 
                        breaks = c('0-<6 mos', '6-<12 mos', '1-<2 yr', '2-4 yr','5-17 yr', 'Total'),
                        values = c("0-<6 mos"="cornflowerblue", '6-<12 mos'="cyan3", '1-<2 yr'="cadetblue2", '2-4 yr'="dodgerblue3", "5-17 yr"="orange", 'Total'='black')) +
     
     xlab('Epi Week') +
     ylab('Hospitalisations per 100,000 persons') +
     ylim(0, 300) +
     ggtitle(label = 'B. RSV', subtitle = '2018-19') +
     theme_bw() +
     scale_x_date(
          date_breaks = "2 weeks",
          date_labels = '%U',
          limits = c(as.Date(min(usa_rsv_18$epi_dates) + 5), as.Date(max(usa_rsv_18$epi_dates)))
     ) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
     theme(legend.position='none')

# rsv plot (2022 - 23) 

usa_rsv_22_23_plot <- ggplot() + 
     geom_line(data = filter(usa_rsv_22, age_group=="0-0.5"), aes(x = epi_dates, y = hsp_rate_rsv, color = '0-<6 mos'), size = 1.25) +
     geom_line(data = filter(usa_rsv_22, age_group=="0.5-1"), aes(x = epi_dates, y = hsp_rate_rsv, color='6-<12 mos'), size = 1.25) +
     geom_line(data = filter(usa_rsv_22, age_group=="1-2"), aes(x = epi_dates, y = hsp_rate_rsv, color='1-<2 yr'), size = 1.25) +
     geom_line(data = filter(usa_rsv_22, age_group=="2-4"), aes(x = epi_dates, y = hsp_rate_rsv, color='2-4 yr'), size = 1.25) +
     geom_line(data = filter(usa_rsv_22, age_group=="5-17"), aes(x = epi_dates, y = hsp_rate_rsv, color='5-17 yr'), size = 1.25) +
     geom_line(data = filter(usa_rsv_22, age_group=="ALL"), aes(x = epi_dates, y = hsp_rate_rsv, color='Total'), linetype="dashed", size = 1.25) +
     scale_color_manual("", 
                        breaks = c('0-<6 mos', '6-<12 mos', '1-<2 yr', '2-4 yr','5-17 yr', 'Total'),
                        values = c("0-<6 mos"="cornflowerblue", '6-<12 mos'="cyan3", '1-<2 yr'="cadetblue2", '2-4 yr'="dodgerblue3", "5-17 yr"="orange", 'Total'='black')) +
     
     xlab('Epi Week') +
     ylab('') +
     ggtitle(label = '', subtitle = '2022-23') + 
     ylim(0, 300) +
     theme_bw() +
     scale_x_date(
          date_breaks = "2 weeks",
          date_labels = '%U',
          limits = c(as.Date(min(usa_covid19_22$epi_dates) - 2), as.Date("2023-04-23"))
     ) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.text.y=element_blank()) 

# arrange plots
usa_rsv_plots <- usa_rsv_18_19_plot + usa_rsv_22_23_plot 

# save plot
ggsave(filename = 'output/Fig 04 - Hospitalization by age group/usa_rsv_hosp_agegroup.pdf', plot = usa_rsv_plots, width=15, height=9)

#------------------------------------------------------------
# USA COVID-19 Plots

usa_covid19_22_23_plot <- ggplot() + 
     geom_line(data = filter(usa_covid19_22, age_group=="0-0.5"), aes(x = epi_dates, y = hsp_rate_covid19, color = '0-6 mos'), size = 1.25) +
     geom_line(data = filter(usa_covid19_22, age_group=="0.5-4"), aes(x = epi_dates, y = hsp_rate_covid19, color='6 mos-4 yr'), size = 1.25) +
     geom_line(data = filter(usa_covid19_22, age_group=="5-17"), aes(x = epi_dates, y = hsp_rate_covid19, color='5-17 yr'), size = 1.25) +
     geom_line(data = filter(usa_covid19_22, age_group=="18-49"), aes(x = epi_dates, y = hsp_rate_covid19, color='18-49 yr'), size = 1.25) +
     geom_line(data = filter(usa_covid19_22, age_group=="50-64"), aes(x = epi_dates, y = hsp_rate_covid19, color='50-64 yr'), size = 1.25) +
     geom_line(data = filter(usa_covid19_22, age_group=="65+"), aes(x = epi_dates, y = hsp_rate_covid19, color='65+ yr'), size = 1.25) +
     geom_line(data = filter(usa_covid19_22, age_group=="ALL"), aes(x = epi_dates, y = hsp_rate_covid19, color='Total'), linetype="dashed", size = 1.25) +
     scale_color_manual("",
                        breaks = c('0-6 mos', '6 mos-4 yr', '5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                        values = c("0-6 mos"="cornflowerblue", '6 mos-4 yr'="darkslategray3", "5-17 yr"="orange", '18-49 yr'='forestgreen', '50-64 yr'='purple', '65+ yr'='indianred', 'Total'='black')) +
     xlab('Epi Week') +
     ylab('Hospitalisations per 100,000 persons') +
     ggtitle(label = "C. SARS-CoV-2", subtitle = '2022-23') +
     theme_bw() +
     scale_x_date(
          date_breaks = "2 weeks",
          date_labels = '%U',
          limits = c(as.Date(min(usa_covid19_22$epi_dates) - 2), as.Date("2023-04-23"))
     ) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

usa_covid19_22_23_plot

ggsave(filename = 'output/Fig 04 - Hospitalization by age group/usa_covid19_hosp_agegroup.pdf', plot = usa_covid19_22_23_plot, width=15, height=9)


# arrange all plots together
all_plots <- (usa_flu_plots) / ((usa_rsv_18_19_plot | usa_rsv_22_23_plot) | usa_covid19_22_23_plot)

ggsave(filename = 'output/Fig 04 - Hospitalization by age group/hosp_byage_plots.pdf', plot = all_plots, width=15, height=9)

## end of file ##