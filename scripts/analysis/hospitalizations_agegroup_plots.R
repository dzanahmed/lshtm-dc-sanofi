#------------------------------------------------------------#
# Analysis Graph #4                                          #
# Age breakdown for each virus (Influenza, RSV, COVID-19)    #                                                     
# Countries:                                                 #
#    Germany                                                 #
#    USA                                                     #
#------------------------------------------------------------#

#setwd("~/Desktop/LSHTM/Github/lshtm-dc-sanofi")

library(tidyverse)
library(patchwork)

# load data
data <- read.csv("data/merged_data/merged_data.csv")

#------------------------------------------------------------
# setting up data for analysis

# convert start_date to date
data$start_date <- as.Date(data$start_date)

#set up data sets by season (taking this bit of code from Jess)

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

#------------------------------------------------------------
# Setting up US data frames

# creating separate data frames for each season and virus
# flu
usa_flu_16 <- data_16 %>% 
     filter(country == "US" & is.na(hsp_rate_flu) == FALSE & (age_group=="0-4 yr" | age_group=="5-17 yr" | age_group=="18-49 yr" | age_group=="50-64 yr" | age_group=="65+ yr" | age_group=="Overall")) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_flu)

usa_flu_17 <- data_17 %>% 
     filter(country == "US" & is.na(hsp_rate_flu) == FALSE & (age_group=="0-4 yr" | age_group=="5-17 yr" | age_group=="18-49 yr" | age_group=="50-64 yr" | age_group=="65+ yr" | age_group=="Overall")) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_flu)

usa_flu_18 <- data_18 %>% 
     filter(country == "US" & is.na(hsp_rate_flu) == FALSE & (age_group=="0-4 yr" | age_group=="5-17 yr" | age_group=="18-49 yr" | age_group=="50-64 yr" | age_group=="65+ yr" | age_group=="Overall")) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_flu)

usa_flu_22 <- data_22 %>% 
     filter(country == "US" & is.na(hsp_rate_flu) == FALSE & (age_group=="0-4 yr" | age_group=="5-17 yr" | age_group=="18-49 yr" | age_group=="50-64 yr" | age_group=="65+ yr" | age_group=="Overall")) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_flu)

# rsv
# no under 18 data for 2016
# usa_rsv_16 <- data_16 %>% 
#      filter(country == "US" & is.na(hsp_rate_rsv) == FALSE & (age_group=="----0-<6 months" | age_group=="----6-<12 months" | age_group=="----1-<2 years" | age_group=="----2-4 years" | age_group=="5-17 years" | age_group=="18-49 years" | age_group=="50-64 years" | age_group=="65+ years" | age_group=="Overall")) %>% 
#      select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_rsv)

# no under 18 data for 2017
# usa_rsv_17 <- data_17 %>% 
#      filter(country == "US" & is.na(hsp_rate_rsv) == FALSE & (age_group=="----0-<6 months" | age_group=="----6-<12 months" | age_group=="----1-<2 years" | age_group=="----2-4 years" | age_group=="5-17 years" | age_group=="18-49 years" | age_group=="50-64 years" | age_group=="65+ years" | age_group=="Overall")) %>% 
#      select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_rsv)

usa_rsv_18 <- data_18 %>% 
     filter(country == "US" & is.na(hsp_rate_rsv) == FALSE & (age_group=="----0-<6 months" | age_group=="----6-<12 months" | age_group=="----1-<2 years" | age_group=="----2-4 years" | age_group=="5-17 years" | age_group=="18-49 years" | age_group=="50-64 years" | age_group=="65+ years" | age_group=="Overall")) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_rsv)

usa_rsv_22 <- data_22 %>% 
     filter(country == "US" & is.na(hsp_rate_rsv) == FALSE & (age_group=="----0-<6 months" | age_group=="----6-<12 months" | age_group=="----1-<2 years" | age_group=="----2-4 years" | age_group=="5-17 years" | age_group=="18-49 years" | age_group=="50-64 years" | age_group=="65+ years" | age_group=="Overall")) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_rsv)

# covid-19
usa_covid19_22 <- data_22 %>% 
               filter(country == "US" & is.na(hsp_rate_covid19) == FALSE & (age_group=="0-<6 months" | age_group=="6 months-4 yr" | age_group=="5-17 yr" | age_group=="18-49 yr" | age_group=="50-64 yr" | age_group=="65+ yr" | age_group=="Overall")) %>% 
               select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_covid19)

#------------------------------------------------------------
# USA Flu Plots

# flu plot (2016 - 17) 
date_breaks <- usa_flu_16$start_date
epi <- usa_flu_16$week

usa_flu_16_17_plot <- ggplot() + 
     geom_line(data = filter(usa_flu_16, age_group=="0-4 yr"), aes(x = start_date, y = hsp_rate_flu, color = '0-4 yr')) +
     geom_line(data = filter(usa_flu_16, age_group=="5-17 yr"), aes(x = start_date, y = hsp_rate_flu, color='5-17 yr')) +
     geom_line(data = filter(usa_flu_16, age_group=="18-49 yr"), aes(x = start_date, y = hsp_rate_flu, color='18-49 yr')) +
     geom_line(data = filter(usa_flu_16, age_group=="50-64 yr"), aes(x = start_date, y = hsp_rate_flu, color='50-64 yr')) +
     geom_line(data = filter(usa_flu_16, age_group=="65+ yr"), aes(x = start_date, y = hsp_rate_flu, color='65+ yr')) +
     geom_line(data = filter(usa_flu_16, age_group=="Overall"), aes(x = start_date, y = hsp_rate_flu, color='Total'), linetype="dashed") +
     scale_color_manual("", 
                         breaks = c('0-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                         values = c("0-4 yr"="red", "5-17 yr"="orange", '18-49 yr'='blue', '50-64 yr'='purple', '65+ yr'='green', 'Total'='black')) +
     xlab('2016-17 Epi Week') +
     ylab('Hospitalization Rate (per 100,000)') +
     ggtitle('US Influenza Hospitalization Rate, by Age Group') +
     theme_bw() +
     scale_x_date(breaks = date_breaks, labels = epi) +
     ylim(0, 50) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
     theme(legend.position='none')

# flu plot (2017 - 18) 
date_breaks <- usa_flu_17$start_date
epi <- usa_flu_17$week

usa_flu_17_18_plot <- ggplot() + 
     geom_line(data = filter(usa_flu_17, age_group=="0-4 yr"), aes(x = start_date, y = hsp_rate_flu, color = '0-4 yr')) +
     geom_line(data = filter(usa_flu_17, age_group=="5-17 yr"), aes(x = start_date, y = hsp_rate_flu, color='5-17 yr')) +
     geom_line(data = filter(usa_flu_17, age_group=="18-49 yr"), aes(x = start_date, y = hsp_rate_flu, color='18-49 yr')) +
     geom_line(data = filter(usa_flu_17, age_group=="50-64 yr"), aes(x = start_date, y = hsp_rate_flu, color='50-64 yr')) +
     geom_line(data = filter(usa_flu_17, age_group=="65+ yr"), aes(x = start_date, y = hsp_rate_flu, color='65+ yr')) +
     geom_line(data = filter(usa_flu_17, age_group=="Overall"), aes(x = start_date, y = hsp_rate_flu, color='Total'), linetype="dashed") +
     scale_color_manual("", 
                        breaks = c('0-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                        values = c("0-4 yr"="red", "5-17 yr"="orange", '18-49 yr'='blue', '50-64 yr'='purple', '65+ yr'='green', 'Total'='black')) +
     xlab('2017-18 Epi Week') +
     ylab("") +
     theme_bw() +
     scale_x_date(breaks = date_breaks, labels = epi) +
     ylim(0, 50) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.text.y=element_blank()) +
     theme(legend.position='none')

# flu plot (2018 - 19) 
date_breaks <- usa_flu_18$start_date
epi <- usa_flu_18$week

usa_flu_18_19_plot <- ggplot() + 
     geom_line(data = filter(usa_flu_18, age_group=="0-4 yr"), aes(x = start_date, y = hsp_rate_flu, color = '0-4 yr')) +
     geom_line(data = filter(usa_flu_18, age_group=="5-17 yr"), aes(x = start_date, y = hsp_rate_flu, color='5-17 yr')) +
     geom_line(data = filter(usa_flu_18, age_group=="18-49 yr"), aes(x = start_date, y = hsp_rate_flu, color='18-49 yr')) +
     geom_line(data = filter(usa_flu_18, age_group=="50-64 yr"), aes(x = start_date, y = hsp_rate_flu, color='50-64 yr')) +
     geom_line(data = filter(usa_flu_18, age_group=="65+ yr"), aes(x = start_date, y = hsp_rate_flu, color='65+ yr')) +
     geom_line(data = filter(usa_flu_18, age_group=="Overall"), aes(x = start_date, y = hsp_rate_flu, color='Total'), linetype="dashed") +
     scale_color_manual("", 
                        breaks = c('0-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                        values = c("0-4 yr"="red", "5-17 yr"="orange", '18-49 yr'='blue', '50-64 yr'='purple', '65+ yr'='green', 'Total'='black')) +
     xlab('2018-19 Epi Week') +
     ylab("") +
     theme_bw() +
     scale_x_date(breaks = date_breaks, labels = epi) +
     ylim(0, 50) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.text.y=element_blank()) +
     theme(legend.position='none')

# flu plot (2022 - 23) 
date_breaks <- usa_flu_22$start_date
epi <- usa_flu_22$week

usa_flu_22_23_plot <- ggplot() + 
     geom_line(data = filter(usa_flu_22, age_group=="0-4 yr"), aes(x = start_date, y = hsp_rate_flu, color = '0-4 yr')) +
     geom_line(data = filter(usa_flu_22, age_group=="5-17 yr"), aes(x = start_date, y = hsp_rate_flu, color='5-17 yr')) +
     geom_line(data = filter(usa_flu_22, age_group=="18-49 yr"), aes(x = start_date, y = hsp_rate_flu, color='18-49 yr')) +
     geom_line(data = filter(usa_flu_22, age_group=="50-64 yr"), aes(x = start_date, y = hsp_rate_flu, color='50-64 yr')) +
     geom_line(data = filter(usa_flu_22, age_group=="65+ yr"), aes(x = start_date, y = hsp_rate_flu, color='65+ yr')) +
     geom_line(data = filter(usa_flu_22, age_group=="Overall"), aes(x = start_date, y = hsp_rate_flu, color='Total'), linetype="dashed") +
     scale_color_manual("", 
                        breaks = c('0-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                        values = c("0-4 yr"="red", "5-17 yr"="orange", '18-49 yr'='blue', '50-64 yr'='purple', '65+ yr'='green', 'Total'='black')) +
     xlab('2022-23 Epi Week') +
     ylab("") +
     theme_bw() +
     scale_x_date(breaks = date_breaks, labels = epi) +
     ylim(0, 50) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.text.y=element_blank())

# arrange plots
usa_flu_plots <- usa_flu_16_17_plot + 
     usa_flu_17_18_plot + 
     usa_flu_18_19_plot + 
     usa_flu_22_23_plot +
     plot_layout(ncol = 4)

# save plot
#save_plot(file = 'output/usa_flu_hosp_agegroup.pdf', plot = usa_flu_plots, base_width=15, base_height=9)

#------------------------------------------------------------
# USA RSV Plots

# rsv plot (2018 - 19) 
date_breaks <- usa_rsv_18$start_date
epi <- usa_rsv_18$week

usa_rsv_18_19_plot <- ggplot() + 
     geom_line(data = filter(usa_rsv_18, age_group=="----0-<6 months"), aes(x = start_date, y = hsp_rate_rsv, color = '0-<6 mos')) +
     geom_line(data = filter(usa_rsv_18, age_group=="----6-<12 months"), aes(x = start_date, y = hsp_rate_rsv, color='6-<12 mos')) +
     geom_line(data = filter(usa_rsv_18, age_group=="----1-<2 years"), aes(x = start_date, y = hsp_rate_rsv, color='1-<2 yr')) +
     geom_line(data = filter(usa_rsv_18, age_group=="----2-4 years"), aes(x = start_date, y = hsp_rate_rsv, color='2-4 yr')) +
     geom_line(data = filter(usa_rsv_18, age_group=="5-17 years"), aes(x = start_date, y = hsp_rate_rsv, color='5-17 yr')) +
     #geom_line(data = filter(usa_rsv_18, age_group=="18-49 years"), aes(x = start_date, y = hsp_rate_rsv, color='18-49 yr')) +
     #geom_line(data = filter(usa_rsv_18, age_group=="50-64 years"), aes(x = start_date, y = hsp_rate_rsv, color='50-64 yr')) +
     #geom_line(data = filter(usa_rsv_18, age_group=="65+ years"), aes(x = start_date, y = hsp_rate_rsv, color='65+ yr')) +
     #geom_line(data = filter(usa_rsv_18, age_group=="Overall"), aes(x = start_date, y = hsp_rate_rsv, color='Total'), linetype="dashed") +
     # scale_color_manual("", 
     #                    breaks = c('0-<6 mos', '6-<12 mos', '1-<2 yr', '2-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
     #                    values = c("0-<6 mos"="red", '6-<12 mos'="pink", '1-<2 yr'="lightblue", '2-4 yr'="darkorange", "5-17 yr"="orange", '18-49 yr'='blue', '50-64 yr'='purple', '65+ yr'='green', 'Total'='black')) +
     scale_color_manual("", 
                        breaks = c('0-<6 mos', '6-<12 mos', '1-<2 yr', '2-4 yr','5-17 yr'),
                        values = c("0-<6 mos"="red", '6-<12 mos'="pink", '1-<2 yr'="lightblue", '2-4 yr'="darkorange", "5-17 yr"="orange")) +
     
     xlab('2018-19 Epi Week') +
     ylab('Hospitalization Rate (per 100,000)') +
     ylim(0, 300) +
     ggtitle('US RSV Hospitalization Rate, by Age Group') +
     theme_bw() +
     scale_x_date(breaks = date_breaks, labels = epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
     theme(legend.position='none')

# rsv plot (2022 - 23) 
date_breaks <- usa_rsv_22$start_date
epi <- usa_rsv_22$week

usa_rsv_22_23_plot <- ggplot() + 
     geom_line(data = filter(usa_rsv_22, age_group=="----0-<6 months"), aes(x = start_date, y = hsp_rate_rsv, color = '0-<6 mos')) +
     geom_line(data = filter(usa_rsv_22, age_group=="----6-<12 months"), aes(x = start_date, y = hsp_rate_rsv, color='6-<12 mos')) +
     geom_line(data = filter(usa_rsv_22, age_group=="----1-<2 years"), aes(x = start_date, y = hsp_rate_rsv, color='1-<2 yr')) +
     geom_line(data = filter(usa_rsv_22, age_group=="----2-4 years"), aes(x = start_date, y = hsp_rate_rsv, color='2-4 yr')) +
     geom_line(data = filter(usa_rsv_22, age_group=="5-17 years"), aes(x = start_date, y = hsp_rate_rsv, color='5-17 yr')) +
#commenting out these higher age groups as there's not much to see here
     #geom_line(data = filter(usa_rsv_22, age_group=="18-49 years"), aes(x = start_date, y = hsp_rate_rsv, color='18-49 yr')) +
     #geom_line(data = filter(usa_rsv_22, age_group=="50-64 years"), aes(x = start_date, y = hsp_rate_rsv, color='50-64 yr')) +
     #geom_line(data = filter(usa_rsv_22, age_group=="65+ years"), aes(x = start_date, y = hsp_rate_rsv, color='65+ yr')) +
     #geom_line(data = filter(usa_rsv_22, age_group=="Overall"), aes(x = start_date, y = hsp_rate_rsv, color='Total'), linetype="dashed") +
     # scale_color_manual("", 
     #                    breaks = c('0-<6 mos', '6-<12 mos', '1-<2 yr', '2-4 yr','5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
     #                    values = c("0-<6 mos"="red", '6-<12 mos'="pink", '1-<2 yr'="lightblue", '2-4 yr'="darkorange", "5-17 yr"="orange", '18-49 yr'='blue', '50-64 yr'='purple', '65+ yr'='green', 'Total'='black')) +
     scale_color_manual("", 
                        breaks = c('0-<6 mos', '6-<12 mos', '1-<2 yr', '2-4 yr','5-17 yr'),
                        values = c("0-<6 mos"="red", '6-<12 mos'="pink", '1-<2 yr'="lightblue", '2-4 yr'="darkorange", "5-17 yr"="orange")) +
     
     xlab('2022-23 Epi Week') +
     ylab('') +
     ylim(0, 300) +
     theme_bw() +
     scale_x_date(breaks = date_breaks, labels = epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.text.y=element_blank()) 

# arrange plots
usa_rsv_plots <- usa_rsv_18_19_plot + usa_rsv_22_23_plot 

# save plot
#save_plot(file = 'output/usa_rsv_hosp_agegroup.pdf', plot = usa_rsv_plots, base_width=15, base_height=9)

#------------------------------------------------------------
# USA COVID-19 Plots
date_breaks <- usa_covid19_22$start_date
epi <- usa_covid19_22$week

usa_covid19_22_23_plot <- ggplot() + 
     geom_line(data = filter(usa_covid19_22, age_group=="0-<6 months"), aes(x = start_date, y = hsp_rate_covid19, color = '0-6 mos')) +
     geom_line(data = filter(usa_covid19_22, age_group=="6 months-4 yr"), aes(x = start_date, y = hsp_rate_covid19, color='6 mos-4 yr')) +
     geom_line(data = filter(usa_covid19_22, age_group=="5-17 yr"), aes(x = start_date, y = hsp_rate_covid19, color='5-17 yr')) +
     geom_line(data = filter(usa_covid19_22, age_group=="18-49 yr"), aes(x = start_date, y = hsp_rate_covid19, color='18-49 yr')) +
     geom_line(data = filter(usa_covid19_22, age_group=="50-64 yr"), aes(x = start_date, y = hsp_rate_covid19, color='50-64 yr')) +
     geom_line(data = filter(usa_covid19_22, age_group=="65+ yr"), aes(x = start_date, y = hsp_rate_covid19, color='65+ yr')) +
     geom_line(data = filter(usa_covid19_22, age_group=="Overall"), aes(x = start_date, y = hsp_rate_covid19, color='Total'), linetype="dashed") +
     scale_color_manual("",
                        breaks = c('0-6 mos', '6 mos-4 yr', '5-17 yr', '18-49 yr', '50-64 yr', '65+ yr', 'Total'),
                        values = c("0-6 mos"="red", '6 mos-4 yr'="pink", "5-17 yr"="orange", '18-49 yr'='blue', '50-64 yr'='purple', '65+ yr'='green', 'Total'='black')) +
     xlab('2022-23 Epi Week') +
     ylab('Hospitalization Rate (per 100,000)') +
     ggtitle("US COVID-19 Hospitalization Rate, by Age Group")
     theme_bw() +
     scale_x_date(breaks = date_breaks, labels = epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

usa_covid19_22_23_plot

#save_plot(file = 'output/usa_covid19_hosp_agegroup.pdf', plot = usa_covid19_22_23_plot, base_width=15, base_height=9)

#------------------------------------------------------------
# Setting up Germany data frames

# creating separate data frames for each season and virus
# flu
ger_flu_16 <- data_16 %>% 
     filter(country == "DE" & is.na(hsp_rate_flu) == FALSE) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_flu)

ger_flu_17 <- data_17 %>% 
     filter(country == "DE" & is.na(hsp_rate_flu) == FALSE) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_flu)

ger_flu_18 <- data_18 %>% 
     filter(country == "DE" & is.na(hsp_rate_flu) == FALSE) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_flu)

ger_flu_22 <- data_22 %>% 
     filter(country == "DE" & is.na(hsp_rate_flu) == FALSE) %>% 
     select(id, data_source, country, hemisphere, start_date, week, year, age_group, hsp_rate_flu)

#------------------------------------------------------------
# Germany Flu Plots

# flu plot (2016 - 17) 
date_breaks <- ger_flu_16$start_date
epi <- ger_flu_16$week

ger_flu_16_17_plot <- ggplot() + 
     geom_line(data = filter(ger_flu_16, agg_age_group=='0-4'), aes(x = start_date, y = hsp_rate_flu, color = '0-4 yr')) +
     geom_line(data = filter(ger_flu_16, agg_age_group=='5-19'), aes(x = start_date, y = hsp_rate_flu, color='5-19 yr')) +
     geom_line(data = filter(ger_flu_16, agg_age_group=='20-49'), aes(x = start_date, y = hsp_rate_flu, color='20-49 yr')) +
     geom_line(data = filter(ger_flu_16, agg_age_group=='50-64'), aes(x = start_date, y = hsp_rate_flu, color='50-64 yr')) +
     geom_line(data = filter(ger_flu_16, agg_age_group=='65+'), aes(x = start_date, y = hsp_rate_flu, color='65+ yr')) +
     geom_line(data = filter(ger_flu_16, agg_age_group=='Total'), aes(x = start_date, y = hsp_rate_flu, color='Total'), linetype="dashed") +
     scale_color_manual("", 
                        breaks = c('0-4 yr','5-19 yr', '20-49 yr', '50-64 yr', '65+ yr', 'Total'),
                        values = c("0-4 yr"="red", "5-19 yr"="orange", '20-49 yr'='blue', '50-64 yr'='purple', '65+ yr'='green', 'Total'='black')) +
     xlab('2016-17 Epi Week') +
     ylab('Hospitalization Rate (per 100,000)') +
     ggtitle('Germany Influenza Hospitalization Rate, by Age Group') +
     theme_bw() +
     scale_x_date(breaks = date_breaks, labels = epi) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
     theme(legend.position='none')
