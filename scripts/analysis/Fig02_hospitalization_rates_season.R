library(tidyverse)
library(ggplot2)
library(readr)
library(ftplottools) #themes
library(cowplot)
library(grid)
library(ggpubr)

# Data
all_countries <- read_csv("data/merged_data/merged_data.csv")


####################################################################
##### ABSOLUTES NUMBER FOR THOSE COUNTRIES WITH ONLY RATES #########
####################################################################
# Code won't use it as the y-axis will be rates
# Germany
# germany_pop <- data.frame(year = c(2016:2019, 2022, 2022),
#                           pop = c(82500000, 82700000, 82900000, 83100000, 84300000, 84300000))
# # USA
# usa_pop <- data.frame(year = c(2016:2019, 2022, 2023),
#                       pop = c(323100000, 325100000, 326800000, 328300000, 333300000, 333300000))
# 
# # Absolute flu number Germany
# for(i in c(germany_pop$year)) {
#      all_countries$hsp_abs_flu <- ifelse(all_countries$country == "DE" & all_countries$year == i, round((all_countries$hsp_rate_flu* germany_pop$pop[germany_pop$year == i])/100000,0),all_countries$hsp_abs_flu)
# }
# 
# # Absolute flu number USA
# for(i in c(usa_pop$year)) {
#      all_countries$hsp_abs_flu <- ifelse(all_countries$country == "US" & all_countries$year == i, round((all_countries$hsp_rate_flu* usa_pop$pop[usa_pop$year == i])/100000,0),all_countries$hsp_abs_flu)
# }


##########################################################
########### NEW VARABLE: SEASON #########################
#########################################################


seasons <- data.frame(start = c(2015:2022),
                      end = c(2016:2023),
                      seas = c("2015-16","2016-17","2017-18", "2018-19","2019-20","2020-21","2021-22","2022-23"))


all_countries$season <- NA # Create the variable

## For loop to iterate the all_countries dataset

for(i in c(seasons$start)) {
     year_start <- i
     year_end <- (seasons$end[seasons$start== i])
     p_season <- (seasons$seas[seasons$start== i])
     
     # Differentiate NH VS SH
     all_countries$season <-
     ifelse((all_countries$hemisphere == "NH" & all_countries$year == year_start & all_countries$week %in% c(40:52)) | 
                (all_countries$hemisphere == "NH" & all_countries$year == year_end & all_countries$week %in% c(1:39)), p_season,
            ifelse(
                   (all_countries$hemisphere == "SH"& all_countries$year == year_start & all_countries$week   %in% c(14:52)) |
                    (all_countries$hemisphere == "SH"& all_countries$year == year_end & all_countries$week %in% c(1:13)),p_season,all_countries$season))
            
 }

# Check the season created

countries <- data.frame(country = c("UK", "US", "DE", "FR", "AUS", "CL"),
                        hemisphere = c(rep("NH",4), "SH","SH"))
checking<- list()

for(c in countries$country){
     data_f   <- all_countries %>% 
          select(country, year, age_group, week, season, hsp_rate_flu, denominator,hsp_abs_flu) %>% 
          filter(country == c, age_group == 'ALL' )
     checking[[c]] <- data_f

}

# To see in the environmnet
x <- checking$UK

############################################
###### Standardized data DE and USA #######
############################################
# No used it as well.
# Table of NH countries

# nh_countries <- countries[countries$hemisphere == "NH",]
# 
# data_all_ages <- filter(all_countries,country =="V") # to create the data set
#      
# for(i in nh_countries$country){
#      for(l in seasons$seas){
#           db_norm <- all_countries  %>% 
#                filter(country == i, season == l, age_group == "ALL") %>% 
#                mutate(min = min(hsp_abs_flu, na.rm = T),
#                       max = max(hsp_abs_flu, na.rm = T)) %>% 
#                mutate(norm_flu = round((hsp_abs_flu - min)/(max-min),3))
#                
#           data_all_ages <- rbind(data_all_ages,db_norm)
#      }
# }

######################################################
######## RATE USING 3% OF THE POPULATION ############
#############    AUS - FR - CL      #################

# Subset varibales
subset_countries <- all_countries %>% 
     select(country, data_source, hemisphere, week, year, age_group, denominator, hsp_rate_flu, hsp_abs_flu,hsp_rate_rsv, hsp_abs_rsv, hsp_rate_covid19, hsp_abs_covid,start_date, season) %>% 
     filter(age_group == "ALL",data_source !='FluNet - Non Sentinel' )

# New flu considering 3% population
subset_countries$flu_rate <- ifelse(subset_countries$country == "CL",
                                    round((subset_countries$hsp_abs_flu/(subset_countries$denominator*0.03))*100000,2),
                                    ifelse(subset_countries$country == "AUS",
                                           round((subset_countries$hsp_abs_flu/(subset_countries$denominator*0.03))*100000,2),
                                           ifelse(subset_countries$country == "FR",
                                                  round((subset_countries$hsp_abs_flu/(subset_countries$denominator*0.03))*100000,2), subset_countries$hsp_rate_flu)))

# New RSV considering 3% population
subset_countries$rsv_rate <- ifelse(subset_countries$country == "CL",
                                    round((subset_countries$hsp_abs_rsv/(subset_countries$denominator*0.03))*100000,2),
                                    ifelse(subset_countries$country == "AUS",
                                           round((subset_countries$hsp_abs_rsv/(subset_countries$denominator*0.03))*100000,2),
                                           ifelse(subset_countries$country == "FR",

                                                  
                                                                                                   round((subset_countries$hsp_abs_rsv/(subset_countries$denominator*0.03))*100000,2), subset_countries$hsp_rate_rsv)))
# New COVID-19 considering 3% population
subset_countries$covid_rate <- ifelse(subset_countries$country == "CL",
                                    round((subset_countries$hsp_abs_covid/(subset_countries$denominator*0.03))*100000,2),
                                    ifelse(subset_countries$country == "AUS",
                                           round((subset_countries$hsp_abs_covid/(subset_countries$denominator*0.03))*100000,2),
                                           ifelse(subset_countries$country == "FR",
                                                  round((subset_countries$hsp_abs_covid/(subset_countries$denominator*0.03))*100000,2), subset_countries$hsp_rate_covid19)))

# Re-select 

subset_countries_final <- subset_countries %>% 
     select(country, data_source, hemisphere,start_date, year,week, year, season, flu_rate, rsv_rate, covid_rate)

#### ################################################## ####
#### ########## GRAPH NORTHERN HEMISPHER ############## ####
#### ################################################## ####

data_graph <-  subset_countries_final %>%
     filter(season != "2015-16", hemisphere == "NH", season != "2021-22") %>% 
     #week = factor(week,levels = c(40:52,1:39) original
     mutate(week = factor(week,levels = c(31:52,1:30)),
            season = as.factor(season),
            breaks_x = as.numeric(week),
            label_x = week) 
axis_nh <- length(unique(data_graph$season))-1

## To create a thicker line
# data_graph_last_season <- data_graph %>% 
#      filter(season == "2022-23")


####  ##############################################
####  ########### INFLUENZA PLOTS NH ###############
####  ##############################################
# to make the rectangle 
db_rect <- data_graph %>% 
     group_by(country, week) %>% 
     filter(week == 40, year == 2017, country == "UK" | country =="DE"|country == "US"|country == "FR")

# Graph using face grid
plot1_flu <-  data_graph %>% 
     ggplot(aes(as.numeric(week),flu_rate, group = season, color = season)) +
     geom_line(aes(linetype = season, size = season))+#size = 0.7, linetype = 1, alpha = 0.6) +
     scale_x_continuous(breaks = data_graph$breaks_x, labels = data_graph$week, limits = c(10,42))  +
    # scale_color_manual(values=c('#999999','#E69F00', '#E69F00','#E69F00', '#E69F00')) +
     scale_linetype_manual(values=c(rep("solid", axis_nh), "twodash"))+
     scale_size_manual(values=c(rep(0.5, axis_nh), 0.7))+
     

     # create a rectangle 
     geom_rect(data = db_rect, aes(xmin = 10, xmax = 42, ymin = -Inf, ymax = Inf),color = NA, fill = '#E06666', alpha = 0.1)   +
     facet_grid(country ~ ., scales = "free_y", ) 
#facet_wrap(vars(country), scales = "free_y")

flu_nh <- plot1_flu + theme_bw() +
     theme(strip.placement = "outside",
           strip.background = element_rect(fill="grey90", color="grey50"),
           panel.spacing=unit(0.1,"cm"),
           legend.position = "bottom", 
           legend.direction = "horizontal") +
     labs(title = "Hospitalization rate for influenza, by year",
          subtitle = "Northern Hemisphere, 2016-17 through 2022–23 seasons",
          x= "Calendar week",
          y = "hospitalization (per 100,000)",
          caption = "(Epi week from 40 to 20)")

# # Plot only 2022-23, to highlight it
# flu_nh_last <- flu_nh +
#      geom_line(data = data_graph_last_season,
#                aes(as.numeric(week),flu_rate),
#                color = "#FF66CC",
#                size = 0.75)

##

#Save
ggsave(
     paste0('output/Fig 02 - Hospitalization rates by season/Fig02_Hospitalization_rates_flu_NH.png'),
     flu_nh,
     width=16,
     height=9
)

####  ##############################################
####  ########### RSV PLOTS NH    ##################
####  ##############################################



# Graph using face grid
plot1_rsv <-  data_graph %>% 
     ggplot(aes(as.numeric(week),rsv_rate, group = season, color = season)) +
     geom_line(aes(linetype = season, size = season)) +#(size = 0.4, linetype = 1) +
     scale_x_continuous(breaks = data_graph$breaks_x, labels = data_graph$week,limits = c(10,42))  +
     scale_linetype_manual(values=c(rep("solid", axis_nh), "twodash"))+
     scale_size_manual(values=c(rep(0.5, axis_nh), 0.7))+     
     # create a rectangle 
     geom_rect(data = db_rect, aes(xmin = 10, xmax = 42, ymin = -Inf, ymax = Inf),color = NA, fill = '#E06666', alpha = 0.1)   +
     facet_grid(country ~ ., scales = "free_y", ) 
#facet_wrap(vars(country), scales = "free_y")

rsv_nh <- plot1_rsv + theme_bw() +
     theme(strip.placement = "outside",
           strip.background = element_rect(fill="grey90", color="grey50"),
           panel.spacing=unit(0.1,"cm"),
           legend.position = "bottom", 
           legend.direction = "horizontal") +
     labs(title = "Hospitalization rate for RSV, by year",
          subtitle = "Northern Hemisphere, 2016-17 through 2022–23 seasons",
          x= "Calendar week",
          y = "hospitalization (per 100,000)")

# # Plot only 2022-23, to highlight it
# rsv_nh_last <- rsv_nh +
#      geom_line(data = data_graph_last_season,
#                aes(as.numeric(week),rsv_rate),
#                color = "#FF66CC",
#                size = 0.75)


# Save
ggsave(
     paste0('output/Fig 02 - Hospitalization rates by season/Fig02_Hospitalization_rates_rsv_NH.png'),
     rsv_nh,
     width=16,
     height=9
)


#### ################################################## ####
#### ########## GRAPH SOUTHERN HEMISPHER ############## ####
#### ################################################## ####

# Data ser
#week = factor(week,levels = c(20:52,1:19)

data_graph_sh <-  subset_countries_final %>%
     filter(season != "2015-16", hemisphere == "SH", data_source != "AUS DOH", season != "2021-22") %>% # Excluding 2021-22 season (only 13 month outside of sriveillance weeks)
     mutate(week = factor(week,levels = c(8:52,1:7)),
            season = as.factor(season),
            breaks_x = as.numeric(week),
            label_x = week)

axis_sh <- length(unique(data_graph_sh$season))-1

## To create a thicker line
# data_graph_last_season_sh <- data_graph_sh %>% 
#      filter(season == "2022-23")



####  ##############################################
####  ########### INFLUENZA PLOTS SH ###############
####  ##############################################
# to make the rectangle 

db_rect_sh <- data_graph_sh %>% 
     group_by(country, week) %>% 
     filter(week == 40, year == 2017, country == "AUS" | country =="CL")

# Using facet_grid

plot1_flu_sh <-  data_graph_sh %>% 
     filter(hemisphere == "SH") %>% 
     ggplot(aes(as.numeric(week),flu_rate, group = season, color = season)) +
     geom_line(aes(linetype = season, size = season)) + #(size = 0.4, linetype = 1) +
     scale_x_continuous(breaks = data_graph_sh$breaks_x, labels = data_graph_sh$week)  +
     scale_linetype_manual(values=c(rep("solid", axis_sh), "twodash"))+
     scale_size_manual(values=c(rep(0.5, axis_sh), 0.7))+          
     
     # create a rectangle 
     geom_rect(data = db_rect_sh, aes(xmin = 7, xmax = 24, ymin = -Inf, ymax = Inf),color = NA, fill = '#E06666', alpha = 0.1)   +
     facet_grid(country ~ ., scales = "free_y", ) 
#facet_wrap(vars(country), scales = "free_y")

flu_sh <- plot1_flu_sh + theme(strip.placement = "outside",
                                  strip.background = element_rect(fill="grey90", color="grey50"),
                                  panel.spacing=unit(0.2,"cm"),
                                  legend.position = "bottom", 
                                  legend.direction = "horizontal") +
     labs(title = "Hospitalization rate for influenza, by year",
          subtitle = "Southern Hemisphere, 2016-17 through 2022–23 seasons",
          x= "Calendar week",
          y = "hospitalization (per 100,000)")


# # Plot only 2022-23, to highlight it
# flu_sh_last <- flu_sh +
#      geom_line(data = data_graph_last_season_sh,
#                aes(as.numeric(week),flu_rate),
#                color = "#FF66CC",
#                size = 0.75)
# 

# Save
ggsave(
     paste0('output/Fig 02 - Hospitalization rates by season/Fig02_Hospitalization_rates_flu_SH.png'),
     flu_sh,
     width=16,
     height=9
)

####  ##############################################
####  ########### RSV PLOTS SH ####################
####  ##############################################


plot1_rsv_sh <-  data_graph_sh %>% 
     filter(hemisphere == "SH") %>% 
     ggplot(aes(as.numeric(week),rsv_rate, group = season, color = season)) +
     geom_line(aes(linetype = season, size = season)) + #(size = 0.4, linetype = 1) +
     scale_x_continuous(breaks = data_graph_sh$breaks_x, labels = data_graph_sh$week)  +
     scale_linetype_manual(values=c(rep("solid", axis_sh), "twodash"))+
     scale_size_manual(values=c(rep(0.5, axis_sh), 0.7))+ 
     
     # create a rectangle 
     geom_rect(data = db_rect_sh, aes(xmin = 7, xmax = 24, ymin = -Inf, ymax = Inf),color = NA, fill = '#E06666', alpha = 0.1)   +
     facet_grid(country ~ ., scales = "free_y", ) 
#facet_wrap(vars(country), scales = "free_y")

rsv_sh <- plot1_rsv_sh + theme(strip.placement = "outside",
                                  strip.background = element_rect(fill="grey90", color="grey50"),
                                  panel.spacing=unit(0.2,"cm"),
                                  legend.position = "bottom", 
                                  legend.direction = "horizontal") +
     labs(title = "Hospitalization rate for RSV, by year",
          subtitle = "Southern Hemisphere, 2016-17 through 2022–23 seasons",
          x= "Calendar week",
          y = "hospitalization (per 100,000)")

# # Plot only 2022-23, to highlight it
# rsv_sh_last <- rsv_sh +
#      geom_line(data = data_graph_last_season_sh,
#                aes(as.numeric(week),rsv_rate),
#                color = "#FF66CC",
#                size = 0.75)

# Save
ggsave(
     paste0('output/Fig 02 - Hospitalization rates by season/Fig02_Hospitalization_rates_rsv_SH.png'),
     rsv_sh,
     width=16,
     height=9
)


