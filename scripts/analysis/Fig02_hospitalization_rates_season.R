library(tidyverse)
library(ggplot2)
library(readr)
library(ftplottools) #themes
library(cowplot)
library(grid)
library(ggpubr)

# Data
all_countries <- read_csv("data/merged_data/merged_data.csv")

# populations to get the absolute numbers
# Germany
germany_pop <- data.frame(year = c(2016:2019, 2022, 2022),
                          pop = c(82500000, 82700000, 82900000, 83100000, 84300000, 84300000))
# USA
usa_pop <- data.frame(year = c(2016:2019, 2022, 2023),
                      pop = c(323100000, 325100000, 326800000, 328300000, 333300000, 333300000))

# Absolute flu number Germany
for(i in c(germany_pop$year)) {
     all_countries$hsp_abs_flu <- ifelse(all_countries$country == "DE" & all_countries$year == i, round((all_countries$hsp_rate_flu* germany_pop$pop[germany_pop$year == i])/100000,0),all_countries$hsp_abs_flu)
}

# Absolute flu number USA

for(i in c(usa_pop$year)) {
     all_countries$hsp_abs_flu <- ifelse(all_countries$country == "US" & all_countries$year == i, round((all_countries$hsp_rate_flu* usa_pop$pop[usa_pop$year == i])/100000,0),all_countries$hsp_abs_flu)
}

## Standarised data





# New variable - season in NH

seasons <- data.frame(start = c(2015:2022),
                      end = c(2016:2023),
                      seas = c("2015-16","2016-17","2017-18", "2018-19","2019-20","2020-21","2021-22","2022-23"))

# Create the new variable

all_countries$season <- (ifelse((all_countries$year == 2015 & all_countries$week   %in% c(40:52)) | (all_countries$year == 2016 & all_countries$week %in% c(1:39)) , "2015-16", 0))

# Populate the rest of the seasons

for(i in c(seasons$start)) {
     year_start <- i
     year_end <- (seasons$end[seasons$start== i])
     p_season <- (seasons$seas[seasons$start== i])

     all_countries$season <- (ifelse((all_countries$year == year_start & all_countries$week   %in% c(40:52)) | (all_countries$year == year_end & all_countries$week %in% c(1:39)) , p_season, all_countries$season))
     
}

# Filter data
#week = factor(week,levels = c(40:52,1:39) original
data_graph <-  all_countries %>%
     filter(age_group == "ALL") %>%
     filter(data_source !="FluNet - Non Sentinel" & season != "2015-16") %>% 
     mutate(week = factor(week,levels = c(31:52,1:30)),
            season = as.factor(season),
            breaks_x = as.numeric(week),
            label_x = week)

# Table of NH countries
countries <- data.frame(country = c("UK", "US", "DE", "FR", "AUS", "CL"),
                        hemisphere = c(rep("NH",4), "SH","SH"))

nh_countries <- countries[countries$hemisphere == "NH",]


#############  FLU PLOTS ###################
##############################################
# to make the rectangle 
db_rect <- data_graph %>% 
     group_by(country, week) %>% 
     filter(week == 40, year == 2017, country == "UK" | country =="DE"|country == "US"|country == "FR")

# Graph using face grid
plot1_flu <-  data_graph %>% 
     filter(hemisphere == "NH") %>% 
     ggplot(aes(as.numeric(week),hsp_rate_flu, group = season, color = season)) +
     geom_line(size = 0.4, linetype = 1) +
     scale_x_continuous(breaks = data_graph$breaks_x, labels = data_graph$week)  +
     
     # create a rectangle 
     geom_rect(data = db_rect, aes(xmin = 10, xmax = 42, ymin = -Inf, ymax = Inf),color = NA, fill = '#E06666', alpha = 0.1)   +
     facet_grid(country ~ ., scales = "free_y", ) 
#facet_wrap(vars(country), scales = "free_y")

flu_nh2 <- plot1_flu + theme_bw() +
     theme(strip.placement = "outside",
           strip.background = element_rect(fill="grey90", color="grey50"),
           panel.spacing=unit(0.1,"cm"),
           legend.position = "bottom", 
           legend.direction = "horizontal") +
     labs(title = "Flu - Northern Hemisphere",
          x= "Calendar week",
          y = "hospitalization (per 100,000)")

# Save
# ggsave(
#      paste0('output/Fig 02 - Hospitalization rates by season/Fig 02 : Hospitalization rates flu - NH.png'),
#      flu_nh2,
#      width=16,
#      height=9
# )

#############  RSV PLOTS ###################
##############################################

# Graph using face grid
plot1_rsv <-  data_graph %>% 
     filter(hemisphere == "NH") %>% 
     ggplot(aes(as.numeric(week),hsp_rate_rsv, group = season, color = season)) +
     geom_line(size = 0.4, linetype = 1) +
     scale_x_continuous(breaks = data_graph$breaks_x, labels = data_graph$week)  +
     
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
     labs(title = "RSV - Northern Hemisphere",
          x= "Calendar week",
          y = "hospitalization (per 100,000)")

# Save
# ggsave(
#      paste0('output/Fig 02 - Hospitalization rates by season/Fig 02 : Hospitalization rates rsv - NH.png'),
#      rsv_nh,
#      width=16,
#      height=9
# )



#############################################
##### #### SOUTHERN HEMISPHERE ##############
#############################################


# New variable - season in SH

seasons_sh <- data.frame(start = c(2015:2022),
                      end = c(2016:2023),
                      seas = c("2015-16","2016-17","2017-18", "2018-19","2019-20","2020-21","2021-22","2022-23"))

# Create the new variable

all_countries$season_sh <- (ifelse((all_countries$year == 2015 & all_countries$week   %in% c(14:52)) | (all_countries$year == 2016 & all_countries$week %in% c(1:13)) , "2015-16", 0))

# Populate the rest of the seasons

for(i in c(seasons_sh$start)) {
     year_start <- i
     year_end <- (seasons_sh$end[seasons_sh$start== i])
     p_season <- (seasons_sh$seas[seasons_sh$start== i])
     
     all_countries$season_sh <- (ifelse((all_countries$year == year_start & all_countries$week   %in% c(14:52)) | (all_countries$year == year_end & all_countries$week %in% c(1:13)) , p_season, all_countries$season_sh))
     
}

#week = factor(week,levels = c(20:52,1:19)
data_graph_sh <-  all_countries %>%
     filter(age_group == "ALL") %>%
     filter(data_source !="FluNet - Non Sentinel" & season_sh != "2015-16", !is.na(hsp_rate_flu)) %>% 
     mutate(week = factor(week,levels = c(8:52,1:7)),
            season = as.factor(season_sh),
            breaks_x = as.numeric(week),
            label_x = week) 
     

# to make the rectangle 
db_rect_sh <- data_graph_sh %>% 
     group_by(country, week) %>% 
     filter(week == 40, year == 2017, country == "AUS" | country =="CL")

#############  FLU PLOTS ###################
##############################################

plot1_flu_sh <-  data_graph_sh %>% 
     filter(hemisphere == "SH") %>% 
     ggplot(aes(as.numeric(week),hsp_rate_flu, group = season, color = season)) +
     geom_line(size = 0.4, linetype = 1) +
     scale_x_continuous(breaks = data_graph_sh$breaks_x, labels = data_graph_sh$week)  +
     
     # create a rectangle 
     geom_rect(data = db_rect_sh, aes(xmin = 7, xmax = 24, ymin = -Inf, ymax = Inf),color = NA, fill = '#E06666', alpha = 0.1)   +
     facet_grid(country ~ ., scales = "free_y", ) 
#facet_wrap(vars(country), scales = "free_y")

flu_nh_sh <- plot1_flu_sh + theme(strip.placement = "outside",
                                   strip.background = element_rect(fill="grey90", color="grey50"),
                                   panel.spacing=unit(0.2,"cm"),
                                   legend.position = "bottom", 
                                   legend.direction = "horizontal") +
     labs(title = "Flu - Suthern Hemisphere",
          x= "Calendar week",
          y = "hospitalization (per 100,000)")

# Save
# ggsave(
#      paste0('output/Fig 02 - Hospitalization rates by season/Fig 02 : Hospitalization rates flu - SH.png'),
#      flu_nh_sh,
#      width=16,
#      height=9
# )


#############  RSV PLOTS ###################
##############################################

plot1_rsv_sh <-  data_graph_sh %>% 
     filter(hemisphere == "SH") %>% 
     ggplot(aes(as.numeric(week),hsp_rate_rsv, group = season, color = season)) +
     geom_line(size = 0.4, linetype = 1) +
     scale_x_continuous(breaks = data_graph_sh$breaks_x, labels = data_graph_sh$week)  +
     
     # create a rectangle 
     geom_rect(data = db_rect_sh, aes(xmin = 7, xmax = 24, ymin = -Inf, ymax = Inf),color = NA, fill = '#E06666', alpha = 0.1)   +
     facet_grid(country ~ ., scales = "free_y", ) 
#facet_wrap(vars(country), scales = "free_y")

rsv_nh_sh <- plot1_rsv_sh + theme(strip.placement = "outside",
                                  strip.background = element_rect(fill="grey90", color="grey50"),
                                  panel.spacing=unit(0.2,"cm"),
                                  legend.position = "bottom", 
                                  legend.direction = "horizontal") +
     labs(title = "RSV - Suthern Hemisphere",
          x= "Calendar week",
          y = "hospitalization (per 100,000)")

# Save
# ggsave(
#      paste0('output/Fig 02 - Hospitalization rates by season/Fig 02 : Hospitalization rates rsv - SH.png'),
#      rsv_nh_sh,
#      width=16,
#      height=9
# )

