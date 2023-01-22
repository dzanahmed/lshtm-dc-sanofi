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




# New variable - season

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

######################
##### FLU - NH #######
######################

# Filter data
data_graph <-  all_countries %>%
     filter(age_group == "ALL") %>%
     filter(data_source !="FluNet - Non Sentinel" & season != "2015-16") %>% 
     mutate(week = factor(week,levels = c(40:52,1:39)),
            season = as.factor(season)) 

# Table of NH countries
countries <- data.frame(country = c("UK", "US", "DE", "FR", "AUS", "CL"),
                        hemisphere = c(rep("NH",4), "SH","SH"))

nh_countries <- countries[countries$hemisphere == "NH",]

# Graph FLU - NH
plots <-list() # list for storing the plots
for(i in c(nh_countries$country)) {
     country <- i
     #Data plot
     db_plot1  <-  data_graph %>%  
          filter(country == i) %>% 
          mutate(breaks_x = as.numeric(week),
                 label_x = week)
     # Plot 1
     plot1 <-  db_plot1 %>% 
          ggplot(aes(as.numeric(week),hsp_rate_flu, group = season, color = season)) +
          geom_line(size = 0.4, linetype = 2) +
          scale_x_continuous(breaks = db_plot1$breaks_x, labels = db_plot1$week) +
          
          # create a rectangle 
          geom_rect(data = data_graph[1,], aes(xmin = 1, xmax = 33, ymin = -Inf, ymax = Inf),color = NA, fill = '#E06666', alpha = 0.1) +
          
          # Theme
          ft_theme(legend_right = T, base_size = 08, base_family = "", base_line_size = "", base_rect_size = "") +
          theme(axis.text.x = element_text(color = "grey20", size = 7, angle = 90, hjust = .5, vjust = .5, face = "plain"), 
                legend.position="none", 
                legend.direction = "horizontal",
                legend.key.size = unit(0.4, 'cm'),
                title =element_text(size=8, face='bold'))  +
          
          labs(title = paste("Country: ", i),
               y = "Hospitalization (per 100,000)",
               x = "Calendar weeks")
     
     # Data plot 2
     db_plot2 <- filter(db_plot1, week %in% c(40:52,1:20))
     # Plot 2
     plots[[i]] <- plot1 + geom_line(data = db_plot2, size = 0.7, linetype = 1) 
     
     #Save
     # ggsave(
     #         paste0('Plots/Fig 02: ' , i ,'- Hospitalization rates.png'),
     #         plot2,
     #         width=16,
     #         height=9
     #         )
     
}

# All NH - FLU

plots_nh_flu <- ggarrange(
     plots$UK + rremove("ylab")+ rremove("xlab"), 
     plots$DE + rremove("xlab") + rremove("ylab"),
     plots$US + rremove("xlab") + rremove("ylab"),
     plots$FR + rremove("xlab") + rremove("ylab"), # remove axis labels from plots
     labels = NULL,
     ncol = 1, nrow = 4,
     common.legend = TRUE, legend = "bottom",
     align = "hv", 
     font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

flu_nh <- annotate_figure(plots_nh_flu, left = textGrob("Hospitalizations (per 100,000)", rot = 90, vjust = 1, gp = gpar(cex = 1)), bottom = textGrob("Calendar weeks", gp = gpar(cex = 1)), fig.lab ="Flu - Northern Hemisphere")

# Save
ggsave(
     paste0('output/Fig 02 - Hospitalization rates by season/Fig 02 : Hospitalization rates flu - NH.png'),
     flu_nh,
     width=16,
     height=9
)
