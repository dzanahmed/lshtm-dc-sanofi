################
### Packages ###
################
library(tidyverse)
library(ggplot2)
library(readr)
library(cowplot)


### Data last update 2/2/23 ###
###############################

data <- read_csv("/data/merged_data/merged_data.csv")

data$country[data$country == "DE"] <- "GER"
data$country[data$country == "FR"] <- "FRA"
data$country[data$country == "CL"] <- "CHI"
data$country[data$country == "US"] <- "USA"

#################################
##### NEW VARABLE: SEASON #######
#################################

seasons <- data.frame(start = c(2015:2022),
                      end = c(2016:2023),
                      seas = c("2015-16","2016-17","2017-18", "2018-19","2019-20","2020-21","2021-22","2022-23"))

data$season <- NA # Create the variable

## For loop to iterate the "data" dataset

for(i in c(seasons$start)) {
     year_start <- i
     year_end <- (seasons$end[seasons$start== i])
     p_season <- (seasons$seas[seasons$start== i])
     
     # Differentiate NH VS SH
     data$season <-
          ifelse((data$hemisphere == "NH" & data$year == year_start & data$week %in% c(40:52)) | 
                      (data$hemisphere == "NH" & data$year == year_end & data$week %in% c(1:39)), p_season,
                 ifelse(
                      (data$hemisphere == "SH"& data$year == year_start & data$week   %in% c(14:52)) |
                           (data$hemisphere == "SH"& data$year == year_end & data$week %in% c(1:13)),p_season,data$season))
     
}

#################################
###### Northern Hemisphere ######
#################################


data_graph <-  data %>%
     filter(season != "2015-16", hemisphere == "NH", season != "2021-22") %>% 
     #week = factor(week,levels = c(40:52,1:39) original
     mutate(week = factor(week,levels = c(31:52,1:30)),
            season = as.factor(season),
            breaks_x = as.numeric(week),
            label_x = week) 
data_graph$week <- as.numeric(data_graph$week)



##############################################
##########   ####  #      #    #   ###########
##########   #     #      #    #   ###########
##########   # #   #      #    #   ###########
##########   #     #      #    #   ###########
##########   #     ####    ####    ###########      
##############################################


###### FLU NH COUNTRIES ######
##############################

seasons_color <- c("2016-17"='#C87479',
                   "2017-18" ='#EEB702',
                   "2018-19" = '#758B59', 
                   "2022-23"='#4D4F80')

#--- UK

flu_uk <- data_graph %>% 
               filter(country == "UK", data_source == "UKHSA") %>% 
               droplevels(data_graph$season) %>% 
          ggplot(aes(week, hsp_rate_flu, group = season, color = season, alpha = season)) +
          scale_x_continuous(breaks = seq(10,43, 2), labels = c(seq(40,52, 2),seq(2,21, 2)), limits = c(10,43),expand = c(0, 0)) +
          scale_colour_manual("Season", 
                              values=c("2017-18" ='#EEB702',
                                       "2018-19" = '#758B59', 
                                       "2022-23"='#DC143C')) +
          geom_line(size = 1) + theme_bw() +
          scale_alpha_manual(values=c(0.7,0.7,1)) +
                theme(
               plot.title = element_text(size = 14, face = "bold", vjust = 2),
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               #axis.title.x = element_text(size=12),# face = "bold"),
               axis.title.y=element_blank(),
               #axis.title.y = element_text(size=12),# face = "bold"),
               legend.position = "nono",
               legend.title=element_text(size=12, face = "bold"), 
               legend.text=element_text(size=11),
               strip.text = element_text(size = 12, face = "bold"),
               plot.margin = unit(c(25, 10, 0, 5.5),"pt")
               ) +
     # geom_text(x = 11, y = 15,
     #           label = "UK",
     #           color = "black",
     #           size = 12) +
     labs(title = "(A) Northern Hemisphere",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")
flu_uk    

#--- USA

flu_usa <- data_graph %>% 
     filter(country == "USA", age_group == "ALL") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_flu, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(10,43, 2), labels = c(seq(40,52, 2),seq(2,21, 2)), limits = c(10,43),expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016-17" ='#8f90b3',
                                  "2017-18" ='#EEB702',
                                  "2018-19" = '#758B59', 
                                  "2022-23"='#DC143C')) +
     geom_line(size = 1) + theme_bw() +
     scale_alpha_manual(values=c(0.7,0.7,0.7,1), guide = 'none') +
     theme(
          plot.title = element_text(size = 14, face = "bold"),
          #axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x = element_text(size=12),# face = "bold"),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "bottom",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(5.5, 10, 0, 5.5),"pt")
     ) +
     labs(#title = "UK",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")
flu_usa  

#--- GER

flu_ger <- data_graph %>% 
     filter(country == "GER", age_group == "ALL") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_flu, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(10,43, 2), labels = c(seq(40,52, 2),seq(2,21, 2)), limits = c(10,43),expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016-17" ='#8f90b3',
                                  "2017-18" ='#EEB702',
                                  "2018-19" = '#758B59', 
                                  "2022-23"='#DC143C')) +
     geom_line(size = 1) + theme_bw() +
     scale_alpha_manual(values=c(0.7,0.7,0.7,0.7,1)) +
     theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          #axis.title.x = element_text(size=12),# face = "bold"),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "none",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(0, 10, 0, 5.5),"pt")
     ) +
     labs(#title = "UK",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")
flu_ger    

     
#--- FR

flu_fr <- data_graph %>% 
     filter(country == "FRA", data_source == "FluNet - Sentinel", season != "2019-20") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_flu, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(10,43, 2), labels = c(seq(40,52, 2),seq(2,21, 2)), limits = c(10,43),expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016-17" ='#8f90b3',
                                  "2017-18" ='#EEB702',
                                  "2018-19" = '#758B59', 
                                  "2022-23"='#DC143C')) +
     geom_line(size = 1) +  scale_alpha_manual(values=c(0.7,0.7,0.7,1)) +
     theme_bw() +
     theme(
          plot.title = element_text(size = 14, face = "bold"),
          #axis.title.x = element_text(size=12),# face = "bold"),
          axis.title.y =element_blank(),
          axis.text.x=element_blank(),
          axis.title.x =element_blank(),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "none",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(0, 10, 0, 5.5),"pt"),
          #plot.tag.position = c(-0.02, 0.5),
          #plot.tag = element_text(angle=90)
     ) +
     labs(#title = "FRA",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          #tag = "arbitrary words",
          caption = "")
flu_fr



###### FLU SH COUNTRIES ######
##############################

data_graph_sh <- data %>% 
     filter(hemisphere == "SH", data_source != "AUS DOH", year != 2023) %>% 
     mutate(season = as.factor(year),
            breaks_x = as.numeric(week),
            label_x = week)


#--- AUS

flu_aus <- data_graph_sh %>% 
     filter(country == "AUS") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_flu, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(1,52, 2), labels = seq(1,52, 2), expand = c(0, 0)) + 
     #scale_x_continuous(breaks = data$breaks_x, labels = data$week, expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016" ='#8f90b3',
                                  "2017" ='#EEB702',
                                  "2018" = '#758B59',
                                  "2019" = '#dc9334',
                                  "2022"='#DC143C')) +
     geom_line(size = 1) + theme_bw() +
     scale_alpha_manual(values=c(0.7,0.7,0.7,0.7,1)) +
     theme(
          plot.title = element_text(size = 14, face = "bold", vjust = 2),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          #axis.title.x = element_text(size=12),# face = "bold"),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "none",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(5.5, 5.5, 0, 5.5),"pt")
     ) +
     labs(title = "(B) Southern Hemisphere",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")
flu_aus  

#---- Chi

flu_chi <- data_graph_sh %>% 
     filter(country == "CHI", data_source == "EPI MINSAL") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_flu, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(1,52, 2), labels = seq(1,52, 2), expand = c(0, 0)) +
     #scale_x_continuous(breaks = data$breaks_x, labels = data$week, expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016" ='#8f90b3',
                                  "2017" ='#EEB702',
                                  "2018" = '#758B59',
                                  "2019" = '#dc9334',
                                  "2022"='#DC143C')) +
     geom_line(size = 1) + theme_bw() +
     scale_alpha_manual(values=c(0.7,0.7,0.7,0.7,1), guide = 'none') +
     theme(
          plot.title = element_text(size = 14, face = "bold"),
          #axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x = element_text(size=12),# face = "bold"),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "bottom",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(0, 5.5, 0, 5.5),"pt")
     ) +
     labs(#title = "AUS",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")

######## MAKING THE LAS FLU GRAPH FOR BOTH HEMISPHERES #####

#create country labels
title_aus <- ggdraw() + 
     draw_label("AUS",fontface = 'bold',x = 0.4,hjust = 0,angle = 90)+theme(plot.margin = margin(0, 0, 0, 0))
title_chi <- ggdraw() + 
     draw_label("CHI",fontface = 'bold',x = 0.4,hjust = 0,angle =90)+theme(plot.margin = margin(0, 0, 0, 0))

country_grid_sh <- plot_grid(title_aus,title_chi,nrow=2)

country_grid_sh

flu_sh <-(country_grid_sh+p_lab | flu_aus  /flu_chi) + plot_layout(widths = c(.1, 1))
flu_sh
ggsave(
     paste0('output/Fig 02 - Hospitalization rates by season/Fig02_Hospitalization_rates_flu_NH_2.png'),
     flu_nh,
     width=6.5,
     height=6.5
)

#####################################
###### FLU PLOT BOTH HEMISPHER #######
######################################

### Cretate y axis

#--- Y axis 
glob_lab <- "Hospitalisation per 100,000 Persons"
p_lab <- 
     ggplot() + 
     annotate(geom = "text", x = 1, y = 1, label = glob_lab, angle = 90) +
     coord_cartesian(clip = "off")+
     theme_void() + theme(plot.margin = margin(0, 0, 0, 0))

####
#create country labels
title_uk <- ggdraw() + 
     draw_label("UK",fontface = 'bold',x = 0.4,hjust = 0,angle = 90)+theme(plot.margin = margin(0, 0, 0, 0))
title_ger <- ggdraw() + 
     draw_label("GER",fontface = 'bold',x = 0.4,hjust = 0,angle =90)+theme(plot.margin = margin(0, 0, 0, 0))
title_fra <- ggdraw() + 
     draw_label("FRA",fontface = 'bold',x = 0.4,hjust = 0,angle =90)+theme(plot.margin = margin(20, 0, 10, 0))
title_usa <- ggdraw() + 
     draw_label("USA",fontface = 'bold',x = 0.4,hjust = 0,angle=90)+theme(plot.margin = margin(30, 0, 0, 0))

country_grid <- plot_grid(title_uk,title_ger,title_fra,title_usa,nrow=4)


## The two hemispheres 
northern_flu <- (flu_uk/ flu_ger /  flu_fr / flu_usa)
southen_flu <- (flu_aus / flu_chi)

# Final plot
influenza_plot <-country_grid + p_lab + northern_flu + country_grid_sh + southen_flu +
     plot_layout(widths = c(.2,.2,5,.2,5),
                 #guides = "collect",
                 design = "
                 12345
                 12345
                 12345
                 12345
              ") +
     plot_annotation(title = 'Hospitalisation rate for influenza by Hemisphere',
                     theme = theme(plot.title = element_text(size = 14, vjust = -0.7, hjust = 0.09, face = "bold")))

ggsave(
     paste0('output/Fig 02 - Hospitalization rates by season/Fig02_Hospitalization_rates_flu.png'),
     influenza_plot,
     width=10,
     height=7
)


##############################################
##########   #####   ####  #      #   ###########
##########   #   #  #      #      #   ###########
##########   ####    ####   #    #    ###########
##########   #  #        #   #  #     ###########
##########   #   #  #####     #       ###########      
##############################################



###-- NORTHEN COUNTRIES

#--- UK

rsv_uk <- data_graph %>% 
     filter(country == "UK", data_source == "UKHSA") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_rsv, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(10,43, 2), labels = c(seq(40,52, 2),seq(2,21, 2)), limits = c(10,43),expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2017-18" ='#EEB702',
                                  "2018-19" = '#758B59', 
                                  "2022-23"='#DC143C')) +
     geom_line(size = 1) + theme_bw() +
     scale_alpha_manual(values=c(0.7,0.7,1)) +
     theme(
          plot.title = element_text(size = 14, face = "bold", vjust = 2),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          #axis.title.x = element_text(size=12),# face = "bold"),
          axis.title.y=element_blank(),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "none",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(25, 10, 0, 5.5),"pt")
     ) +
     # geom_text(x = 11, y = 15,
     #           label = "UK",
     #           color = "black",
     #           size = 12) +
     labs(title = "(A) Northern Hemisphere",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")
rsv_uk    

#--- USA

rsv_usa <- data_graph %>% 
     filter(country == "USA", age_group == "ALL") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_rsv, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(10,43, 2), labels = c(seq(40,52, 2),seq(2,21, 2)), limits = c(10,43),expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016-17" ='#8f90b3',
                                  "2017-18" ='#EEB702',
                                  "2018-19" = '#758B59', 
                                  "2022-23"='#DC143C')) +
     geom_line(size = 1) + theme_bw() +
     scale_alpha_manual(values=c(0.7,0.7,0.7,1), guide = 'none') +
     theme(
          plot.title = element_text(size = 14, face = "bold"),
          #axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x = element_text(size=12),# face = "bold"),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "bottom",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(5.5, 10, 0, 5.5),"pt")
     ) +
     labs(#title = "UK",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")
rsv_usa  

#--- GER

rsv_ger <- data_graph %>% 
     filter(country == "GER", age_group == "ALL") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_rsv, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(10,43, 2), labels = c(seq(40,52, 2),seq(2,21, 2)), limits = c(10,43),expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016-17" ='#8f90b3',
                                  "2017-18" ='#EEB702',
                                  "2018-19" = '#758B59', 
                                  "2022-23"='#DC143C')) +
     geom_line(size = 1) + theme_bw() +
     scale_alpha_manual(values=c(0.7,0.7,0.7,0.7,1)) +
     theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          #axis.title.x = element_text(size=12),# face = "bold"),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "none",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(0, 10, 0, 5.5),"pt")
     ) +
     labs(#title = "UK",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")
rsv_ger    


#--- FR

rsv_fr <- data_graph %>% 
     filter(country == "FRA", data_source == "FluNet - Sentinel", season != "2019-20") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_rsv, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(10,43, 2), labels = c(seq(40,52, 2),seq(2,21, 2)), limits = c(10,43),expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016-17" ='#8f90b3',
                                  "2017-18" ='#EEB702',
                                  "2018-19" = '#758B59', 
                                  "2022-23"='#DC143C')) +
     geom_line(size = 1) +  scale_alpha_manual(values=c(0.7,0.7,0.7,1)) +
     theme_bw() +
     theme(
          plot.title = element_text(size = 14, face = "bold"),
          #axis.title.x = element_text(size=12),# face = "bold"),
          axis.title.y =element_blank(),
          axis.text.x=element_blank(),
          axis.title.x =element_blank(),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "none",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(0, 10, 0, 5.5),"pt"),
          #plot.tag.position = c(-0.02, 0.5),
          #plot.tag = element_text(angle=90)
     ) +
     labs(#title = "FRA",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          #tag = "arbitrary words",
          caption = "")
rsv_fr

###------ SOUTHERN HEMISPHERE

#--- AUS

rsv_aus <- data_graph_sh %>% 
     filter(country == "AUS", !is.na(hsp_abs_rsv)) %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_rsv, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(1,52, 2), labels = seq(1,52, 2), expand = c(0, 0)) + 
     #scale_x_continuous(breaks = data$breaks_x, labels = data$week, expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016" ='#8f90b3',
                                  "2017" ='#EEB702',
                                  "2018" = '#758B59',
                                  "2019" = '#dc9334',
                                  "2022"='#DC143C')) +
     geom_line(size = 1) +  theme_bw() +
     scale_alpha_manual(values=c(0.7,0.7,0.7,0.7,1)) +
     theme(
          plot.title = element_text(size = 14, face = "bold", vjust = 2),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          #axis.title.x = element_text(size=12),# face = "bold"),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "none",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(5.5, 5.5, 0, 5.5),"pt")
     ) +
     labs(title = "(B) Southern Hemisphere",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")
rsv_aus  



#---- Chi

rsv_chi <- data_graph_sh %>% 
     filter(country == "CHI", data_source == "EPI MINSAL") %>% 
     droplevels(data_graph$season) %>% 
     ggplot(aes(week, hsp_rate_rsv, group = season, color = season, alpha = season)) +
     scale_x_continuous(breaks = seq(1,52, 2), labels = seq(1,52, 2), expand = c(0, 0)) +
     #scale_x_continuous(breaks = data$breaks_x, labels = data$week, expand = c(0, 0)) +
     scale_colour_manual("Season", 
                         values=c("2016" ='#8f90b3',
                                  "2017" ='#EEB702',
                                  "2018" = '#758B59',
                                  "2019" = '#dc9334',
                                  "2022"='#DC143C')) +
     geom_line(size = 1) + theme_bw() +
     scale_alpha_manual(values=c(0.7,0.7,0.7,0.7,1), guide = 'none') +
     theme(
          plot.title = element_text(size = 14, face = "bold"),
          #axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x = element_text(size=12),# face = "bold"),
          #axis.title.y = element_text(size=12),# face = "bold"),
          legend.position = "bottom",
          legend.title=element_text(size=12, face = "bold"), 
          legend.text=element_text(size=11),
          strip.text = element_text(size = 12, face = "bold"),
          plot.margin = unit(c(0, 5.5, 0, 5.5),"pt")
     ) +
     labs(#title = "AUS",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "")
rsv_chi

#### THE FINAL PLOT ####

## The two hemispheres 
northern_rsv <- (rsv_uk/ rsv_ger /  rsv_fr / rsv_usa)
southen_rsv <- (rsv_aus / rsv_chi)

# Final plot
rsv_plot <-country_grid + p_lab + northern_rsv + country_grid_sh + southen_rsv +
     plot_layout(widths = c(.2,.2,5,.2,5),
                 #guides = "collect",
                 design = "
                 12345
                 12345
                 12345
                 12345
              ") +
     plot_annotation(title = 'Hospitalisation rate for RSV by Hemisphere',
                     theme = theme(plot.title = element_text(size = 14, vjust = -0.7, hjust = 0.09, face = "bold")))

rsv_plot

ggsave(
     paste0('output/Fig 02 - Hospitalization rates by season/Fig02_Hospitalization_rates_RSV.png'),
     rsv_plot,
     width=10,
     height=7
)

