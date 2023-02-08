##----------------------------------------------------------------
##                           Packages                           --
##----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(readr)
library(patchwork)
library(cowplot)

##----------------------------------------------------------------
##                          Last data:                          --
##                            2/2/23                            --
##----------------------------------------------------------------


data <- read_csv("data/merged_data/merged_data.csv")

data$country[data$country == "DE"] <- "GER"
data$country[data$country == "FR"] <- "FRA"
data$country[data$country == "CL"] <- "CHI"
data$country[data$country == "US"] <- "USA"

##---------------------------------------------------------------
##                    CREATE A NEW VARABLE:                    --
##---------------------------------------------------------------

# Data frame to use in the loop.

seasons <- data.frame(start = c(2015:2022),
                      end = c(2016:2023),
                      seas = c("2015-16","2016-17","2017-18", "2018-19","2019-20","2020-21","2021-22","2022-23"))

data$season <- NA # Create the variable

## For loop to create season in NH

for(i in c(seasons$start)) {
     year_start <- i
     year_end <- (seasons$end[seasons$start== i])
     p_season <- (seasons$seas[seasons$start== i])
     
 
     data$season <-ifelse((data$hemisphere == "NH" & data$year == year_start & data$week %in% c(40:52)) | 
          (data$hemisphere == "NH" & data$year == year_end & data$week %in% c(1:39)), p_season,data$season)

}

## To create season in SH

data$season[data$hemisphere == "SH"] <-as.character(data$year)


##################################################################
##                     NORTHERN HEMISPHERE:                     ##
##                      CLEANING AND PLOTS                      ##
##################################################################


##----------------------------------------------------------------
##                           cleaning                           --
##----------------------------------------------------------------

data_graph_nh <-     data %>% 
     filter(age_group == "ALL", season != "2015-16",season != "2021-22", season != "2019-20", data_source != 	
"FluNet - Non Sentinel") %>% 
     mutate(week = factor(week,levels = c(31:52,1:30)),
            season = as.factor(season),
            breaks_x = as.numeric(week),
            label_x = week) 

# Transform week to numeric (x-axis graph) & drop levels

data_graph_nh <- data_graph_nh %>% 
     mutate(week = as.numeric(week)) %>% 
     droplevels(data_graph_nh$season)

##----------------------------------------------------------------
##                           Plots                           --
##----------------------------------------------------------------

graph_nh <- list() # list for storing the plots.

for(v in c("hsp_rate_flu", "hsp_rate_rsv")){
     for(i in c("UK", "USA", "GER", "FRA")){

          nh_country <- data_graph_nh %>% 
               filter(country == i) %>% 
               select(week, season, virus = v) %>% 
               ggplot(aes(week, virus, group = season, color = season, alpha = season)) +
               scale_x_continuous(breaks = seq(10,43, 2), labels = c(seq(40,52, 2),seq(2,21, 2)), limits = c(10,43),expand = c(0, 0)) +
               scale_colour_manual("Season", 
                                   values=c("2016-17" ='#8f90b3',
                                            "2017-18" ='#EEB702',
                                            "2018-19" = '#758B59', 
                                            "2022-23"='#DC143C')) +
               geom_line(size = 1) + theme_bw() +
               scale_alpha_manual(values=c(rep(0.7,data_graph_nh %>% filter(country == i) %>% summarise(n = length(unique(season)))-1),1),guide = 'none') +
               theme(
                    plot.title = element_text(size = 14, face = "bold", vjust = 2),
                    axis.title.y=element_blank(),
                    legend.position = "bottom",
                    legend.title=element_text(size=12, face = "bold"), 
                    legend.text=element_text(size=11),
                    plot.margin = unit(c(0, 10, 5.5, 5.5),"pt") #plot.margin(top, right, bottom, and left margins)
               )
          graph_nh[[paste0(v,i)]] <-nh_country
          
               }
          }


##################################################################
##                     SOUTHERN HEMISPHERE:                     ##
##                      CLEANING AND PLOTS                      ##
##################################################################


##----------------------------------------------------------------
##                           cleaning                           --
##----------------------------------------------------------------

data_graph_sh <- data %>% 
     filter(hemisphere == "SH", data_source != "AUS DOH", year != 2023, data_source != "EPI MINSAL - Ministry of Science", !is.na(hsp_rate_rsv)) %>% 
     mutate(season = as.factor(year),
            breaks_x = as.numeric(week),
            label_x = week)

##----------------------------------------------------------------
##                           Plots                           --
##----------------------------------------------------------------

graph_sh <- list() #list for storing the plots.

for(v in c("hsp_rate_flu", "hsp_rate_rsv")){
     for(i in c("AUS", "CHI")){
          
          sh_country <- data_graph_sh %>% 
               filter(country == i) %>% 
               select(week, season, virus = v) %>% 
               ggplot(aes(week, virus, group = season, color = season, alpha = season)) +
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
                    axis.title.y=element_blank(),
                    axis.title.x = element_text(size=12),# face = "bold"),
                    legend.position = "bottom",
                    legend.title=element_text(size=12, face = "bold"), 
                    legend.text=element_text(size=11),
                    strip.text = element_text(size = 12, face = "bold"),
                    plot.margin = unit(c(0, 5.5, 0, 5.5),"pt") 
               ) +
               labs(x= "Epi week",
                    y = "Hospitalisation per \n 100,000 Persons",
                    caption = "")
          graph_sh[[paste0(v,i)]] <-sh_country

     }
}

##################################################################
##                          PATCHWORK:                          ##
##                            PLOTS                             ##
##################################################################

# Theme to delete legend and x axis
theme1 <- theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position = "none")

##----------------------------------------------------------------
##                        countries tags                        --
##----------------------------------------------------------------
tag <- list()
for(i in c("UK","GER","FRA","USA", "AUS","CHI")){
     title <- ggdraw()+ draw_label(i,fontface = 'bold',x = 0.4,hjust = 0,angle = 90)
    tag[[i]]<-title
}  

# NORTHERN
grid_nh<-plot_grid(tag$UK,tag$GER, (tag$FRA +theme(plot.margin = margin(20, 0, 10, 0))), (tag$USA+ theme(plot.margin = margin(35, 0, 0, 0))),nrow=4)

# SOURTHERN
grid_sh<-plot_grid(tag$AUS,tag$CHI, nrow=2)

# Y-AXIS FOR PATCHWORK
p_lab <- 
     ggplot() + 
     annotate(geom = "text", x = 1, y = 1, label = "Hospitalisation per 100,000 Persons", angle = 90) +
     coord_cartesian(clip = "off")+
     theme_void() + theme(plot.margin = margin(0, 0, 0, 0))

#################################################################
##                             FLU                             ##
##                            PLOTS                            ##
#################################################################

##----------------------------------------------------------------
##                         NH countries                         --
##----------------------------------------------------------------

# Some counties are modified to make the patchwork
hsp_rate_fluUK <- graph_nh$hsp_rate_fluUK + labs(title = "A. Northern Hemisphere")+ theme1  

hsp_rate_fluUSA <- graph_nh$hsp_rate_fluUSA +  theme(axis.title.x = element_text(size=12)) + labs(x = "Epi week")

# Final flu NH graph
flu_nh <- (hsp_rate_fluUK / (graph_nh$hsp_rate_fluGER + theme1) /  (graph_nh$hsp_rate_fluFRA + theme1) / hsp_rate_fluUSA)                                       


##----------------------------------------------------------------
##                         SH countries                         --
##----------------------------------------------------------------
hsp_rate_fluAUS <- graph_sh$hsp_rate_fluAUS + 
          labs(title = "B. Southern Hemisphere",
               x= "Epi week",
               y = "Hospitalisation per \n 100,000 Persons",
               caption = "") + theme(
                    plot.title = element_text(size = 14, face = "bold"),
                    axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    legend.position = "none",
                    plot.margin = unit(c(25, 5.5, 0, 5.5),"pt") 
)

# Final flu SH graph
flu_sh <- (hsp_rate_fluAUS / graph_sh$hsp_rate_fluCHI)


##----------------------------------------------------------------
##                        Final Flu Plot                        --
##----------------------------------------------------------------
influenza_plot <-grid_nh + p_lab + flu_nh + grid_sh + flu_sh +
     plot_layout(widths = c(.2,.2,5,.2,5),
                 #guides = "collect",
                 design = "
                 12345
                 12345
                 12345
                 12345
              ") +
     plot_annotation(#title = 'Hospitalisation rate for influenza by Hemisphere',
                     theme = theme(plot.title = element_text(size = 14, vjust = -0.7, hjust = 0.09, face = "bold")))

# ggsave(
#      paste0('output/Fig 02 - Hospitalization rates by season/Fig02_Hospitalization_rates_flu.png'),
#      influenza_plot,
#      width=10,
#      height=7
# )

#################################################################
##                             RSV                             ##
##                            PLOTS                            ##
#################################################################

##----------------------------------------------------------------
##                         NH countries                         --
##----------------------------------------------------------------

#--- Northern countries (modify plots)
hsp_rate_rsvUK <- graph_nh$hsp_rate_rsvUK + labs(title = "A. Northern Hemisphere")+ theme1  #+ #theme(plot.margin = unit(c(25, 10, 0, 5.5),"pt")) + theme1

hsp_rate_rsvUSA <- graph_nh$hsp_rate_rsvUSA +  theme(axis.title.x = element_text(size=12)) + labs(x = "Epi week")

# Final RSV NH graph
rsv_nh <- (hsp_rate_rsvUK / (graph_nh$hsp_rate_rsvGER + theme1) /  (graph_nh$hsp_rate_rsvFRA + theme1) / hsp_rate_rsvUSA)                                       


##----------------------------------------------------------------
##                         SH countries                         --
##----------------------------------------------------------------

hsp_rate_rsvAUS <- graph_sh$hsp_rate_rsvAUS + 
     labs(title = "B. Southern Hemisphere",
          x= "Epi week",
          y = "Hospitalisation per \n 100,000 Persons",
          caption = "") + theme(
               plot.title = element_text(size = 14, face = "bold"),
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               legend.position = "none",
               plot.margin = unit(c(25, 5.5, 0, 5.5),"pt") 
          )
# Final RSV SH graph
rsv_sh <- (hsp_rate_rsvAUS / graph_sh$hsp_rate_rsvCHI)


##----------------------------------------------------------------
##                        Final RSV Plot                        --
##----------------------------------------------------------------

rsv_plot <-grid_nh + p_lab + rsv_nh + grid_sh + rsv_sh +
     plot_layout(widths = c(.2,.2,5,.2,5),
                 #guides = "collect",
                 design = "
                 12345
                 12345
                 12345
                 12345
              ") +
     plot_annotation(#title = 'Hospitalisation rate for RSV by Hemisphere',
                     theme = theme(plot.title = element_text(size = 14, vjust = -0.7, hjust = 0.09, face = "bold")))

# ggsave(
#      paste0('output/Fig 02 - Hospitalization rates by season/Fig02_Hospitalization_rates_RSV.png'),
#      rsv_plot,
#      width=10,
#      height=7
# )
