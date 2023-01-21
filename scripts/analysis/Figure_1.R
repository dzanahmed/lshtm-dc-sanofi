library(tidyverse)
library(lubridate)

data <- read.csv(file='data/merged_data/merged_data.csv')

# Overall hospitalization by virus (Influenza, RSV, COVID-19) for the NH and SH 
# (complete time series for full time period: 2016 - 2023)
# Design: line graph
# X-axis: 2016 - 2023 with a second x-axis label that identifies the seasons

data$start_date <- as.Date(data$start_date)

date_breaks <- data$start_date
epi <- data$week

breaks_mo <- seq(min(data$start_date), max(data$start_date), by="4 months")


# this is messy 

data |> filter(year < 2021, age_group=="ALL") |> ggplot()+
     geom_line(mapping = aes(start_date, hsp_rate_flu, color='Influenza')) +
     geom_line(mapping = aes(start_date, hsp_rate_rsv, color='RSV')) +
     scale_color_manual("", 
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange"))+
     scale_x_date(
          date_breaks = "4 weeks",
          date_minor_breaks = "1 week",
          date_labels = "%b %Y"
     ) +
     scale_y_sqrt() +
     theme_bw() +
     theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
     ))+
     theme(legend.position = 'none')

###

Figure_1_NH <- data |> filter(hemisphere=='NH', age_group=="ALL", data_source!="FluNet - Sentinel") |> 
     filter(country!="UK" | year!=2020 | week<15) |> 
     ggplot()+
     geom_line(mapping = aes(start_date, hsp_rate_flu, color="Influenza")) +
     geom_line(mapping = aes(start_date, hsp_rate_rsv, color="RSV")) +
     scale_color_manual("",
                        breaks = c('Influenza','RSV'),
                        values = c("Influenza"="red", "RSV"="orange"))+
     scale_x_date(date_labels="%b", date_breaks="month", expand=c(0.01,0)) +
     facet_grid(country ~ year(start_date), space="free_x", scales="free_x", switch="x") +
     theme_bw() +
     theme(strip.placement = "outside",
           strip.background = element_rect(fill="grey90", color="grey50"),
           panel.spacing=unit(0,"cm"))+
     theme(axis.text.x = element_text(
          angle = 90,
          vjust = 1.2,
          hjust = 1
     ), 
     plot.title=element_text(hjust=0.5),
     plot.subtitle = element_text(hjust=0.5))+
     theme(legend.position = 'bottom')+
     scale_y_sqrt()+
     labs(title="Influenza and RSV hospitalizations in the Northern hemisphere, seasons 2016-2019 and 2022-23",
          subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)", caption="Preliminary output Dzan")

Figure_1_NH

Figure_1_SH <- data |> filter(hemisphere=='SH', age_group=="ALL") |> 
     filter(data_source!="AUS DOH") |> 
     ggplot()+
        geom_line(mapping = aes(start_date, hsp_rate_flu, color="Influenza")) +
        geom_line(mapping = aes(start_date, hsp_rate_rsv, color="RSV")) +
        scale_color_manual("",
                           breaks = c('Influenza','RSV'),
                           values = c("Influenza"="red", "RSV"="orange"))+
     scale_x_date(date_labels="%b", date_breaks="month", expand=c(0.01,0)) +
     facet_grid(country ~ year(start_date), space="free_x", scales="free_x", switch="x") +
     theme_bw() +
     theme(strip.placement = "outside",
           strip.background = element_rect(fill="grey90", color="grey50"),
           panel.spacing=unit(0,"cm"))+
     theme(axis.text.x = element_text(
          angle = 90,
          vjust = 1.2,
          hjust = 1
     ), 
     plot.title=element_text(hjust=0.5),
     plot.subtitle = element_text(hjust=0.5))+
     #theme(legend.position = 'none')+
     labs(title="Influenza and RSV hospitalizations in the Southern hemisphere, seasons 2016-2019 and 2022-23",
          subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)", caption="Preliminary output Dzan")

Figure_1_SH

Figure_1_Both <- data |> filter(age_group=="ALL") |> 
     filter(data_source!="AUS DOH", data_source!="FluNet - Sentinel") |> 
     filter(country!="UK" | year!=2020 | week<15) |> 
     ggplot()+
        geom_line(mapping = aes(start_date, hsp_rate_flu, color="Influenza")) +
        geom_line(mapping = aes(start_date, hsp_rate_rsv, color="RSV")) +
        scale_color_manual("",
                           breaks = c('Influenza','RSV'),
                           values = c("Influenza"="red", "RSV"="orange"))+
     scale_x_date(date_labels="%b", date_breaks="month", expand=c(0.01,0)) +
     facet_grid(country ~ year(start_date), space="free_x", scales="free_x", switch="x") +
     theme_bw() +
     theme(strip.placement = "outside",
           strip.background = element_rect(fill="grey90", color="grey50"),
           panel.spacing=unit(0,"cm"))+
     theme(axis.text.x = element_text(
          angle = 90,
          vjust = 1.2,
          hjust = 1
     ), 
     plot.title=element_text(hjust=0.5),
     plot.subtitle = element_text(hjust=0.5))+
     scale_y_sqrt()+
     theme(legend.position = 'bottom')+
     labs(title="Influenza and RSV hospitalizations in NH and SH, seasons 2016-2019 and 2022-23",
          subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)", caption="Preliminary output Dzan")

Figure_1_Both


ggsave(
        'output/Hospitalization rates per 100k/Figure_1_NH.png',
        Figure_1_NH,
        width=16,
        height=9
)


ggsave(
        'output/Hospitalization rates per 100k/Figure_1_SH.png',
        Figure_1_SH,
        width=16,
        height=9
)

ggsave(
        'output/Hospitalization rates per 100k/Figure_1_Both.png',
        Figure_1_Both,
        width=16,
        height=9
)

