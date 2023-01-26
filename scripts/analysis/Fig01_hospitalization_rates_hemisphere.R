library(tidyverse)
library(lubridate)

data <- read.csv(file='data/merged_data/merged_data2.csv')

# Overall hospitalization by virus (Influenza, RSV, COVID-19) for the NH and SH 
# (complete time series for full time period: 2016 - 2023)
# Design: line graph
# X-axis: 2016 - 2023 with a second x-axis label that identifies the seasons


# !!!! NOTE TO SELF: THIS CODE NEEDS SERIOUS UPDATING

data$start_date <- as.Date(data$start_date)

date_breaks <- data$start_date
epi <- data$week

breaks_mo <- seq(min(data$start_date), max(data$start_date), by="4 months")


x <- data |> filter(age_group == "ALL") |>
        mutate(hsp_x = ifelse(is.na(hsp_rate_flu)==TRUE, 0, hsp_rate_flu)+
                       ifelse(is.na(hsp_rate_rsv)==TRUE, 0, hsp_rate_rsv)+
                       ifelse(is.na(hsp_rate_covid19)==TRUE, 0, hsp_rate_covid19))


Figure_1_NH <- x |> filter(hemisphere=='NH', age_group=="ALL", data_source!="FluNet - Sentinel") |> 
     filter(country!="UK" | year!=2020 | week<15) |> 
     ggplot()+
     geom_col(mapping=aes(start_date, hsp_x, fill='Total'))+
     geom_line(mapping = aes(start_date, hsp_rate_flu, color="Influenza")) +
     geom_line(mapping = aes(start_date, hsp_rate_rsv, color="RSV")) +
     geom_line(mapping=aes(start_date,hsp_rate_covid19, color='SARS_CoV_2')) +
     scale_color_manual("",
                        breaks = c('Influenza','RSV','SARS_CoV_2', 'Total'),
                        values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue'))+
scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey80')) +
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
     labs(title="Total hospitalizations and hospitalizations by virus in the Northern hemisphere, seasons 2016-2019 and 2022-23",
          subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)", caption="")



Figure_1_NH


x <- data |> filter(age_group == "ALL") |>
        mutate(hsp_x = hsp_rate_flu + hsp_rate_rsv) + ifelse(is.na(hsp_rate_covid19) ==
                                                                     TRUE, 0, hsp_rate_covid19)

Figure_1_SH <- x |> filter(hemisphere=='SH', age_group=="ALL") |> 
     #filter(data_source!="AUS DOH") |> 
        ggplot()+
        #geom_col(mapping=aes(start_date, hsp_x, fill='Total'))+
        geom_line(mapping = aes(start_date, hsp_rate_flu, color="Influenza")) +
        geom_line(mapping = aes(start_date, hsp_rate_rsv, color="RSV")) +
        geom_line(mapping=aes(start_date,hsp_rate_covid19, color='SARS_CoV_2')) +
        scale_color_manual("",
                           breaks = c('Influenza','RSV','SARS_CoV_2', 'Total'),
                           values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue'))+
        #scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey80')) +
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
     labs(title="Total hospitalizations and hospitalizations by virus in the Southern hemisphere, seasons 2016-2019 and 2022-23",
          subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)", caption="")

Figure_1_SH

Figure_1_Both <- x |> filter(age_group=="ALL") |> 
     filter(data_source!="AUS DOH", data_source!="FluNet - Sentinel") |> 
     filter(country!="UK" | year!=2020 | week<15) |> 
        ggplot()+
        geom_col(mapping=aes(start_date, hsp_x, fill='Total'))+
        geom_line(mapping = aes(start_date, hsp_rate_flu, color="Influenza")) +
        geom_line(mapping = aes(start_date, hsp_rate_rsv, color="RSV")) +
        geom_line(mapping=aes(start_date,hsp_rate_covid19, color='SARS_CoV_2')) +
        scale_color_manual("",
                           breaks = c('Influenza','RSV','SARS_CoV_2', 'Total'),
                           values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue'))+
        scale_fill_manual('',breaks=c('Total'),values=c('Total'='grey80')) +
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
     labs(title="Total hospitalizations and hospitalizations by virus in countries of Northern and Southern hemisphere, seasons 2016-2019 and 2022-23",
          subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)", caption="")

Figure_1_Both


ggsave(
        'output/Fig 01 - Hospitalization rates per 100k/Figure_1_NH_20220126.png',
        Figure_1_NH,
        width=16,
        height=9
)


ggsave(
        'output/Fig 01 - Hospitalization rates per 100k/Figure_1_SH_20220126.png',
        Figure_1_SH,
        width=16,
        height=9
)

ggsave(
        'output/Fig 01 - Hospitalization rates per 100k/Figure_1_Both_20220126.png',
        Figure_1_Both,
        width=16,
        height=9
)

