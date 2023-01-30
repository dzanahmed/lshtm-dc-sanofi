# install.packages("ggbreak")
# install.packages("patchwork")

library(tidyverse)
library(lubridate)

library(ggbreak)
library(patchwork)

data <- read.csv(file='data/merged_data/merged_data.csv')

# Overall hospitalization by virus (Influenza, RSV, COVID-19) for the NH and SH 
# (complete time series for full time period: 2016 - 2023)
# Design: line graph
# X-axis: 2016 - 2023 with a second x-axis label that identifies the seasons

data$epi_dates <- as.Date(data$epi_dates)

date_breaks <- data$epi_dates
epi <- data$week

breaks_mo <- seq(min(data$epi_dates), max(data$epi_dates), by="4 months")


# X-axis alternative
# scale_x_date(
#         date_breaks = "4 weeks",
#         date_minor_breaks = "1 week",
#         date_labels = "%b %Y"
# ) +
#         theme(axis.text.x = element_text(
#                 angle = 90,
#                 vjust = 0.5,
#                 hjust = 1
#         ))

#  Modification for France - not sure if applicable
# #
# data <- data |> mutate(hsp_rate_flu =
#                                case_when(country == "FR" ~ (hsp_abs_flu *100000 /(68000000 * 0.3)),
#                                          TRUE ~ hsp_rate_flu)) |>
#         mutate(hsp_rate_rsv =
#                        case_when(country == "FR" ~ (hsp_abs_rsv *100000 /(68000000 * 0.3)),
#                                  TRUE ~ hsp_rate_rsv))


epi_weeks <- read_csv(file="data/epi_weeks.csv")

NH_seasons <- data.frame(season=c("2016-2017","2017-2018","2018-2019", "2019-2020", "2021-2022", "2022-2023"),
                         beginning=c("2016-10-02", "2017-10-01","2018-09-30","2019-09-29", "2021-10-01", "2022-10-02"),
                         end=c("2017-05-14","2018-05-13","2019-05-12","2020-05-11","2022-05-15", "2023-02-15")) |> 
    mutate(beginning=as_date(beginning), end=as_date(end))




Figure_01_data <-
    data |> select(data_source:hsp_rate_covid19, epi_dates) |>
    mutate(hsp_rate = ifelse(is.na(hsp_rate_flu) == TRUE, 0, hsp_rate_flu)+
               ifelse(is.na(hsp_rate_rsv) == TRUE, 0, hsp_rate_rsv)+
               ifelse(is.na(hsp_rate_covid19) == TRUE, 0, hsp_rate_covid19)
    )

# y <- Figure_01_data |> filter(country=="UK", year=="2018") |> 
#         mutate(hsp_rate_flu==ifelse(week), 
#                hsp_rate_rsv, 
#                hsp_rate_covid19)


Figure_1_NH <- Figure_01_data |> filter(hemisphere=='NH', age_group=="ALL", data_source!="FluNet - Sentinel") |> 
    ggplot()+
    geom_col(mapping=aes(epi_dates, hsp_rate, fill="Total hospitalizations"))+
    geom_line(mapping = aes(epi_dates, hsp_rate_flu, color="Influenza")) +
    geom_line(mapping = aes(epi_dates, hsp_rate_rsv, color="RSV")) +
    geom_line(mapping=aes(epi_dates,hsp_rate_covid19,color='SARS_CoV_2')) +
    scale_fill_manual("", breaks=c('Total hospitalizations'), values=c("Total hospitalizations"="grey80"))+
    scale_color_manual("", 
                       breaks = c('Influenza','RSV','SARS_CoV_2'),
                       values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue'))+
    scale_x_date(date_labels="%b", date_breaks="month", expand=c(0,0)) +
    facet_grid(country ~ year(epi_dates), space="free_x", scales="free_x", switch="x") +
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
    labs(title="Influenza and RSV hospitalizations in the Northern hemisphere, seasons 2016-2019 and 2022-23",
         subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)")

Figure_1_NH


# Experiment --------------------------------------------------------------

format_dates <- function(x) {
    months <- strftime(x, format = "%b")  |>   # Abbreviated name of the month.
        #stringr::str_to_upper() |>              # May or may not be needed depending on your locale.
        stringr::str_sub(start = 1, end = 3)              # Abbreviated name of the month.
    years <- lubridate::year(x)                       # Year as a 4-digit number.
    if_else(is.na(lag(years)) | lag(years) != years,  # Conditions for pasting.
            true = paste(months, years, sep = "\n"), 
            false = months)
}

format_dates(as.Date(c("2018-09-30", "2018-12-31", "2019-03-31", "2019-06-30")))


Figure_01_data |> filter(hemisphere=='NH', age_group=="ALL", data_source!="FluNet - Sentinel") |> 
    filter(epi_dates < "2019-12-31" | epi_dates>"2022-01-01") |> 
    ggplot()+
    geom_line(aes(epi_dates, hsp_rate_flu))+
    scale_x_date(date_breaks = "2 months", 
                 minor_breaks = NULL,
                 #expand=expand_scale(add=15),
                 labels = format_dates)+
    facet_grid(country ~ .)+
    theme(axis.text.x = element_text(hjust = -0.1),  
          axis.title = element_blank())+
    scale_x_cut(breaks=c("2019-12-31","2022-01-01"), which=c(1,2,3), scales=c(3.5, 0, 1), space=0.1)


epiweek("2019-12-31")

Figure_01_data |> filter(hemisphere == 'NH',
                         age_group == "ALL",
                         data_source != "FluNet - Sentinel") |>
    ggplot() +
    geom_histogram(mapping = aes(epi_dates, y = hsp_rate_flu),
                   stat = "identity") +
    scale_x_date(
        date_breaks = "3 months",
        # labels every 2 months
        date_minor_breaks = "1 month",
        # gridlines every month
        date_labels = '%b\n%Y'
    ) +       #labeled by month with year below
    # Choose color palette (uses RColorBrewer package)
    scale_fill_brewer(palette = "Pastel2") +
    facet_grid(country ~ .)+
    theme_minimal() +
    labs(x = "Week of onset",
         y = "Weekly case incidence",
         fill = "Hospital",
         title = "Weekly case incidence, from aggregated count data by hospital")+
    scale_x_break(
             breaks = c("2019-12-31", "2022-01-01")
             # which = c(1, 2, 3),
             # scales = c(3.5, 0, 1),
             # space = 0.1
         )



# This can be ignored for now

x <- Figure_01_data |> filter(hemisphere=='NH', age_group=="ALL", data_source != "FluNet - Sentinel") |> 
    select(country:hsp_rate_covid19, epi_dates, hsp_rate) |> 
  # select(-hsp_rate) |> 
    pivot_longer(cols = c("hsp_rate_flu", "hsp_rate_rsv", "hsp_rate_covid19"), names_to = "pathogen",
                 values_to = "hsp_rate_pathogen") |> 
    mutate(pathogen = case_when(pathogen=="hsp_rate_flu"~"Influenza",
                                pathogen=="hsp_rate_rsv"~"RSV",
                                pathogen=="hsp_rate_covid19"~"COVID-19")) |> 
    mutate(hsp_rate = hsp_rate_pathogen |> summarize(sum(na.rm=TRUE)))


#|> 
   # mutate(hsp_rate= group_by(country, week, year) |> summarize(sum(hsp_rate_pathogen, na.rm=TRUE)))



x |> group_by(week, year) |> summarize(px=sum(hsp_rate_pathogen, na.rm=TRUE))

# This is the best one so far - prefer this for hospitalizations. 
# More thinking on colors, axis labeling (any way to solve years + months?)
# Annotation for seasons
# Annotation for countries


# !! DONT FORGET THIS
# Main message here: Total hospitalizations in the hemisphere
# Create one with a histogram, and one with total as columns+lines 
# Break data between seasons

x |> filter(epi_dates>"2016-08-08") |> ggplot()+
    geom_rect(mapping=aes(xmin=beginning, xmax=end, ymin=-Inf, ymax=Inf), data=NH_seasons, alpha=0.05, fill="#00bb00")+
    geom_histogram(mapping = aes(epi_dates, y = hsp_rate_pathogen, fill=pathogen),
                   stat = "identity") +
    scale_x_date(
        date_breaks = "2 months",
        # labels every 2 months
        date_minor_breaks = "1 month",
        # gridlines every month
        date_labels = '%b\n%y'
    ) +       #labeled by month with year below
    # Choose color palette (uses RColorBrewer package)
    scale_fill_brewer(palette = "Set2") +
    facet_grid(country ~ .)+
    theme_minimal() +
    scale_x_break(
        breaks = c("2019-05-01", "2022-01-01")
        # which = c(1, 2, 3),
        # scales = c(3.5, 0, 1),
        # space = 0.1
    )+
    theme(strip.placement = "outside",
          strip.background = element_rect(fill="grey90"),
          panel.spacing=unit(0.1,"cm"))+
    theme(axis.text.x = element_text(
        angle = 0,
        vjust = 2.2,
        hjust = 1
    ), 
    plot.title=element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))+
    theme(legend.position = 'bottom')+
    labs(title="Influenza and RSV hospitalizations in the Northern hemisphere, seasons 2016-2019 and 2022-23",
         subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)")+
    
    # REMOVE TOP AXIS
    theme(axis.text.x.top = element_blank(),
          axis.ticks.x.top = element_blank(),
          axis.line.x.top = element_blank())+
    
    # Not bad, needs vectorization and removal of top bold row
    
    annotate("text", x=ymd('2017-01-26'), y=50, 
             label = 'atop(bold("Season"),"2016/2017")',
             colour = "#66c2a5", parse = TRUE)+
    
    annotate("text", x=ymd('2018-01-25'), y=50, 
             label = 'atop(bold("Season"),"2017/2018")',
             colour = "#66c2a5", parse = TRUE)+
    
    annotate("text", x=ymd('2019-01-24'), y=50, 
             label = 'atop(bold("Season"),"2018/2019")',
             colour = "#66c2a5", parse = TRUE)+
    
    
    annotate("text", x=ymd('2022-03-01'), y=50, 
             label = 'atop(bold("Season"),"2021/2022")',
             colour = "#66c2a5", parse = TRUE)
    
    # annotate(geom = "rect", xmin = ymd('2016-04-02'), xmax = ymd('2016-06-06'), ymin = -Inf, ymax = Inf,
    #          fill = "grey", alpha = 0.15)+
    # geom_rect(mapping=aes(xmin="2016-10-02", xmax="201"))
    

# Not bad for complete display of situation in northern hemisphere, 

x |> filter(epi_dates>"2016-08-08") |> 
    filter(epi_dates < "2019-07-31" | epi_dates>"2021-12-31", country=="DE") |> 
    ggplot()+
    geom_rect(mapping=aes(xmin=beginning, xmax=end, ymin=-Inf, ymax=Inf), data=NH_seasons, alpha=0.05, fill="#00bb00")+
    geom_line(mapping=aes(epi_dates, hsp_rate_pathogen, color=pathogen))+
    #geom_histogram(mapping = aes(epi_dates, y = hsp_rate_pathogen, fill=pathogen), stat = "identity") +
    scale_x_date(date_breaks = "2 months", 
                         minor_breaks = NULL,
                         #expand=expand_scale(add=15),
                         labels = format_dates,
                 position ="bottom",
                 sec.axis = dup_axis()
    ) +       #labeled by month with year below
    # Choose color palette (uses RColorBrewer package)
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    scale_x_break(
        breaks = c("2019-07-31", "2021-12-31")
    #     which = c(1, 2, 3),
    # scales = c(3.5, 0, 1),
    #     space = 0.1
    )+
    theme(strip.placement = "outside",
          strip.background = element_rect(fill="grey90"),
          panel.spacing=unit(0.1,"cm"))+
    theme(axis.text.x = element_text(  hjust = 1), 
    plot.title=element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))+
    theme(legend.position = 'bottom',
          axis.line.x = element_line(colour = 'black', size = 1),
          axis.ticks.x = element_line(colour='black', size = 1))+
    labs(title="Influenza and RSV hospitalizations in the Northern hemisphere, seasons 2016-2019 and 2022-23",
         subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)")+
    theme(axis.text.x.top = element_blank(),
          axis.ticks.x.top = element_blank(),
          axis.line.x.top = element_blank())+
    
    # THIS NEEDS TO BE VECTORIZED
    
    # ANNOTATE EVERY SEASON WITH A DIFFERENT COLOR TO BE COHERENT IN THE NEXT GRAPHS
    
    annotate("text", x=ymd('2017-01-26'), y=95, 
             label = 'atop(bold("Season"),"2016/2017")',
             colour = "#66c2a5", parse = TRUE)+
    
    annotate("text", x=ymd('2018-01-25'), y=95, 
             label = 'atop(bold("Season"),"2017/2018")',
             colour = "#66c2a5", parse = TRUE)+
    
    annotate("text", x=ymd('2019-01-24'), y=95, 
             label = 'atop(bold("Season"),"2018/2019")',
             colour = "#66c2a5", parse = TRUE)+
    

    annotate("text", x=ymd('2022-03-01'), y=95, 
             label = 'atop(bold("Season"),"2021/2022")',
             colour = "#66c2a5", parse = TRUE)
    
# annotate(geom = "rect", xmin = ymd('2016-04-02'), xmax = ymd('2016-06-06'), ymin = -Inf, ymax = Inf,
#          fill = "grey", alpha = 0.15)+
# geom_rect(mapping=aes(xmin="2016-10-02", xmax="201"))





# Fig_01b starts here -----------------------------------------------------



Figure_1_SH <- Figure_01_data |> filter(hemisphere=='SH', age_group=="ALL") |> 
    ggplot()+
    geom_col(mapping=aes(epi_dates, hsp_rate, fill="Total hospitalizations"))+
    geom_line(mapping = aes(epi_dates, hsp_rate_flu, color="Influenza")) +
    geom_line(mapping = aes(epi_dates, hsp_rate_rsv, color="RSV")) +
    geom_line(mapping=aes(epi_dates,hsp_rate_covid19,color='SARS_CoV_2')) +
    scale_fill_manual("", breaks=c('Total hospitalizations'), values=c("Total hospitalizations"="grey80"))+
    scale_color_manual("", 
                       breaks = c('Influenza','RSV','SARS_CoV_2'),
                       values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue'))+
    scale_x_date(date_labels="%b", date_breaks="month", expand=c(0,0)) +
    facet_grid(country ~ year(epi_dates), space="free_x", scales="free_x", switch="x") +
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
    labs(title="Influenza and RSV hospitalizations in the Southern hemisphere, seasons 2016-2019 and 2022-23",
         subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)")

Figure_1_SH

Figure_1_Both <- Figure_01_data |> filter(age_group=="ALL") |> 
    filter(data_source!="AUS DOH", data_source!="FluNet - Sentinel") |> 
    filter(country!="UK" | year!=2020 | week<15) |> 
    ggplot()+
    geom_col(mapping=aes(epi_dates, hsp_rate, fill="Total hospitalizations"))+
    geom_line(mapping = aes(epi_dates, hsp_rate_flu, color="Influenza")) +
    geom_line(mapping = aes(epi_dates, hsp_rate_rsv, color="RSV")) +
    geom_line(mapping=aes(epi_dates,hsp_rate_covid19,color='SARS_CoV_2')) +
    scale_fill_manual("", breaks=c('Total hospitalizations'), values=c("Total hospitalizations"="grey80"))+
    scale_color_manual("", 
                       breaks = c('Influenza','RSV','SARS_CoV_2'),
                       values = c("Influenza"="red", "RSV"="orange",'SARS_CoV_2'='blue'))+
    scale_x_date(date_labels="%b", date_breaks="month", expand=c(0,0)) +
    facet_grid(country ~ year(epi_dates), space="free_x", scales="free_x", switch="x") +
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
         subtitle="Rates per 100,000", x="Time", y="Hospitalizations (n/100,000)")

Figure_1_Both


ggsave(
    'output/Fig 01 - Hospitalization rates per 100k/Figure_1_NH.png',
    Figure_1_NH,
    width=16,
    height=9
)


ggsave(
    'output/Fig 01 - Hospitalization rates per 100k/Figure_1_SH.png',
    Figure_1_SH,
    width=16,
    height=9
)

ggsave(
    'output/Fig 01 - Hospitalization rates per 100k/Figure_1_Both.png',
    Figure_1_Both,
    width=16,
    height=9
)

