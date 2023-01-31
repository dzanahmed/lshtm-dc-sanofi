# Overall hospitalization by virus (Influenza, RSV, COVID-19) for the NH and SH 
# (complete time series for full time period: 2016 - 2023)
# Design: line graph
# X-axis: 2016 - 2023 with a second x-axis label that identifies the seasons

# install.packages("ggbreak")
# install.packages("patchwork")


# INIT --------------------------------------------------------------------

library(tidyverse)
library(lubridate)

library(RColorBrewer)

library(ggbreak)
library(patchwork) # Not sure if necessary after updates

data <- read.csv(file='data/merged_data/merged_data.csv')
epi_weeks <- read_csv(file="data/epi_weeks.csv")

data$epi_dates <- as.Date(data$epi_dates)

# This is to create a table of season dates in the NH
NH_seasons <- epi_weeks |>
  filter(epi_wk_no == 20 | epi_wk_no == 40) |> 
  filter(row_number() != 1 & row_number() != 12) |>
  mutate(season_threshold = case_when(epi_wk_no == 40 ~ "Start",
                                      TRUE ~ "End")) |>
  pivot_wider(id_cols = year,
              names_from = season_threshold,
              values_from = epi_dates) |>
  mutate(year = as.character(paste0(year, "/", year + 1)))

# Shift End upward by one row to correct for merging on year
NH_seasons$End <- data.table::shift(NH_seasons$End, n = -1) 

# Drop 2023/2024
NH_seasons <- NH_seasons[1:nrow(NH_seasons) - 1,]

# Artificial ending of the season, for better plot rendering
NH_seasons[5,3] <- as.Date("2023-03-01")

br <- sort(c(NH_seasons$End, NH_seasons$Start))

br <-
  c(
    "2017-05-14", # end 16-17
    "2017-10-01", # beginning 17-18
    "2018-05-13", # end 17-18
    "2018-09-30", # beginning 18-19
    "2019-05-12", # ending 18-19
    "2022-10-02", # beginning 22-23
    "2023-03-01"
  )

# Consider deleting -------------------------------------------------------



#date_breaks <- data$epi_dates

# breaks_mo <- seq(min(data$epi_dates), max(data$epi_dates), by="4 months")


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


# Data preparation --------------------------------------------------------

Figure_01_data_pivoted <- data |> select(data_source:hsp_rate_covid19, epi_dates) |>
  mutate(hsp_rate = ifelse(is.na(hsp_rate_flu) == TRUE, 0, hsp_rate_flu)+
           ifelse(is.na(hsp_rate_rsv) == TRUE, 0, hsp_rate_rsv)+
           ifelse(is.na(hsp_rate_covid19) == TRUE, 0, hsp_rate_covid19)) |> 
  pivot_longer(cols = c("hsp_rate_flu", "hsp_rate_rsv", "hsp_rate_covid19"), names_to = "pathogen",
          values_to = "hsp_rate_pathogen") |> 
  filter(hemisphere=='NH', age_group=="ALL", data_source != "FluNet - Sentinel")


# This graph should show all hospitalizations as geom_col

# This graph should show all hospitalizations by virus as geom_line

# X-axis 2016-2019 (3 seasons) - consider implementing more breaks 
# X-axis 08/2022 - X/2023

head(Figure_01_data_pivoted)


# Preparation for graphs by hemispheres ------------------------------------

# Total_hosp extracts data and transforms hsp_rate for breakdown by week, aggregated for hemisphere
# Total_hosp_virus extracts data and transforms it for breakdown by week, aggregated for hemisphere and virus

Total_hosp <-
  data |> 
  select(country:hsp_rate_covid19, epi_dates) |> filter(age_group == "ALL") |>
  mutate(
    hsp_rate = ifelse(is.na(hsp_rate_flu) == TRUE, 0, hsp_rate_flu) +
      ifelse(is.na(hsp_rate_rsv) == TRUE, 0, hsp_rate_rsv) +
      ifelse(is.na(hsp_rate_covid19) == TRUE, 0, hsp_rate_covid19)
  ) |>
  group_by(hemisphere, epi_dates) |>
  summarise(total_hsp_rate = sum(hsp_rate, na.rm = TRUE))


Total_hosp_virus <- data |> filter(data_source!="FluNet - Sentinel") |> # Drop FR subset of data
  select(country:hsp_rate_covid19, epi_dates) |> filter(age_group == "ALL") |> 
  pivot_longer(cols = c("hsp_rate_flu", "hsp_rate_rsv", "hsp_rate_covid19"), names_to = "pathogen",
               values_to = "hsp_rate_pathogen") |> 
  mutate(Virus = case_when(pathogen=="hsp_rate_flu"~"Influenza",
                              pathogen=="hsp_rate_rsv"~"RSV",
                              pathogen=="hsp_rate_covid19"~"COVID-19")) |>
  group_by(hemisphere, Virus, epi_dates) |>
  summarise(total_hsp_rate = sum(hsp_rate_pathogen, na.rm = TRUE))


## Preparation of data for NH -----------------------------------------


# Filter on NH, exclude dates outside range, remove 0 values from COVID-19 entries

Total_hosp_NH <-
  Total_hosp |> filter(hemisphere == "NH" 
                       & epi_dates > "2016-09-25" 
                       & epi_dates < "2023-05-15")


Total_hosp_virus_NH <-
  Total_hosp_virus |> filter(hemisphere == "NH"
                             & epi_dates > "2016-09-14"
                             & epi_dates < "2023-05-15") |>
  filter(
    Virus == "RSV" |
      Virus == "Influenza" |
      (Virus == "COVID-19" & epi_dates > "2021-12-31")
  )

# Empty row to show the rest of this 22/23 season
Total_hosp_NH[nrow(Total_hosp_NH) + 1, ] <-
  list("NH", as.Date("2023-05-15"), 0)

# Create breaks for X axis for NH
NH_breaks <-
  c(
    "2017-05-14", # end 16-17
    "2017-09-25", # beginning 17-18
    "2018-05-13", # end 17-18
    "2018-09-25", # beginning 18-19
    "2019-05-12", # ending 18-19
    "2022-09-25"  # beginning 22-23 
  )

# Create Season labels for plots
NH_season_labels <-
  c(
    "Season 2016/2017",
    "Season 2017/2018",
    "Season 2018/2019",
    "Season 2019/2020",
    "Season 2020/2021",
    "Season 2021/2022",
    "Season 2022/2023"
  )

NH_seasonLabels <- data.frame(seq.Date(as.Date("2017-03-10"), as.Date("2023-03-10"), by="1 year"),116,NH_season_labels)


## Total hospitalizations in the NH - overall and by virus -----------------

Total_hosp_NH |> ggplot() +
  geom_col(mapping = aes(epi_dates, total_hsp_rate, fill='Total hospitalizations'), data = Total_hosp_NH) +
  geom_line(mapping = aes(epi_dates, total_hsp_rate, color = Virus), linewidth=1, data = Total_hosp_virus_NH) +
  scale_x_date(
    date_breaks = "1 months",    # labels every 2 months
    #date_minor_breaks = "1 week",    # gridlines every month
    date_labels = '%b' # Mon 'YR
  ) +
 scale_x_break(breaks = NH_breaks)+
  scale_y_continuous(breaks=seq(0,140,20))+
  scale_color_brewer(palette = "Set2") +
  scale_fill_manual("", breaks=c('Total hospitalizations'), values=c("Total hospitalizations"="grey80"))+
  theme_bw() +
  theme(
    legend.position = "bottom", 
    plot.title=element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5),
    axis.line.x = element_line(colour = 'black', size = 1),
    axis.ticks.x = element_line(colour = 'black', size = 1),
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.line.x.top = element_blank(),
    axis.title.x.bottom = element_blank()
    ) +
  labs(
    title = "Hospitalization rate in the Northern hemisphere, seasons 2016-2019 and 2022-23",
    subtitle = "Total hospitalization rate and breakdown by virus",
    x = "Time",
    y = "Hospitalizations (n/100,000)"
  )+
  geom_text(mapping = aes(x,y,label=NH_season_labels), NH_seasonLabels)
  

## Preparation of data for SH ----------------------------------------------


Total_hosp_SH <- Total_hosp |> filter(hemisphere == "SH")





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
                                pathogen=="hsp_rate_covid19"~"COVID-19"))


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
    geom_rect(mapping=aes(xmin=Start, xmax=End, ymin=-Inf, ymax=Inf), data=NH_seasons, alpha=0.05, fill="#00bb00")+
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

