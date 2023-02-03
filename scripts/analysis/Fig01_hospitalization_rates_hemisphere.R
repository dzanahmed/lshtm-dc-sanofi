# Overall hospitalization by virus (Influenza, RSV, COVID-19) for the NH and SH 
# (complete time series for full time period: 2016 - 2023)
# Design: line graph
# X-axis: 2016 - 2023 with a second x-axis label that identifies the seasons

# install.packages("ggbreak")
# install.packages("patchwork")


# INIT --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(stringr)

library(RColorBrewer)

library(ggbreak)
library(patchwork) # Not sure if necessary after updates

data <- read.csv(file='data/merged_data/merged_data.csv')
epi_weeks <- read_csv(file="data/epi_weeks.csv")

data$epi_dates <- as.Date(data$epi_dates)



# Consider deleting -------------------------------------------------------

#  Modification for France - not sure if applicable
# #
# data <- data |> mutate(hsp_rate_flu =
#                                case_when(country == "FR" ~ (hsp_abs_flu *100000 /(68000000 * 0.3)),
#                                          TRUE ~ hsp_rate_flu)) |>
#         mutate(hsp_rate_rsv =
#                        case_when(country == "FR" ~ (hsp_abs_rsv *100000 /(68000000 * 0.3)),
#                                  TRUE ~ hsp_rate_rsv))


# Data preparation --------------------------------------------------------


## Preparation for graphs by hemispheres ------------------------------------

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

Total_hosp_virus <-
  data |> filter(data_source != "FluNet - Sentinel") |> # Drop FR subset of data
  select(country:hsp_rate_covid19, epi_dates) |> filter(age_group == "ALL") |>
  pivot_longer(
    cols = c("hsp_rate_flu", "hsp_rate_rsv", "hsp_rate_covid19"),
    names_to = "pathogen",
    values_to = "hsp_rate_pathogen"
  ) |>
  mutate(
    Virus = case_when(
      pathogen == "hsp_rate_flu" ~ "Influenza",
      pathogen == "hsp_rate_rsv" ~ "RSV",
      pathogen == "hsp_rate_covid19" ~ "SARS-CoV-2"
    )
  ) |>
  group_by(hemisphere, Virus, epi_dates) |>
  summarise(total_hsp_rate = sum(hsp_rate_pathogen, na.rm = TRUE))


### Preparation of data for NH -----------------------------------------

space<-strrep(" ", 15) # Spacing for labeled titles (Seasons)
years <- 2016:2022

# Filter on NH, exclude dates outside range, remove 0 values from SARS-CoV-2 entries

Total_hosp_NH <-
  Total_hosp |> filter(hemisphere == "NH" 
                       & epi_dates > "2016-09-25" 
                       & epi_dates < "2023-05-11") 


Total_hosp_virus_NH <-
  Total_hosp_virus |> filter(hemisphere == "NH"
                             & epi_dates > "2016-09-25"
                             & epi_dates < "2023-02-05") |>
  filter(
    Virus == "RSV" |
      Virus == "Influenza" |
      (Virus == "SARS-CoV-2" & epi_dates > "2021-12-31")
  )

# Empty row to show the rest of the 22/23 season
Total_hosp_NH[nrow(Total_hosp_NH) + 1, ] <-
  list("NH", as.Date("2023-05-11"), 0)

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

# Labels for NH seasons

NH_seasons_geom <-
  data.frame(
    A = space,
    B = "Season",
    C = years,
    D = "/",
    E = years + 1,
    F = space,
    x = seq.Date(as.Date("2017-01-18"), as.Date("2023-01-18"), by = "1 year"), # X label position
    y = 149 # Y label position
  ) |> mutate(label = paste(A, B, as.character(C), D, as.character(E), F))


## Total hospitalizations in the NH - overall and by virus -----------------

Figure_1_NH <- Total_hosp_NH |> ggplot() +
  geom_col(mapping = aes(epi_dates, total_hsp_rate, fill='Total hospitalisations'), data = Total_hosp_NH) +
  geom_line(mapping = aes(epi_dates, total_hsp_rate, color = Virus), linewidth=1, data = Total_hosp_virus_NH, alpha=0.8) +
  scale_x_date(
    date_breaks = "1 months",    # labels for every month
    date_labels = '%b' # Short month Name
  ) +
 scale_x_break(breaks = NH_breaks)+ # This is ggbreak, breaks up from specified vector
  scale_y_continuous(breaks=seq(0,150,20))+ # Organize y axis
  #scale_colour_viridis_d(option="B")+
 # scale_color_brewer(palette="Set2")+
  scale_colour_manual("", 
                      #breaks=c("SARS-CoV-2", "Influenza", "RSV"), 
                      values=c("#ff880d", "#008000", "#00337C"))+
  scale_fill_manual("", breaks=c('Total hospitalisations'), values=c("Total hospitalisations"="#bee3ff"))+
  theme_bw() +
  theme(
    #legend.position = "left",
    plot.title=element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5),
    axis.title.y.left = element_text(hjust=0.7),
    # Styling of X axis
    axis.line.x = element_line(colour = 'black', size = 1),
    axis.ticks.x = element_line(colour = 'black', size = 1),
    # These are fixes to remove double axis introduced by scale x break
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.line.x.top = element_blank(),
    axis.title.x.bottom = element_blank()
    ) +
  labs(
    # title = "Hospitalisation rate in the Northern hemisphere, seasons 2016-2019 and 2022-23",
    # subtitle = "Total hospitalisation rate and hospitalisations by virus",
    x = "Time",
    y = "Hospitalisations per 100,000 persons"
  ) +
  geom_label(mapping = aes(x, y, label = label), NH_seasons_geom, fill="grey80", size=4,
             label.padding=unit(0.5, "lines")) # Provide season labels
  
Figure_1_NH

### Preparation of data for SH ----------------------------------------------

Total_hosp_SH <-
  Total_hosp |> filter(hemisphere == "SH" 
                        & epi_dates > "2016-03-22" 
                        & epi_dates < "2022-11-01"
                       )


Total_hosp_virus_SH <-
  Total_hosp_virus |> filter(hemisphere == "SH"
                             & epi_dates > "2016-03-22"
                             & epi_dates < "2022-11-01"
                             ) |>
  filter(
    Virus == "RSV" |
      Virus == "Influenza" |
      (Virus == "SARS-CoV-2" & epi_dates > "2021-12-31")
  )

# Empty row to show the rest of this 22/23 season
#Total_hosp_SH[nrow(Total_hosp_SH) + 1, ] <-
 # list("SH", as.Date("2023-05-15"), 0)



# Create breaks for X axis for SH
SH_breaks <-
  c(
    "2017-11-01", # end 16-17
    "2018-03-22", # beginning 17-18
    "2018-11-01", # end 17-18
    "2019-03-22", # beginning 18-19
    "2019-11-01", # ending 18-19
    "2022-03-22"  # beginning 22-23 
  )

# Labels for NH seasons
SH_seasons_geom <-
  data.frame(
    A = space,
    B = "Season",
    C = years,
    F = space,
    x = seq.Date(as.Date("2016-07-15"), as.Date("2022-07-15"), by = "1 year"), # X label position
    y = 75 # Y label position
  ) |> mutate(label = paste(A, B, as.character(C), F))


# Create breaks for X axis for NH
SH_breaks <-
  c(#"2016-03-22", # beginning 16
    "2016-11-01", # end 17
    "2017-03-22", # beginning 17
    "2017-11-01", # end 17
    "2018-03-22", # beginning 18
    "2018-11-01", # end 18
    "2019-03-22", # beginning 18-19
    "2019-11-01", # ending 18-19
    "2022-03-22"  # beginning 22-23 
    #"2022-11-01"   # end  22-23
    #"2023-03-22"
  )

## Total hospitalizations in the NH - overall and by virus -----------------

Figure_1_SH <- Total_hosp_SH |> ggplot() +
  geom_col(mapping = aes(epi_dates, total_hsp_rate, fill='Total hospitalisations'), data = Total_hosp_SH) +
  geom_line(mapping = aes(epi_dates, total_hsp_rate, color = Virus), linewidth=1, data = Total_hosp_virus_SH, alpha=0.8) +
  scale_x_date(
    date_breaks = "1 months",    # labels for every month
    date_labels = '%b', # Short month Name
    limits = as.Date(c("2016-04-01", "2022-11-01"))
  ) +
  scale_x_break(breaks = SH_breaks)+ # This is ggbreak, breaks up from specified vector
  scale_y_continuous(breaks=seq(0,60,20), limits=c(0,75))+ # Organize y axis
  #scale_colour_viridis_d(option="B")+
  # scale_color_brewer(palette="Set2")+
  scale_colour_manual("", 
                      #breaks=c("SARS-CoV-2", "Influenza", "RSV"), 
                      values=c("#ff880d", "#008000", "#00337C"))+
  scale_fill_manual("", breaks=c('Total hospitalisations'), values=c("Total hospitalisations"="#bee3ff"))+
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title=element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5),
    axis.title.y.left = element_text(hjust=0.7),
    # Styling of X axis
    axis.line.x = element_line(colour = 'black', size = 1),
    axis.ticks.x = element_line(colour = 'black', size = 1),
    # These are fixes to remove double axis introduced by scale x break
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.line.x.top = element_blank(),
    axis.title.x.bottom = element_blank()
  ) +
  labs(
    # title = "Hospitalisation rate in the Northern hemisphere, seasons 2016-2019 and 2022-23",
    # subtitle = "Total hospitalisation rate and hospitalisations by virus",
    x = "Time",
    y = "Hospitalisations per 100,000 persons"
  ) +
  geom_label(mapping = aes(x, y, label = label), SH_seasons_geom, fill="grey80", size=4,
             label.padding=unit(0.6, "lines")) # Provide season labels


Figure_1_SH


# Plot to PNG -------------------------------------------------------------


ggsave(
  'output/Fig 01 - Hospitalization rates per 100k/Figure_1_NH.png',
  Figure_1_NH,
  width=12,
  height=5
)


ggsave(
  'output/Fig 01 - Hospitalization rates per 100k/Figure_1_SH.png',
  Figure_1_SH,
  width=12,
  height=5
)


# Patchwork experiment ----------------------------------------------------

x <- Figure_1_NH / Figure_1_SH + plot_annotation(tag_levels = 'A')


x

ggsave(
  'output/Fig 01 - Hospitalization rates per 100k/Figure_X.png',
  x,
  width=12,
  height=8.5
)

# Old SH ---------------------------------------------------------------

Figure_1_SH <- Total_hosp_SH |> ggplot() +
  geom_col(mapping = aes(epi_dates, total_hsp_rate, fill='Total hospitalisations'), data = Total_hosp_SH) +
  geom_line(mapping = aes(epi_dates, total_hsp_rate, color = Virus), linewidth=1, data = Total_hosp_virus_SH) +
  scale_x_date(
    date_breaks = "1 months",    # labels for every month
    date_labels = '%b' # Short month Name
  ) +
  scale_x_break(breaks = SH_breaks)+ # This is ggbreak, breaks up from specified vector
  scale_y_continuous(breaks=seq(0,80,20), limits = c(0,70))+ # Organize y axis
  scale_color_brewer(palette = "Set2") +
  scale_fill_manual("", breaks=c('Total hospitalisations'), values=c("Total hospitalisations"="grey80"))+
  theme_bw() +
  theme(
    legend.position = "bottom", 
    plot.title=element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5),
    # Styling of X axis
    axis.line.x = element_line(colour = 'black', size = 1),
    axis.ticks.x = element_line(colour = 'black', size = 1),
    # These are fixes to remove double axis introduced by scale x break
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.line.x.top = element_blank(),
    axis.title.x.bottom = element_blank()
  ) +
  labs(
    title = "Hospitalisation rate in the Southern hemisphere, seasons 2016-2019 and 2022-23",
    subtitle = "Total hospitalisation rate and hospitalisations by virus",
    x = "Time",
    y = "Hospitalisations per 100,000 people"
  ) +
  geom_text(mapping = aes(x, y, label = SH_season_labels), SH_seasons_geom) # Provide season labels


Figure_1_SH



# Drop after this ---------------------------------------------------------



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