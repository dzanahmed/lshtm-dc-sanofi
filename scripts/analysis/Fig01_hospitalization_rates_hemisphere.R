# Overall hospitalization by virus (Influenza, RSV, COVID-19) for the NH and SH 
# (complete time series for full time period: 2016 - 2023)
# Design: line graph + column graph for totals
# X-axis: 2016 - 2023 with a second x-axis label that identifies the seasons

# install.packages("ggbreak")
# install.packages("patchwork")

# INIT --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(stringr)

library(ggbreak)
library(patchwork) 

data <- read.csv(file='data/merged_data/merged_data.csv')
epi_weeks <- read_csv(file="data/epi_weeks.csv")

data$epi_dates <- as.Date(data$epi_dates)

# Consider deleting -------------------------------------------------------

#  Modification for France - VESTIGIAL
# #
# data <- data |> mutate(hsp_rate_flu =
#                                case_when(country == "FR" ~ (hsp_abs_flu *100000 /(68000000 * 0.3)),
#                                          TRUE ~ hsp_rate_flu)) |>
#         mutate(hsp_rate_rsv =
#                        case_when(country == "FR" ~ (hsp_abs_rsv *100000 /(68000000 * 0.3)),
#                                  TRUE ~ hsp_rate_rsv))


# Data preparation --------------------------------------------------------

space<-strrep(" ", 30) # Spacing for labeled titles (Seasons)
years <- 2016:2022

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
    "2017-05-13", # end 16-17
    "2017-09-25", # beginning 17-18
    "2018-05-13", # end 17-18
    "2018-09-25", # beginning 18-19
    "2019-05-13", # ending 18-19
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
    y = 151 # Y label position
  ) |> mutate(label = paste(A, B, as.character(C), D, as.character(E), F))


## Total hospitalizations in the NH - overall and by virus -----------------

Figure_1_NH <- Total_hosp_NH |> ggplot() +
  geom_col(mapping = aes(epi_dates, total_hsp_rate, fill='Total hospitalisations'), data = Total_hosp_NH) +
  geom_line(mapping = aes(epi_dates, total_hsp_rate, color = Virus), linewidth=1, data = Total_hosp_virus_NH, alpha=0.8) +
  scale_x_date(
    date_breaks = "1 months",  # labels for every month
    date_labels = '%b' # Short month Name
  ) +
 scale_x_break(breaks = NH_breaks)+ # This is ggbreak, breaks up from specified vector
  scale_y_continuous(breaks=seq(0,153,20), limits=c(0,153))+ # Organize y axis
  scale_colour_manual("", values=c("#ff880d", "#008000", "#00337C"))+
  scale_fill_manual("", breaks=c('Total hospitalisations'), values=c("Total hospitalisations"="#bee3ff"))+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.y.left = element_text(hjust=0.5),
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
    y = "Hospitalisations per\n 100,000 persons"
  ) +
  geom_label(mapping = aes(x, y, label = label, fontface='bold'), NH_seasons_geom, fill="white", size=4,
             label.padding=unit(0.5, "lines")) # Provide season labels
  

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

# Create breaks for X axis for SH
SH_breaks <-
  c(
    "2016-11-01", # end 17
    "2017-03-22", # beginning 17
    "2017-11-01", # end 17
    "2018-03-22", # beginning 18
    "2018-11-01", # end 18
    "2019-03-22", # beginning 18
    "2019-11-01", # ending 18
    "2022-03-22"  # beginning 22-23
  )

# Labels for SH seasons
SH_seasons_geom <-
  data.frame(
    A = space,
    B = "Season",
    C = years,
    F = space,
    x = seq.Date(as.Date("2016-07-15"), as.Date("2022-07-15"), by = "1 year"), # X label position
    y = 87 # Y label position
  ) |> mutate(label = paste(A, B, as.character(C), F))


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
  scale_y_continuous(breaks=seq(0,80,20), limits=c(0,88))+ # Organize y axis
  scale_colour_manual("", 
                      values=c("#ff880d", "#008000", "#00337C"))+
  scale_fill_manual("", breaks=c('Total hospitalisations'), values=c("Total hospitalisations"="#bee3ff"))+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.y.left = element_text(hjust=0.5),
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
    y = "Hospitalisations per\n 100,000 persons"
  ) +
  geom_label(mapping = aes(x, y, label = label, fontface='bold'), SH_seasons_geom, fill="white", size=4,
             label.padding=unit(0.6, "lines")) # Provide season labels

Figure_1_SH

# Plot to PNG (NH / SH )-----------------------------------------------------

ggsave(
  'output/Fig 01 - Hospitalization rates per 100k/Figure_1_NH.png',
  Figure_1_NH,
  width=12,
  height=4.5
)

ggsave(
  'output/Fig 01 - Hospitalization rates per 100k/Figure_1_SH.png',
  Figure_1_SH,
  width=12,
  height=4.5
)

# Use Patchwork lib to merge these into one graph  -------------------------

# Add margins to prepare for aligned patching
Figure_1_A <- Figure_1_NH +
  labs(title = "Northern Hemisphere") +
  theme(
    plot.title = element_text(face = 'bold', size = 14, hjust = 0.027),
    legend.position = "right",
    legend.text = element_text(size = 11.5),
    legend.margin = margin(l = 23, r = 25)
  )

Figure_1_B <- Figure_1_SH +
  labs(title = "Southern Hemisphere") +
  theme(
    plot.title = element_text(
      face = 'bold',
      size = 14,
      hjust = 0.023
    ),
    legend.position = "none"
  )

# Patched Figure (Both hemispheres)
Figure_1_Both <- (Figure_1_A/Figure_1_B)+
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = 'bold', size = 16))

# View
Figure_1_Both

## Plot to PNG (NH+SH) ----------------------------------------------------

ggsave(
  'output/Fig 01 - Hospitalization rates per 100k/Figure_Both_Presentation.png',
  Figure_1_Both,
  width=14,
  height=7.5
)