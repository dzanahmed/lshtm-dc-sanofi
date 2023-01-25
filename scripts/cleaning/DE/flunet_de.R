# import WHO FluNet FluID RSV data and collate into one dataset for 
  # Germany

# CURRENTLY WORKING ON

library(tidyverse)

#----------------------------------------------------------------
# import FluID and FluNet data
# FluID
VIW_FID_EPI <- read_csv("data/raw_data/FluNet/VIW_FID_EPI.csv")

# FluID has numbers on number of total SARI hospitalizations from 2022 onwards

# FluNet
VIW_FNT <- read_csv("data/raw_data/FluNet/VIW_FNT.csv")

# FluNET has influenza and RSV numbers from a monitored ~1% of the population of Germany

#----------------------------------------------------------------
# Germany

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

years <- c(2016:2019, 2022,2023)

germanyPopulation <- read_csv("data/raw_data/Countries_population.csv") 

germanyPopulation <- germanyPopulation |> filter(Code=="DE")

germany_fluID <-
  VIW_FID_EPI |> filter(COUNTRY_CODE == 'DEU' &
                          MMWR_YEAR %in% years) |> select(where(not_all_na)) #|> 
  # select(COUNTRY_AREA_TERRITORY,
  #        MMWR_WEEKSTARTDATE,
  #        MMWR_YEAR,
  #        MMWR_WEEK, 
  #        ORIGIN_SOURCE,
  #        AGEGROUP_CODE,
  #        SARI_INPATIENTS,
  #        SARI_POP_COV)
  

  germany_fluNET <-
  VIW_FNT |> filter(COUNTRY_CODE == 'DEU' &
                      MMWR_YEAR %in% years) |> select(where(not_all_na)) # |> select(
                      #   HEMISPHERE,
                      #   COUNTRY_CODE,
                      #   COUNTRY_AREA_TERRITORY,
                      #   MMWR_WEEKSTARTDATE,
                      #   MMWR_YEAR,
                      #   MMWR_WEEK,
                      #   ORIGIN_SOURCE,
                      #   SPEC_PROCESSED_NB,
                      #   RSV,
                      #   INF_A,
                      #   INF_B,
                      #   INF_ALL
                      # )


de_data <- merge(germany_fluNET, germany_fluID, by=c("MMWR_WEEKSTARTDATE",
                                          "MMWR_YEAR",
                                          "MMWR_WEEK"), all.x=TRUE)


 germany_covid_age <- read_csv(file="data/raw_data/DE/COVID/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")
 germany_covid_age <- germany_covid_age |> filter(Bundesland=="Bundesgebiet" & Datum>"2021-12-31")

# This is possibly unnecessary since filter(Altersgruppe=="00+") extracts the same
germany_covid_total <- read_csv(file = "data/raw_data/DE/COVID/Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv")

germany_covid_total <-  germany_covid_total |> filter(Bundesland == "Bundesgebiet" & Datum > "2021-12-31")


# |>
#   select(
#     Datum,
#     Altersgruppe,
#     aktualisierte_7T_Hospitalisierung_Faelle,
#     aktualisierte_7T_Hospitalisierung_Inzidenz,
#     Bevoelkerung
#   )




germany_fid <- VIW_FID_EPI %>% 
  filter(COUNTRY_CODE=='DEU' & ((MMWR_YEAR >= 2016 & MMWR_YEAR < 2020) | MMWR_YEAR == 2022 | MMWR_YEAR==2023)) %>% 
  select(HEMISPHERE, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK, AGEGROUP_CODE, SARI_INPATIENTS)

germany_fid <- germany_fid[order(germany_fid$MMWR_WEEKSTARTDATE),]
# SARI inpatients is blank for all values -- ignore this dataset

germany_fnt <- VIW_FNT %>% 
  filter(COUNTRY_CODE=='DEU' & ((MMWR_YEAR >= 2016 & MMWR_YEAR < 2020) | MMWR_YEAR == 2022| MMWR_YEAR==2023)) %>% 
  select(HEMISPHERE, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK, ORIGIN_SOURCE, SPEC_PROCESSED_NB, RSV, INF_A, INF_B, INF_ALL)

germany_fnt <- germany_fnt[order(germany_fnt$MMWR_WEEKSTARTDATE),]

#standardize var names according to data template
france_fnt$data_source <- "fluNet"

france_fnt <- france_fnt %>% 
              rename(hemisphere = HEMISPHERE,
                     country = COUNTRY_AREA_TERRITORY, 
                     week = MMWR_WEEK,
                     year = MMWR_YEAR,
                     hsp_abs_flu = INF_ALL,
                     hsp_abs_rsv = RSV,
                     subtype_a_abs = INF_A,
                     subtype_b_abs = INF_B,
                     origin_source = ORIGIN_SOURCE)

# include population to calculate rate per 100,000
france_fnt$denominator <- ifelse(france_fnt$year==2016, 66688564,
                                ifelse(france_fnt$year==2017,66883321,
                                       ifelse(france_fnt$year==2018, 67125071,
                                              ifelse(france_fnt$year==2019, 67356052,
                                                     ifelse(france_fnt$year==2022, 67885000, NA))))) 

#france population data
# 2016: 66688564
# 2017: 66883321
# 2018: 67125071
# 2019: 67356052 #(https://www.insee.fr/en/statistiques/serie/001641584)
# 2022: 67885000 #average of the estimated population at the beginning of each month in 2022 (https://www.insee.fr/en/statistiques/serie/001641607)

# calculate rate per 100,000
# RSV rate
france_fnt$hsp_rate_rsv = (france_fnt$hsp_abs_rsv / france_fnt$denominator)*100000

# Flu rate
france_fnt$hsp_rate_flu = (france_fnt$hsp_abs_flu / france_fnt$denominator)*100000

# reorder vars
var_order <- c("data_source", "country", "hemisphere", "week", "year", "origin_source", "hsp_rate_flu", "hsp_rate_rsv", "hsp_abs_flu", "hsp_abs_rsv", "denominator", "subtype_a_abs", "subtype_b_abs")
france_fnt <- france_fnt[,var_order]

# save and export
write_csv(france_fnt, file = "france_fluNet.csv")