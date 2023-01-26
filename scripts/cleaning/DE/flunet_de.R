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

# Setting up helpers

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

years <- c(2016:2019, 2022,2023)

germanyPopulation <- read_csv("data/raw_data/Countries_population.csv") 

germanyPopulation <- germanyPopulation |> filter(code=="DE")

source(file="scripts/cleaning/var_order_merged_csv.R")

epiweeks <- read_csv(file = 'data/epi_weeks.csv')

epiweeks <- epiweeks |> rename("Datum" = epi_dates, "week" = epi_wk_no)

# WHO Data: FluID + FluNet ------------------------------------------------

# Hospitalization rate for SARI for all age groups calculated here
germany_fluID <-
  VIW_FID_EPI |> filter(COUNTRY_CODE == 'DEU' &
                          MMWR_YEAR %in% years) |> select(where(not_all_na)) |>
  select(
    COUNTRY_AREA_TERRITORY,
    MMWR_WEEKSTARTDATE,
    MMWR_YEAR,
    MMWR_WEEK,
    ORIGIN_SOURCE,
    AGEGROUP_CODE,
    SARI_INPATIENTS,
    SARI_POP_COV
  ) |> drop_na() |>
  mutate(hsp_rate = SARI_INPATIENTS / SARI_POP_COV * 100000) |>
  mutate(AGEGROUP_CODE = toupper(AGEGROUP_CODE))

# FluNet has sentinel data. Drop all-NA columns, select what is necessary and filter
germany_fluNET <-
  VIW_FNT |> select(where(not_all_na)) |> select(
                        HEMISPHERE,
                        COUNTRY_CODE,
                        COUNTRY_AREA_TERRITORY,
                        MMWR_WEEKSTARTDATE,
                        MMWR_YEAR,
                        MMWR_WEEK,
                        ORIGIN_SOURCE,
                        SPEC_PROCESSED_NB,
                        RSV,
                        INF_A,
                        INF_B,
                        INF_ALL
                      ) |> filter(COUNTRY_CODE == 'DEU' &
                                 MMWR_YEAR %in% years & 
                                   ORIGIN_SOURCE=="SENTINEL")

# FluNet has aggregated age data, no age groups. Set to uppercase ALL to match FluID
germany_fluNET$AGEGROUP_CODE <- "ALL"

# Merge these two on week/year/agegroup
WHO_fluNET_fluID_data <- merge(
  germany_fluNET,
  germany_fluID,
  by = c(
    "MMWR_WEEKSTARTDATE",
    "MMWR_YEAR",
    "MMWR_WEEK",
    "AGEGROUP_CODE"
  ),
  all.x = TRUE,
  all.y = TRUE
) |> rename("age_group" = AGEGROUP_CODE,
            "week" = MMWR_WEEK,
            "year" = MMWR_YEAR)


# COVID data from RKI GitHub ----------------------------------------------

germany_covid_age <- read_csv(file="data/raw_data/DE/COVID/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")

germany_covid_age <- germany_covid_age |> filter(Bundesland=="Bundesgebiet" & Datum>"2022-01-01") |> arrange(Datum)

# Add weeks from epiweeks
germany_covid_age <- merge(epiweeks, germany_covid_age)

# Rename german variables
germany_covid_age <- germany_covid_age |> rename(
  "hsp_abs_covid" = `7T_Hospitalisierung_Faelle`,
  "hsp_rate_covid19" = `7T_Hospitalisierung_Inzidenz`,
  "age_group" = Altersgruppe,
  "MMWR_WEEKSTARTDATE" = Datum
) |>
  select(-Bundesland, -Bundesland_Id) |>
  mutate(
    age_group = case_when(
      age_group == "00+" ~ "ALL",
      age_group == "00-04" ~ "0TO4",
      age_group == "05-14" ~ "5TO14",
      # Other age groups are quite similar....
      TRUE ~ age_group
    )
  )


# Merge WHO data or Flu and RSV with RKI data on COVID -----------------------------------

WHO_RKI_Hosp_data <-
 merge(
   germany_covid_age,
   WHO_fluNET_fluID_data,
   by = c("year", "week", "age_group"),
   all.x = TRUE,
   all.y = TRUE
 )

# Trim the excess
WHO_RKI_Hosp_data <- WHO_RKI_Hosp_data |> select(
 -c(MMWR_WEEKSTARTDATE.x,
  MMWR_WEEKSTARTDATE.y,
 COUNTRY_AREA_TERRITORY.x,
 ORIGIN_SOURCE.x,
 ORIGIN_SOURCE.y,
 SPEC_PROCESSED_NB,
 COUNTRY_AREA_TERRITORY.y,
 SARI_POP_COV))
 
# Rename
WHO_RKI_Hosp_data <- WHO_RKI_Hosp_data |> rename(
 "hemisphere" = HEMISPHERE,
 "country" = COUNTRY_CODE,
 "hsp_abs_rsv" = RSV,
 "subtype_a_abs" = INF_A,
 "subtype_b_abs" = INF_B,
 "hsp_abs_flu" = INF_ALL,
 "hsp_abs" = SARI_INPATIENTS,
) 

# Add denominator / population data
WHO_RKI_Hosp_data <- WHO_RKI_Hosp_data |> left_join(germanyPopulation[,c("year", "population")], by=c("year"="year"))
WHO_RKI_Hosp_data <- WHO_RKI_Hosp_data |> rename("denominator"=population)

# Add fixed values for DE
# Calculate Hospitalization rates based on absolute numbers divided by denominator, 
# multiplied by 100 (1%), and 100,000 for rate

WHO_RKI_Hosp_data <- WHO_RKI_Hosp_data |> mutate(hemisphere = "NH",
  country = "DE",
  data_source = "WHO FluNET/FluID+RKI",
  hsp_rate_flu = 10000000 * (hsp_abs_flu / denominator),
  hsp_rate_rsv = 10000000 * (hsp_abs_rsv / denominator)
)

# Set all NAs
WHO_RKI_Hosp_data$id <- NA
WHO_RKI_Hosp_data$cases_rate_flu <- NA
WHO_RKI_Hosp_data$cases_rate_rsv <- NA
WHO_RKI_Hosp_data$cases_rate_covid19 <- NA
WHO_RKI_Hosp_data$cases_abs_covid <- NA
WHO_RKI_Hosp_data$cases_abs_flu <- NA
WHO_RKI_Hosp_data$cases_abs_rsv <- NA
WHO_RKI_Hosp_data$subtype_c_abs <- NA
WHO_RKI_Hosp_data$subtype_a_rate<-NA
WHO_RKI_Hosp_data$subtype_b_rate<-NA
WHO_RKI_Hosp_data$subtype_c_rate<-NA

# Reorder variables
WHO_RKI_Hosp_data[, order_header_premerge]

# save and export
write_csv(WHO_RKI_Hosp_data, file = 'data/processed_data/DE/WHO_RKI_Hosp_data.csv')
