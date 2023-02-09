# This is for processing WHO FluNET/FluID data for Germany

# Output is a file 'RKI_Covid_hosp_data.csv' with year, week, age_group, hsp_abs_covid, hsp_rate_covid19

#install.packages('tidyverse')
library(tidyverse)

# Setting up helpers

source(file="scripts/cleaning/var_order_merged_csv.R")

epiweeks <- read_csv(file = 'data/epi_weeks.csv')
epiweeks <- epiweeks |> rename("Datum" = epi_dates, "week" = epi_wk_no)

# COVID data from RKI GitHub ----------------------------------------------

germany_covid_age <-
     read_csv(file = "data/raw_data/DE/COVID/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")

germany_covid_age <-
     germany_covid_age |> filter(Bundesland == "Bundesgebiet" &
                                      Datum > "2022-01-01") |> arrange(Datum)

# Add weeks from epiweeks
germany_covid_age <- merge(epiweeks, germany_covid_age)

# Rename german variables
germany_covid_age <- germany_covid_age |> 
     select(-c(Bundesland, Bundesland_Id, Datum)) |>
     rename(
     "hsp_abs_covid" = `7T_Hospitalisierung_Faelle`,
     "hsp_rate_covid19" = `7T_Hospitalisierung_Inzidenz`,
     "age_group" = Altersgruppe) |>
     mutate(
          age_group = case_when(
               age_group == "00+" ~ "ALL",
               age_group == "00-04" ~ "0TO4",
               age_group == "05-14" ~ "5TO14",
               # Other age groups are quite similar....
               TRUE ~ age_group
          )
     )

write_csv(germany_covid_age, file = 'data/processed_data/DE/RKI_Covid_hosp_data.csv')