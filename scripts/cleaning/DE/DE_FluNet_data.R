# This is for processing WHO FluNET/FluID data for Germany

#install.packages('tidyverse')
library(tidyverse)

#----------------------------------------------------------------
# import FluID and FluNET data

# FluID has numbers on number of total SARI hospitalizations from 2022 onwards
VIW_FID_EPI <- read_csv("data/raw_data/FluNet/VIW_FID_EPI.csv")

# FluNET has influenza and RSV numbers from a surveyed ~1% of the population of Germany
VIW_FNT <- read_csv("data/raw_data/FluNet/VIW_FNT.csv")

# Setting up helpers

# Functions to wipe NA cols
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

years <- c(2016:2019, 2022,2023)

source(file="scripts/cleaning/var_order_merged_csv.R")

# WHO Data: FluID + FluNet ------------------------------------------------

# Hospitalization rate for SARI for all age groups calculated here in hsp_rate variable
# Lot of entries with "unknown" in age_group field, hence all those are dropped.

# Numbers for hsp_rate seem very high.

germany_fluID <-
  VIW_FID_EPI |> filter(COUNTRY_CODE == 'DEU' &
                          MMWR_YEAR %in% years &
                          AGEGROUP_CODE != "UNKNOWN") |>
  select(where(not_all_na)) |> select(
    MMWR_WEEKSTARTDATE,
    MMWR_YEAR,
    MMWR_WEEK,
    AGEGROUP_CODE,
    SARI_INPATIENTS,
    SARI_POP_COV
  ) |> drop_na() |>
  mutate(hsp_rate = SARI_INPATIENTS / SARI_POP_COV * 100000) |>
  mutate(AGEGROUP_CODE = toupper(AGEGROUP_CODE)) |> 
  select(-c(SARI_INPATIENTS, SARI_POP_COV))


# FluNet for DE has sentinel and non-sentinel data. 
# Filter for DE, drop NA columns and select what is necessary

# Sentinel data seems more reliable 

germany_fluNET <-
  VIW_FNT |> filter(COUNTRY_CODE == 'DEU' &
                      MMWR_YEAR %in% years &
                      ORIGIN_SOURCE=="SENTINEL" 
                      ) |> select(where(not_all_na)) |>
  select(MMWR_WEEKSTARTDATE,
         MMWR_YEAR,
         MMWR_WEEK,
         INF_A,
         INF_B,
         INF_ALL,
         RSV)

# FluNet has aggregated age data, no age groups. Set to uppercase ALL to match FluID
germany_fluNET$AGEGROUP_CODE <- "ALL"

# Merge these two on week/year/agegroup, and rename for merge
germany_WHO_fluNET_fluID_data <- merge(
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
            "year" = MMWR_YEAR,
            "subtype_a_abs"=INF_A,
            "subtype_b_abs"=INF_B,
            "hsp_abs_flu"=INF_ALL,
            "hsp_abs_rsv"=RSV) |> 
  select(-MMWR_WEEKSTARTDATE)

# Write a CSV
write_csv(germany_WHO_fluNET_fluID_data, file = 'data/processed_data/DE/WHO_FluNet_data.csv')