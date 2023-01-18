library(tidyr)
library(dplyr)
library(stringr)

source(file = 'scripts/cleaning/helper.R')

source(file = 'scripts/cleaning/US/Script_COVID.R')
source(file = 'scripts/cleaning/US/Script_Flu.R')
source(file = 'scripts/cleaning/US/Script_RSV.R')
rm(db_covid, db_flu, db_rsv) # less items in env

# Flu

US_Flu_premerged_age_groups <- db_flu_age |> select(
     data_source = NETWORK,
     year = `MMWR-YEAR`,
     week = `MMWR-WEEK`,
     age_group = `AGE CATEGORY`,
     hsp_rate_flu = `WEEKLY RATE`
)

US_Flu_premerged_total <- db_flu_overall |> select(
     data_source = NETWORK,
     year = `MMWR-YEAR`,
     week = `MMWR-WEEK`,
     age_group = `AGE CATEGORY`,
     hsp_rate_flu = `WEEKLY RATE`
)

# COVID-19 ----------------------------------------------------------------

US_COVID_premerged_age_groups <- db_covid_age |> select(
     data_source = NETWORK,
     year = `MMWR-YEAR`,
     week = `MMWR-WEEK`,
     age_group = `AGE CATEGORY`,
     hsp_rate_covid19 = `WEEKLY RATE`
)

US_COVID_premerged_total <- db_covid_overall |> select(
     data_source = NETWORK,
     year = `MMWR-YEAR`,
     week = `MMWR-WEEK`,
     age_group = `AGE CATEGORY`,
     hsp_rate_covid19 = `WEEKLY RATE`
)


# RSV ---------------------------------------------------------------------

US_RSV_premerged_age_groups <- db_rsv_age |> select(
     data_source = State,
     year = Season,
     week = `MMWR Week`,
     age_group = `Age Category`,
     hsp_rate_rsv = Rate
) |> mutate(week = as.numeric(week)) |> mutate(year = case_when(
     week >= 40 ~ substr(year, start = 1, stop = 4),
     week < 25 ~ substr(year, start = 6, stop = 9)
)) |> mutate(year = as.numeric(year))

US_RSV_premerged_total <- db_rsv_overall |> select(
     data_source = State,
     year = Season,
     week = `MMWR Week`,
     age_group = `Age Category`,
     hsp_rate_rsv = Rate
) |> mutate(week = as.numeric(week)) |> mutate(year = case_when(
     week >= 40 ~ substr(year, start = 1, stop = 4),
     week < 25 ~ substr(year, start = 6, stop = 9)
)) |> mutate(year = as.numeric(year))


# Age stratified ----------------------------------------------------------

# THIS NEEDS WORK! Problem with stratification

     # US_premerged_age_groups <-
     #      bind_rows(
     #           US_Flu_premerged_age_groups,
     #           US_RSV_premerged_age_groups,
     #           US_COVID_premerged_age_groups
     #      )
     # 
     # colnames(US_Flu_premerged_age_groups)
     # colnames(US_RSV_premerged_age_groups)
     # colnames(US_COVID_premerged_age_groups)


# Totals ------------------------------------------------------------------

US_premerged_total <-
     bind_rows(US_Flu_premerged_total,
               US_RSV_premerged_total,
               US_COVID_premerged_total) |> mutate(
                    hsp_rate_covid19 = as.numeric(hsp_rate_covid19),
                    hsp_rate_flu = as.numeric(hsp_rate_flu)
               )

US_premerged_total <- US_premerged_total |> select(-data_source) |>
     pivot_longer(
          names_to = "pathogen",
          cols = c(hsp_rate_flu,
                   hsp_rate_covid19,
                   hsp_rate_rsv),
          values_to = "weekly_rate"
     ) |> drop_na() |> pivot_wider(names_from = pathogen, values_from = weekly_rate)


order_header_premerge

US_premerged_total$data_source <- "CDC" # data_source
US_premerged_total$country <- "US" # country
US_premerged_total$hemisphere <- "NH"# hemisphere
US_premerged_total$hsp_rate <- NA # hsp_rate

# These are all set to NA as Germany provides weekly incidence rate reports
US_premerged_total[order_header_premerge[11:27]] <- NA


# Set the order in the CSV
US_premerged_overall <- US_premerged_overall[, order_header_premerge]

write_csv(US_premerged_total, file="data/premerged_data/US_premerged_total.csv")