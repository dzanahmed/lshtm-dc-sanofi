library(tidyr)
library(dpylr)
library(stringr)

source(file='scripts/cleaning/helper.R')

source(file='scripts/cleaning/US/Script_COVID.R')
source(file='scripts/cleaning/US/Script_Flu.R')
source(file='scripts/cleaning/US/Script_RSV.R')
rm(db_covid, db_flu, db_rsv) # less items in env

# Flu

US_Flu_premerged_age_groups <- db_flu_age |> select(
     NETWORK,
     week = `MMWR-WEEK`,
     year = `MMWR-YEAR`,
     age_group = `AGE CATEGORY`,
     hsp_rate_flu = `WEEKLY RATE`
)


US_Flu_premerged_total <- db_flu_overall |> select(
     NETWORK,
     week = `MMWR-WEEK`,
     year = `MMWR-YEAR`,
     age_group = `AGE CATEGORY`,
     hsp_rate_flu = `WEEKLY RATE`
)


# COVID-19 ----------------------------------------------------------------

US_COVID_premerged_age_groups <- db_covid_age |> select(
     NETWORK,
     week = `MMWR-WEEK`,
     year = `MMWR-YEAR`,
     age_group = `AGE CATEGORY`,
     hsp_rate_covid19 = `WEEKLY RATE`
)

US_COVID_premerged_total <- db_covid_overall |> select(
     NETWORK,
     week = `MMWR-WEEK`,
     year = `MMWR-YEAR`,
     age_group = `AGE CATEGORY`,
     hsp_rate_covid19 = `WEEKLY RATE`
) 


# RSV ---------------------------------------------------------------------

US_RSV_premerged_age_groups <- db_rsv_age |> select(
     State,
     year = Season,
     week = `MMWR Week`,
     age_group = `Age Category`,
     hsp_rate_rsv = Rate
) |> mutate(week = as.numeric(week)) |> mutate(year = case_when(
     week >= 40 ~ substr(year, start = 1, stop = 4),
     week < 25 ~ substr(year, start = 6, stop = 9)
))

US_RSV_premerged_total <- db_rsv_overall |> select(
     State,
     year = Season,
     week = `MMWR Week`,
     age_group = `Age Category`,
     hsp_rate_rsv = Rate
) |> mutate(week = as.numeric(week)) |> mutate(year = case_when(
     week >= 40 ~ substr(year, start = 1, stop = 4),
     week < 25 ~ substr(year, start = 6, stop = 9)
))

order_header_premerge
