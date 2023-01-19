library(tidyr)
library(dplyr)

DE_premerged <-
     read.csv(file = "data/processed_data/DE/DE_Pathogens_Years_Weekly_Age_breakdown.csv")

DE_premerged <- DE_premerged |> select(-X) # remove ID

# Only totals across all age groups - each week is a row, each pathogen has its column
DE_premerged_all_age_groups <-
     DE_premerged |> group_by(year, week, pathogen) |>
     summarise(age_group = "Total", weekly_inc = Total) |> 
     distinct()

# Drop totals, pivot long - each week is a row, each pathogen has its column
DE_premerged_age_groups <-
     DE_premerged |> select(-Total) |> pivot_longer(names_to = "age_group",
                                                values_to = "weekly_inc",
                                                cols = starts_with("A"))

# Combine the two above
DE_premerged_overall <- rbind(DE_premerged_age_groups, DE_premerged_all_age_groups) |> 
     arrange(year, week, pathogen)


DE_premerged_overall <-
     DE_premerged_overall |> pivot_wider(names_from = pathogen,
                                         names_prefix = "hsp_rate_",
                                         values_from = weekly_inc)

# Now let's build a dataframe for the premerged csv

source(file='scripts/cleaning/helper.R') # Import order of the header columns

DE_premerged_overall$id <- NA # id
DE_premerged_overall$data_source <- "RKI" # data_source
DE_premerged_overall$country <- "DE" # country
DE_premerged_overall$hemisphere <- "NH"# hemisphere
# week
# year
# age_group
DE_premerged_overall$hsp_rate <- NA # hsp_rate
# hsp_rate_flu
# hsp_rate_rsv
# hsp_rate_covid19

# These are all set to NA as Germany provides weekly incidence rate reports
DE_premerged_overall[order_header_premerge[12:28]]<-NA 

# Equivalent to:

# DE_premerged_overall$cases_rate_flu      <- NA     # cases_rate_flu
# DE_premerged_overall$cases_rate_rsv      <- NA     # cases_rate_rsv
# DE_premerged_overall$cases_rate_covid_19      <- NA     # cases_rate_covid19
# DE_premerged_overall$hsp_abs      <- NA     # hsp_abs
# DE_premerged_overall$hsp_abs_flu      <- NA     # hsp_abs_flu
# DE_premerged_overall$hsp_abs_rsv      <- NA     # hsp_abs_rsv
# DE_premerged_overall$hsp_abs_covid      <- NA     # hsp_abs_covid
# DE_premerged_overall$cases_abs_flu      <- NA     # cases_abs_flu
# DE_premerged_overall$cases_abs_rsv      <- NA     # cases_abs_rsv
# DE_premerged_overall$cases_abs_covid     <- NA     # cases_abs_covid
# DE_premerged_overall$denominator_      <- NA     # denominator
# DE_premerged_overall$subtype_a_abs      <- NA     # subtype_a_abs
# DE_premerged_overall$subtype_b_abs      <- NA     # subtype_b_abs
# DE_premerged_overall$subtype_c_abs      <- NA     # subtype_c_abs
# DE_premerged_overall$subtype_a_rate      <- NA     # subtype_a_rate
# DE_premerged_overall$subtype_b_rate      <- NA     # subtype_b_rate
# DE_premerged_overall$subtype_c_rate     <- NA     # subtype_c_rate

# Set the order in the CSV
DE_premerged_overall <- DE_premerged_overall[, order_header_premerge]

# Split to totals and age-stratified
DE_premerged_total <- DE_premerged_overall |> filter(age_group=="Total")
DE_premerged_by_age <- DE_premerged_overall |> filter(age_group!="Total")

# Export to .csv for premerge

write_csv(DE_premerged_total, file="data/premerged_data/DE_premerged_total.csv")
write_csv(DE_premerged_by_age, file="data/premerged_data/DE_premerged_by_age.csv")