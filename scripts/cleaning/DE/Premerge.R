library(tidyr)
library(dplyr)

DE_premerged <-
     read.csv(file = "data/processed_data/DE/DE_Pathogens_Years_Weekly_Age_breakdown.csv")

DE_premerged <- DE_premerged |> select(-X) # remove ID

# Only totals across all age groups - each week is a row, each pathogen has its column
DE_premerged_all_age_groups <-
     DE_premerged |> group_by(Year, Week, Pathogen) |>
     summarise(age_group = "Total", weekly_inc = Total) |> distinct()

# Drop totals, pivot long - each week is a row, each pathogen has its column
DE_premerged_age_groups <-
     DE_premerged |> select(-Total) |> pivot_longer(names_to = "age_group",
                                                values_to = "weekly_inc",
                                                cols = starts_with("A"))

# Combine the two above
DE_premerged_overall <- rbind(DE_premerged_age_groups, DE_premerged_all_age_groups) |> 
     arrange(Year, Week, Pathogen)


# Now let's build a dataframe for the premerged csv

DE_premerged$data_source <- "RKI"
DE_premerged$country <- "DE"
DE_premerged$hemisphere <- "NH"

# 
# 
# id
# data_source
# country
# hemisphere
# week
# year
# age_group
# hsp_rate
# hsp_rate_flu
# hsp_rate_rsv
# hsp_rate_covid19
# cases_rate_flu
# cases_rate_rsv
# cases_rate_covid19
# hsp_abs
# hsp_abs_flu
# hsp_abs_rsv
# hsp_abs_covid
# cases_abs_flu
# cases_abs_flu
# cases_abs_flu
# denominator
# subtype_a_abs
# subtype_b_abs
# subtype_c_abs
# subtype_a_rate
# subtype_b_rate
# subtype_c_rate
