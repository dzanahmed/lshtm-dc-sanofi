#install.packages('tidyverse')
#install.packages('stringr')

library(tidyverse)
library(stringr)

# For template and variable order
source(file = 'scripts/cleaning/var_order_merged_csv.R')

us_flu <- read_csv(file='data/processed_data/US/US_Flu.csv')
us_rsv <- read_csv(file='data/processed_data/US/US_RSV.csv')
us_covid <- read_csv(file='data/processed_data/US/US_COVID.csv')

unique(us_covid$age_group)

# List dataframes, then drop first column (data source - as they are all CDC data)
us_data <- list(us_flu, us_rsv, us_covid)
us_data <- lapply(us_data, function(x){x[,-1]})

# Full join on time and age groups
us_data <- us_data |> reduce(full_join, by=c('year','week','age_group'))

# Join with template for all premerge variables, set order
us_data <- merge(us_data, template, all.x=TRUE, all.y=TRUE)
us_data <- us_data[, order_header_premerge]

# Set uniform CSV values
us_data$data_source <- "CDC FluSurv-Net/RSV-NET/COVID-NET"
us_data$country <- "US"
us_data$hemisphere <- "NH"

# Age-aggregated and age-stratified
US_premerged_total <- us_data |> filter(age_group=="ALL")
US_premerged_by_age <- us_data |> filter(age_group!="ALL")

# Write CSV
write_csv(US_premerged_total, file="data/premerged_data/US_premerged_total.csv")
write_csv(US_premerged_by_age, file="data/premerged_data/US_premerged_by_age.csv")