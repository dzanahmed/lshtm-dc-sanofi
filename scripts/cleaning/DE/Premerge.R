library(tidyverse)
library(stringr)

# Import order of the header columns
source(file='scripts/cleaning/var_order_merged_csv.R') 

#German population data
germanyPopulation <- read_csv("data/raw_data/Countries_population.csv") 
germanyPopulation <- germanyPopulation |> filter(code=="DE")

# Read in processed files
RKI_Lab_data <- read_csv(file="data/processed_data/DE/RKI_Lab_data.csv")
WHO_FluNet_data <- read_csv(file="data/processed_data/DE/WHO_FluNet_data.csv") 
RKI_COVID_hosp_data <- read_csv(file="data/processed_data/DE/RKI_Covid_hosp_data.csv")

# Merge WHO FluNet with RKI Covid data
DE_premerged <- merge(
        WHO_FluNet_data,
        RKI_COVID_hosp_data,
        by = c("year", "week", "age_group"),
        all.x = TRUE,
        all.y = TRUE
)

# Merge result of previous merge with RKI Lab data
DE_premerged <- merge(
        DE_premerged,
        RKI_Lab_data,
        by = c("year", "week", "age_group"),
        all.x = TRUE,
        all.y = TRUE
)

# Merge with template for structure
DE_premerged <- merge(DE_premerged,
                     template,
                     all.x=TRUE,
                     all.y=TRUE)

# Add denominator / population data
DE_premerged <- DE_premerged |> select(-denominator) |> 
        left_join(germanyPopulation[,c("year", "population")], by=c("year"="year")) |>  
        rename("denominator"=population)

# Calculate Hospitalization rates based on absolute numbers divided by denominator, 
# multiplied by 100 (1%), and 100,000 for rate

DE_premerged <- DE_premerged |> mutate(hemisphere = "NH",
                                                 country = "DE",
                                                 data_source = "WHO FluNET/FluID+RKI",
                                                 hsp_rate_flu = 10000000 * (hsp_abs_flu / denominator),
                                                 hsp_rate_rsv = 10000000 * (hsp_abs_rsv / denominator)
)

# Reorder variables
DE_premerged <- DE_premerged[, order_header_premerge]

DE_premerged_total <- DE_premerged |> filter(age_group=="ALL")
DE_premerged_by_age <- DE_premerged |> filter(age_group!="ALL")

# save and export
write_csv(DE_premerged_total, file = 'data/premerged_data/DE_premerged_total.csv')
write_csv(DE_premerged_by_age, file = 'data/premerged_data/DE_premerged_by_age.csv')