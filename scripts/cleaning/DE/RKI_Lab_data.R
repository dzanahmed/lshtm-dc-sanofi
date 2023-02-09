# This is for processing raw Germany Lab data from Robert Koch Institut 

# Output is a file 'RKI_Lab_data.csv' with year, week, age_group, cases_rate_flu, cases_rate_covid19

#install.packages('tidyverse')
#install.packages('stringr')

library(tidyverse)
library(stringr)

# All RKI Lab data is here
dir <- "data/raw_data/DE/RKI_Lab_data" 

files <- list.files(dir, pattern = "*.csv", full.names = TRUE)

all_RKI_data <- data.frame()

# For loop to read in all the .csvs from dir folder with raw data
for (file in files) {
     df <- read.csv(
          file = file,
          sep = "\t",
          skipNul = T,
          check.names = F
     )
     colnames(df)[1] <- "week"
     
     year <- substr(file, start = nchar(dir) + 2, stop = nchar(dir) + 5)
     pathogen <- substr(file, start = nchar(dir) + 10, stop = nchar(dir) + 12)
     
     df$year <- as.numeric(year)
     df$pathogen <- pathogen
     
     all_RKI_data <- rbind(all_RKI_data, df)
}

# Rename all COV values in pathogen column to covid19 for consistency
all_RKI_data$pathogen[all_RKI_data$pathogen=="COV"] <- "covid19" # rename

# Set all NAs to 0s
all_RKI_data[is.na(all_RKI_data)] <- 0 

# Reorder variables and remove Unknown as there are no values present other than 0
all_RKI_data <- all_RKI_data |> dplyr::select(year, week, pathogen, Total, starts_with('A'))

# Reorient the table to show weeks & age-groups in rows and lab+ incidence by virus in columns
all_RKI_data <- all_RKI_data |> pivot_longer(names_to = "age_group",
                                             values_to = "incidence",
                                             cols = Total:`A80+`) |>
        pivot_wider(names_from = pathogen, values_from = incidence) |>
        rename(
                "cases_rate_flu" = flu,
                "cases_rate_rsv" = rsv,
                "cases_rate_covid19" = covid19
        ) 

# Rename values in age groups

all_RKI_data <-
        all_RKI_data |> mutate(age_group = case_when(
                age_group == "Total" ~ "ALL",
                age_group != "Total" ~ str_replace_all(age_group, c("\\.." =
                                                                            "-", "A" = ""))
        ))

# Write a CSV
write_csv(all_RKI_data, file = 'data/processed_data/DE/RKI_Lab_data.csv')