# This is for processing DE Lab data from Robert Koch Institut 

# All RKI Lab data is here
dir <- "data/raw_data/DE" 

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

all_RKI_data[2:20] <- as.numeric(unlist(all_RKI_data[2:20])) # convert to numeric

all_RKI_data$pathogen[all_RKI_data$pathogen=="COV"] <- "covid19" # rename

all_RKI_data[is.na(all_RKI_data)] <- 0 # Set all NAs to 0s

# Reorder variables and remove Unknown as there are no values present other than 0
all_RKI_data <- all_RKI_data |> dplyr::select(year, week, pathogen, Total, starts_with('A'))

# Reorient the table to show weeks & age-groups in rows and lab+ incidence by virus in columns
all_RKI_data <-
        all_RKI_data |> pivot_longer(names_to = "age_group",
                                     values_to = "incidence",
                                     cols = Total:`A80+`) |>
        pivot_wider(names_from = pathogen, values_from = incidence) |> 
        rename("cases_rate_flu"=flu,"cases_rate_rsv"=rsv,"cases_rate_covid19"=covid19)

# Write a CSV
write.csv(all_RKI_data, file = 'data/processed_data/DE/RKI_Lab_data.csv')
