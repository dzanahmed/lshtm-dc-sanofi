dir <- "data/raw_data/DE"

files <- list.files(dir, pattern = "*.csv", full.names = TRUE)

all_data <- data.frame()

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
     
     all_data <- rbind(all_data, df)
}

all_data[2:20] <- as.numeric(unlist(all_data[2:20])) # convert to numeric

all_data$pathogen[all_data$pathogen=="COV"] <- "covid19" # rename

all_data[is.na(all_data)] <- 0 # Set all NAs to 0s

# Reorder variables and remove Unknown as there are no values present other than 0
all_data <- all_data |> dplyr::select(year, week, pathogen, Total, starts_with('A'))

write.csv(all_data, file = 'data/processed_data/DE/DE_Pathogens_Years_Weekly_Age_breakdown.csv')