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
     colnames(df)[1] <- "Week"
     
     year <- substr(file, start = nchar(dir) + 2, stop = nchar(dir) + 5)
     pathogen <- substr(file, start = nchar(dir) + 10, stop = nchar(dir) + 12)
     
     df[is.na(df)] <- 0 # Set all NAs to 0s
     df$year <- as.numeric(year)
     df$pathogen <- pathogen
     
     all_data <- rbind(all_data, df)
}

all_data[2:20] <- as.numeric(unlist(all_data[2:20]))
all_data$pathogen[all_data$pathogen=="COV"] <- "COVID-19"


write.csv(all_data, file = 'data/processed_data/DE/DE_Pathogens_Years_Weekly_Age_breakdown.csv')
