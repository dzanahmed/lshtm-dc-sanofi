### AUSTRALIA RAW TO PROCESSED COVID ###
#install.packages('tidyverse')
library(tidyverse)

#change working directory if needed
#setwd('~/lshtm-dc-sanofi')

#read in raw data file
aus_covid <- read.csv('data/raw_data/AUS/aus_covid_national_update.csv')

#read in epidemiological weeks file
epi_wks <- read.csv('data/epi_weeks.csv')

#create date column with complete data
aus_covid <- aus_covid %>% rowwise() %>%
     mutate(date = paste0(Date,'/',X))

#select only data from 2022 onwards
aus_covid <- aus_covid %>% filter(X >= 2022)

#reformat dates to Date class
aus_covid$date <- as.Date(aus_covid$date,format='%d/%m/%Y')
epi_wks$epi_dates <- as.Date(epi_wks$epi_dates)

#merge epi weeks data with australia data by epi_dates/date
merged_data <- merge(epi_wks, aus_covid, by.x = c('epi_dates'), by.y = c('date'))

#drop unnecessary columns
merged_data$Date <- NULL
merged_data$X <- NULL

#rename colnames
colnames(merged_data) <- c("epi_dates","epi_wk_no","year","hsp_abs_covid" )

#save as csv in processed folder
write.csv(merged_data,'data/processed_data/AUS/australia_covid_all_updated.csv')
