library(tidyverse)
library(readr)
setwd('~/lshtm-dc-sanofi')

aus_covid <- read.csv('data/raw_data/AUS/aus_covid_national_update.csv')
epi_wks <- read.csv('data/epi_weeks.csv')
aus_covid <- aus_covid %>% rowwise() %>%
     mutate(date = paste0(Date,'/',X))

aus_covid <- aus_covid %>% filter(X >= 2022)

aus_covid$date <- as.Date(aus_covid$date,format='%d/%m/%Y')
epi_wks$epi_dates <- as.Date(epi_wks$epi_dates)

merged_data <- merge(epi_wks, aus_covid, by.x = c('epi_dates'), by.y = c('date'))

#drop unnecessary columns
merged_data$Date <- NULL
merged_data$X <- NULL

#rename colnames
colnames(merged_data) <- c("epi_dates","epi_wk_no","year","hsp_abs_covid" )

#save as csv in processed folder
write.csv(merged_data,'data/processed_data/AUS/australia_covid_all_updated.csv')
