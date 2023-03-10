### AUS PROCESSED TO PREMERGED ###
#install.packages('tidyverse')
library(tidyverse)

#Update working directory as required
#setwd('~/lshtm-dc-sanofi')

#read in csv files made in previous step for merging
aus_covid <- read.csv('data/processed_data/AUS/australia_covid_all_updated.csv')
aus_flu_rsv <- read.csv('data/processed_data/AUS/australia_fluNet.csv')

## read in template file for premerged csv
template <- read.csv('data/template.csv')

#update column types for merging
template$data_source <- as.character(template$data_source)
template$country <- as.character(template$country)
template$hemisphere <- as.character(template$hemisphere)
template$denominator <- as.character(template$denominator)

#read in population csv
pop <- read.csv('data/raw_data/Countries_population.csv')
aus_pop <- pop %>% filter(code == 'AU')
aus_pop <- aus_pop %>% select('year','population')
aus_pop <- aus_pop %>% filter(year != 2020) %>% filter(year != 2021)

## merge two datasets
aus_all_viruses <- merge(aus_flu_rsv,aus_covid, by.x = c('week','year'),by.y = c('epi_wk_no','year'),all = T)

## add some metadata
aus_all_viruses$id <- seq(1:nrow(aus_all_viruses))
aus_all_viruses$data_source <- 'FluNet/AUS DOH'
aus_all_viruses$country <- 'AUS'
aus_all_viruses$hemisphere <- 'SH'

#add denominator data
aus_all_viruses <- merge(aus_all_viruses, aus_pop, by.x = c('year'),by.y = c('year'),all = T)
aus_all_viruses$denominator <- aus_all_viruses$population
aus_all_viruses$population <- NULL

#match column classes before merge
template$hsp_abs_covid <- as.character(template$hsp_abs_covid)
template$denominator <- as.integer(template$denominator)

#combine CSV with template
aus_premerged <- bind_rows(template,aus_all_viruses)

#tidy up rest of the relevant rows
aus_premerged$age_group <- 'ALL'
aus_premerged$denominator <- as.numeric(aus_premerged$denominator)
aus_premerged$hsp_abs_covid <- as.numeric(gsub(',','',aus_premerged$hsp_abs_covid))

#calculate hospital rates
aus_premerged <- aus_premerged %>% rowwise() %>%
     mutate(hsp_rate_covid19 = hsp_abs_covid/denominator*100000)
aus_premerged <- aus_premerged %>% rowwise() %>%
     mutate(hsp_rate_flu = hsp_abs_flu/denominator*100000)
aus_premerged <- aus_premerged %>% rowwise() %>%
     mutate(hsp_rate_rsv = hsp_abs_rsv/denominator*100000)
aus_premerged <- aus_premerged %>% rowwise() %>%
     mutate(hsp_rate = sum(hsp_rate_covid19,hsp_rate_flu,hsp_rate_rsv,na.rm=T))

#drop unnecessary columns
aus_premerged$X <- NULL
aus_premerged$epi_dates <- NULL

#write csv for export
write.csv(aus_premerged,'data/premerged_data/AU_premerged_total.csv')
