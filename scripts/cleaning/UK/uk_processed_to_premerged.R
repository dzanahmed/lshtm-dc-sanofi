library(tidyverse)
library(readr)

setwd('~/lshtm-dc-sanofi')

#open processed files
rsv <- read.csv('data/processed_data/UK/rsv_all_yrs_all_ages.csv')
flu <- read.csv('data/processed_data/UK/flu_rates_all_ages.csv')
flu <- flu %>% select(!X)
covid <- read.csv('data/processed_data/UK/hsp_rates_cov_flu_2223.csv')
covid <- covid %>% select(!hsp_flu_rate)


premerged_data <- merge(flu,rsv,by=c('week','year'),all=T)
premerged_data <- merge(premerged_data,covid,by=c('week','year'),all=T)
premerged_data$age.x <- NULL
premerged_data$age.y <- NULL
premerged_data$age <- 'ALL'


## open template
template <- read.csv('data/template.csv',blank.lines.skip=T)

#open population csv (for later)
pop <- read.csv('data/raw_data/Countries_population.csv')
uk_pop <- pop %>% filter(code == 'UK')
uk_pop <- uk_pop %>% select('year','population')

#change premerged colnames to match template colnames
colnames(premerged_data) <- c('week','year','hsp_rate_flu','hsp_rate_rsv','hsp_rate_covid19','age')

#bind rows, keeping columns from both dataframes
combined <- bind_rows(template,premerged_data)

#drop age as there is age_group is this template that we can fill
combined$age <- NULL

#now fill in df rows with relevant information
names(combined) #getsnames to fill

combined$id <- seq(1:nrow(combined))
combined$data_source <- 'UKHSA'
combined$country <- 'UK'
combined$hemisphere <- 'NH'
combined$age_group <- 'ALL'

#add denominators from population csv
combined <- merge(combined, uk_pop, by.x = c('year'),by.y = c('year'),all = T)
combined$denominator <- as.numeric(combined$population)
combined$population <- NULL
class(combined$denominator)

#calculate absolute values
combined<- combined %>% mutate(hsp_abs_flu = (hsp_rate_flu*denominator)/100000)
combined<- combined %>% mutate(hsp_abs_rsv = (hsp_rate_rsv*denominator)/100000)
combined<- combined %>% mutate(hsp_abs_covid = (hsp_rate_covid19*denominator)/100000)
combined<- combined %>% rowwise() %>%
     mutate(hsp_abs = sum(hsp_abs_flu,hsp_abs_rsv,hsp_abs_covid,na.rm=T))

#calculate total hospitalisation rate
combined <- combined %>% rowwise() %>%
     mutate(hsp_rate = hsp_abs/(denominator/100000))

#export as pre_merged csv
write.csv(combined,'data/premerged_data/uk_premerged_total.csv')
