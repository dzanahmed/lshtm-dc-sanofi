##----------------------------------------------------------------
##                           Packages                           --
##----------------------------------------------------------------
#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages('readxl')
#install.packages('readr')
#install.packages('lubridate')

library(tidyverse)
library(ggplot2)
library(readxl)
library(readr)
library(lubridate)

##----------------------------------------------------------------
##                     Countries population                     --
##----------------------------------------------------------------

#Population based on INE [Instituto de estadistica]
#(https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion)

population <- read_csv("data/raw_data/Countries_population.csv")
population <- population %>% 
     filter(code == "CL") %>% 
     select(year, population)

##----------------------------------------------------------------
##        Data set all for COVID-19 hospitalization ages        --
##----------------------------------------------------------------

hospilalised <- read_csv("data/raw_data/CL/HospitalizadosEtario_Acumulado_std.csv")
colnames(hospilalised) <- c("age_group", "sex", "date", "number")

hospilalised$date <- as.Date(hospilalised$date, "%d-%m-%Y")
hospilalised$week <- epiweek(hospilalised$date)
hospilalised <-hospilalised %>% filter(date >= "2021-12-31")


hospilalised$age_group <- as.factor(hospilalised$age_group)
hospilalised$year <- format(hospilalised$date, "%Y")

hospilalised_week <- hospilalised %>% 
     group_by(date, year, week, age_group) %>% 
     summarise("cumul_cases" = sum(number, na.rm = T)) %>% 
     arrange(date, year, week,age_group) 

hospilalised_week <-  hospilalised_week %>% 
     group_by(year, week, age_group) %>% 
     summarise("cumul_cases" = last(cumul_cases)) %>% 
     filter(cumul_cases > 0) %>% 
     arrange(year, week,age_group) 

## Editing levels of variable age

hospilalised_week$age_group <-as.character(hospilalised_week$age_group)

hospilalised_week$age_group <- ifelse(hospilalised_week$age_group == "00  5 a??os", "0-5",
                                      ifelse(hospilalised_week$age_group == "5  17 a??os", "5-17",
                                             ifelse(hospilalised_week$age_group == "18  49 a??os", "18-49",
                                                    ifelse(hospilalised_week$age_group == "50  59 a??os", "50-59",
                                                           ifelse(hospilalised_week$age_group == "60  69 a??os", "60-69",
                                                                  ifelse(hospilalised_week$age_group == "70  79 a??os", "70-79",
                                                                         ifelse(hospilalised_week$age_group == "80 y m??s a??os", "80+",0)))))))

hospilalised_week$age_group <- factor(hospilalised_week$age_group, levels = c("0-5","5-17","18-49","50-59","60-69","70-79", "80+"))
hospilalised_week <- arrange(hospilalised_week,year, week,age_group)

# Obtain number of cases - prior week
hospilalised_week$prior_week <- c(rep(NA,7),hospilalised_week$cumul_cases[-c(379:385)])
hospilalised_week$daily <- hospilalised_week$cumul_cases - hospilalised_week$prior_week

hospilalised_week$denominator <- as.numeric(ifelse(hospilalised_week$year == 2022, population[7,2],
                                                   ifelse(hospilalised_week$year == 2023, population[8,2], NA)))


##----------------------------------------------------------------
##             Data from SARI cases (sentinel data)             --
##----------------------------------------------------------------

sari_chile <- read_csv('data/raw_data/CL/SARI_minsal.csv')
sari_chile <- sari_chile[,c(1:5)]

sari_chile$virus2 <- ifelse(sari_chile$virus %in% c("Influenza A","Influenza B"),"flu",
                            ifelse(sari_chile$virus == "VRS", "RSV", sari_chile$virus ))

sari_chile <- filter(sari_chile, !is.na(virus2))
colnames(sari_chile) <- c("year", "week", "subtipe", "subtipe2", "positive_sample", "virus")


sari_chile <- sari_chile %>% 
     group_by(year, week,virus) %>% 
     summarise("positive_sample" = sum(positive_sample)) 
sari_chile <- pivot_wider(sari_chile, names_from = "virus", values_from = "positive_sample")

# set prior 2020 COVID to cero
sari_chile$COVID[sari_chile$year != 2022 ] <- 0

## Adding population

sari_chile$denominator <- as.numeric(ifelse(sari_chile$year == 2016, population[1,2],
                                            ifelse(sari_chile$year == 2017, population[2,2],
                                                   ifelse(sari_chile$year == 2018, population[3,2],
                                                          ifelse(sari_chile$year == 2019, population[4,2],
                                                                 ifelse(sari_chile$year == 2022, population[7,2],0))))))


##----------------------------------------------------------------
##   The data set for all age - FLU - RSV - COVID from Minsal   --
##----------------------------------------------------------------

chi_premerged_total <- data.frame(
     id =1:260,
     data_source = "EPI MINSAL",
     country = "CL",
     hemisphere = "SH",
     week = sari_chile$week,
     year = sari_chile$year,
     age_group = "ALL",
     hsp_rate = round(((sari_chile$flu + sari_chile$RSV + sari_chile$COVID)/(sari_chile$denominator*0.03))*100000,2),
     hsp_rate_flu = round((sari_chile$flu/(sari_chile$denominator*0.03))*100000,2),
     hsp_rate_rsv = round((sari_chile$RSV/(sari_chile$denominator*0.03))*100000,2),
     hsp_rate_covid19 = round((sari_chile$COVID/(sari_chile$denominator*0.03))*100000,2),
     cases_rate_flu = NA,
     cases_rate_rsv = NA,
     cases_rate_covid19 = NA,
     hsp_abs = (sari_chile$flu + sari_chile$RSV + sari_chile$COVID),
     hsp_abs_flu = sari_chile$flu,
     hsp_abs_rsv = sari_chile$RSV,
     hsp_abs_covid = sari_chile$COVID,
     cases_abs_flu = NA,
     cases_abs_rsv = NA,
     cases_abs_covid19 = NA,
     denominator = sari_chile$denominator,
     subtype_a_abs = NA,
     subtype_b_abs = NA,
     subtype_c_abs = NA,
     subtype_a_rate = NA,
     subtype_b_rate = NA,
     subtype_c_rate = NA
)


#write_csv(chi_premerged_total, "/Users/igna/Documents/LSHTM/Term 2/Data Challenge/lshtm-dc-sanofi/data/premerged_data/CL_premerged_total.csv")

##---------------------------------------------------------------
##    The data set by age for hospitalizations (only COVID)    --
##              From Ministry of Science Git-Hub               --
##---------------------------------------------------------------


chi_premerged_by_age <- data.frame(
     id =1:385,
     data_source = "EPI MINSAL - Ministry of Science",
     country = "chi",
     hemisphere = "sh",
     week = hospilalised_week$week,
     year = hospilalised_week$year,
     age_group = hospilalised_week$age_group,
     hsp_rate = NA,
     hsp_rate_flu = NA,
     hsp_rate_rsv = NA,
     hsp_rate_covid19 = round((hospilalised_week$daily/hospilalised_week$denominator)*100000,2),
     cases_rate_flu = NA,
     cases_rate_rsv = NA,
     cases_rate_covid19 = NA,
     hsp_abs = NA,
     hsp_abs_flu = NA,
     hsp_abs_rsv = NA,
     hsp_abs_covid = hospilalised_week$daily,
     cases_abs_flu = NA,
     cases_abs_rsv = NA,
     cases_abs_covid19 = NA,
     denominator = hospilalised_week$denominator,
     subtype_a_abs = NA,
     subtype_b_abs = NA,
     subtype_c_abs = NA,
     subtype_a_rate = NA,
     subtype_b_rate = NA,
     subtype_c_rate = NA
)

chi_premerged_by_age <- filter(chi_premerged_by_age, !is.na(denominator)) # delete 7 first rows

#write_csv(chi_premerged_by_age, "data/premerged_data/chi_premerged_by_age.csv")

#################################################################
##                       Other countries                       ##
#################################################################

##---------------------------------------------------------------
##                          Australia                          --
##---------------------------------------------------------------

# This code was meant to fix and the population error (just the last year)

flunet_raw <- read_csv("data/processed_data/AUS/australia_fluNet.csv")


flunet_raw$denominator[flunet_raw$denominator == 25890.8] <- 25978935


aus_premerged_total <- data.frame(
     data_source = flunet_raw$data_source,
     country = "AUS",
     hemisphere = "SH",
     week = flunet_raw$week,
     year = flunet_raw$year,
     age_group = "ALL",
     hsp_rate = NA,
     hsp_rate_flu = round((flunet_raw$hsp_abs_flu/flunet_raw$denominator)*100000,2),
     hsp_rate_rsv = round((flunet_raw$hsp_abs_rsv/flunet_raw$denominator)*100000,2),
     hsp_rate_covid19 = NA,
     cases_rate_flu = NA,
     cases_rate_rsv = NA,
     cases_rate_covid19 = NA,
     hsp_abs = NA,
     hsp_abs_flu = flunet_raw$hsp_abs_flu,
     hsp_abs_rsv = flunet_raw$hsp_abs_rsv,
     hsp_abs_covid = NA,
     cases_abs_flu = NA,
     cases_abs_rsv = NA,
     cases_abs_covid = NA,
     denominator = flunet_raw$denominator,
     subtype_a_abs = flunet_raw$subtype_a_abs,
     subtype_b_abs = flunet_raw$subtype_b_abs,
     subtype_c_abs = NA,
     subtype_a_rate = round((flunet_raw$subtype_a_abs/flunet_raw$denominator)*100000,2),
     subtype_b_rate = round((flunet_raw$subtype_b_abs/flunet_raw$denominator)*100000,2),
     subtype_c_rate = NA
)

#write_csv(aus_premerged_total, "data/premerged_data/AU_premerged_total.csv")

##---------------------------------------------------------------
##                     France non_sentinel                     --
##---------------------------------------------------------------

## This code takes FR data from processed_data folder (those don't have all the needed columns). This code provides the missing columns

preclean_fran_flu<- read_csv("data/processed_data/FRA/france_fluNet.csv")

preclean_fran_covid<- read_csv("data/processed_data/FRA/france_covid19.csv")


FR_premerged_total <- data.frame(
     data_source = ifelse(preclean_fran_flu$origin_source == "NONSENTINEL", "FluNet - Non Sentinel","FluNet - Sentinel" ),
     country = "FR",
     hemisphere = "NH",
     week = preclean_fran_flu$week,
     year = preclean_fran_flu$year,
     age_group = "ALL",
     hsp_rate = NA,
     hsp_rate_flu = round((preclean_fran_flu$hsp_abs_flu/preclean_fran_flu$denominator)*100000,2),
     hsp_rate_rsv = round((preclean_fran_flu$hsp_abs_rsv/preclean_fran_flu$denominator)*100000,2),
     hsp_rate_covid19 = NA,
     cases_rate_flu = NA,
     cases_rate_rsv = NA,
     cases_rate_covid19 = NA,
     hsp_abs = NA,
     hsp_abs_flu = preclean_fran_flu$hsp_abs_flu,
     hsp_abs_rsv = preclean_fran_flu$hsp_abs_rsv,
     hsp_abs_covid = NA,
     cases_abs_flu = NA,
     cases_abs_rsv = NA,
     cases_abs_covid = NA,
     denominator = preclean_fran_flu$denominator,
     subtype_a_abs = preclean_fran_flu$subtype_a_abs,
     subtype_b_abs = preclean_fran_flu$subtype_b_abs,
     subtype_c_abs = NA,
     subtype_a_rate = round((preclean_fran_flu$subtype_a_abs/preclean_fran_flu$denominator)*100000,2),
     subtype_b_rate = round((preclean_fran_flu$subtype_b_abs/preclean_fran_flu$denominator)*100000,2),
     subtype_c_rate = NA
)

FR_premerged_total$hsp_abs_covid <- left_join(preclean_fran_flu, preclean_fran_covid, by = c("year", "week"))$hsp_abs_covid19

FR_premerged_total$hsp_rate_covid19 <- round((FR_premerged_total$hsp_abs_covid/FR_premerged_total$denominator)*100000,2)

#write_csv(FR_premerged_total, "data/premerged_data/FR/FR_premerged_total.csv")






