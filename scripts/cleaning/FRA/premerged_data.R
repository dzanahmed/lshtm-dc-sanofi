## France non_sentinel
#install.packages('tidyverse')
library(tidyverse)

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
     hsp_rate_flu = ifelse(preclean_fran_flu$origin_source == 'SENTINEL', 
                           round((preclean_fran_flu$hsp_abs_flu/(preclean_fran_flu$denominator*0.03))*100000,2), #01Feb23: updating to be 3% of the pop for sentinel data
                           round((preclean_fran_flu$hsp_abs_flu/preclean_fran_flu$denominator)*100000,2)),
     hsp_rate_rsv = ifelse(preclean_fran_flu$origin_source == 'SENTINEL', 
                           round((preclean_fran_flu$hsp_abs_rsv/(preclean_fran_flu$denominator*0.03))*100000,2), #01Feb23: updating to be 3% of the pop for sentinel data
                           round((preclean_fran_flu$hsp_abs_flu/preclean_fran_flu$denominator)*100000,2)),
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
     subtype_a_rate = ifelse(preclean_fran_flu$origin_source == 'SENTINEL',
                             round((preclean_fran_flu$subtype_a_abs/(preclean_fran_flu$denominator*0.03))*100000,2), #01Feb23: updating to be 3% of the pop for sentinel data
                             round((preclean_fran_flu$subtype_a_abs/preclean_fran_flu$denominator)*100000,2)), 
     subtype_b_rate = ifelse(preclean_fran_flu$origin_source == 'SENTINEL',
                             round((preclean_fran_flu$subtype_b_abs/(preclean_fran_flu$denominator*0.03))*100000,2), #01Feb23: updating to be 3% of the pop for sentinel data
                             round((preclean_fran_flu$subtype_b_abs/preclean_fran_flu$denominator)*100000,2)),
     subtype_c_rate = NA
)

FR_premerged_total$hsp_abs_covid <- left_join(preclean_fran_flu, preclean_fran_covid, by = c("year", "week"))$hsp_abs_covid19
FR_premerged_total$hsp_rate_covid19 <- round((FR_premerged_total$hsp_abs_covid/FR_premerged_total$denominator)*100000,2)

write_csv(FR_premerged_total, "data/premerged_data/FR_premerged_total.csv")