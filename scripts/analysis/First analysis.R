# This is the first basic analysis
# DZAJ 19 01 2022

library(tidyverse)

data <- read.csv(file = 'data/merged_data/first_output_2022_01_19.csv')

not_all_na <- function(x) any(!is.na(x))

unique(data$age_group)


data <- data |> select(where(not_all_na)) |>
     mutate(age_group = case_when(age_group == "Overall" ~ "Total", 
                                  age_group == "ALL" ~ "Total",
                                  TRUE ~ age_group))



totals <-
     data |> mutate(hemisphere = case_when(hemisphere == "sh" ~ "SH",
                                   TRUE ~ hemisphere)) |> 
     filter(age_group == "Total" & hemisphere=="NH")


totals

totals |> ggplot(mapping=aes(x=week, y=hsp_rate_flu, fill=country))+
     geom_col(position="dodge")+
     facet_wrap(. ~ year)+
     scale_y_sqrt()

# data <- data |> mutate(across(hsp_rate:subtype_c_rate, as.double))