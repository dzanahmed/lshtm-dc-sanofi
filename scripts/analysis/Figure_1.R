library(tidyverse)

data <- read.csv(file='data/merged_data/merged_data.csv')

# Overall hospitalization by virus (Influenza, RSV, COVID-19) for the NH and SH 
# (complete time series for full time period: 2016 - 2023)
# Design: line graph
# X-axis: 2016 - 2023 with a second x-axis label that identifies the seasons

plot1 <- data |> select(country:hsp_rate_covid19)

plot1 |> ggplot(mapping=aes(week, hsp_rate))+
     geom_line()
