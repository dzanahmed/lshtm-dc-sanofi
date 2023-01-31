# Experimental

epi_weeks <- read_csv(file="data/epi_weeks.csv")

# This is to create a table of season dates in the NH
NH_seasons <- epi_weeks |>
     filter(epi_wk_no == 20 | epi_wk_no == 40) |> 
     filter(row_number() != 1 & row_number() != 12) |>
     mutate(season_threshold = case_when(epi_wk_no == 40 ~ "Start",
                                         TRUE ~ "End")) |>
     pivot_wider(id_cols = year,
                 names_from = season_threshold,
                 values_from = epi_dates) |>
     mutate(year = as.character(paste0(year, "/", year + 1)))

# Shift End upward by one row to correct for merging on year
NH_seasons$End <- data.table::shift(NH_seasons$End, n = -1) 

# Drop 2023/2024
NH_seasons <- NH_seasons[1:nrow(NH_seasons) - 1,]

# Artificial ending of the season, for better plot rendering
NH_seasons[5,3] <- as.Date("2023-03-01")

br <- sort(c(NH_seasons$End, NH_seasons$Start))

br <-
     c(
          "2017-05-14", # end 16-17
          "2017-10-01", # beginning 17-18
          "2018-05-13", # end 17-18
          "2018-09-30", # beginning 18-19
          "2019-05-12", # ending 18-19
          "2022-10-02", # beginning 22-23
          "2023-03-01"
     )