# Merge script
# Dzan 20 01 2023

library(dplyr)

source(file='scripts/cleaning/helper.R') # Variable order, names and types are sourced from here

dir <- "data/premerged_data" # Pull data from this folder

files <- list.files(dir, pattern = "*.csv", full.names = TRUE) # read filenames of csvs
namesEnv <- gsub("data/premerged_data/", "", gsub(".csv", "", files)) # rename dfs in env

fileList <- lapply(setNames(files, namesEnv), read.csv) # Create a list of dfs

list2env(fileList, envir = .GlobalEnv) # Send dfs to environment

varClass <- data.frame(lapply(fileList, function(x) sapply(x, class))) 
# varClass is a table that shows what is the type of variable in every dataframe

merged_data <- bind_rows(fileList) # Bind them

epiweeks <- read.csv(file='data/epiweeks_withdate.csv')

epiweeks$start_date <- as.Date(epiweeks$start_date)

# All the last columns (after age_group) are numeric. Also, assign new IDs

merged_data <-
     merged_data |> mutate(across(hsp_rate:subtype_c_rate, as.double)) |>
     mutate(id = row_number())

merged_data <- merge(merged_data, epiweeks)

readr::write_csv(merged_data, 'data/merged_data/merged_data.csv') # Write a CSV


# Goal is to set variable types in each dataframe from a list (preferrably with a function)
# which will apply variable types to all dataframes

# Names and types of variables are declared in helper script 
# (objects order_header_premerge and var_type_header)

# Update 19/01/2022 afternoon - DZAJ:
# Made it work without this!

# -------------------------------------------------------------------------
# This is the part that I'm working on 

# 
# declare_types <- function(df_list, var_types) {
#      
#      # Declare variable types
#      for(i in 1:length(df_list)) {
#           df_list[[i]] <- df_list[[i]][, var_types]
#      }
#      
#      return(df_list)
# }
# 
# declare_types(fileList, var_type_header)
# 
# 
# colnames(fileList[[2]]) 
# names(var_type_header)
# 
# AU_premerged_covid[, var_type_header]
# 
# setequal(colnames(fileList[[2]]),
#          names(var_type_header))
# 
# fileList[[2]] <- fileList[[2]][, var_type_header]
# 
# 
# fileList[[1]][, c("id", "data_source")]
# 
# # First I will create a small list
# smallList <- list(AU_premerged_covid, AU_premerged_total)
# 
# sapply(AU_premerged_covid, typeof)
# 
# bind_rows(smallList)
