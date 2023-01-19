# Merge script

library(dplyr)

source(file='scripts/cleaning/helper.R') # Variable order, names and types are sourced from here

dir <- "data/premerged_data" # Pull data from this folder

files <- list.files(dir, pattern = "*.csv", full.names = TRUE) # read filenames of csvs
namesEnv <- gsub("data/premerged_data/", "", gsub(".csv", "", files)) # rename dfs in env

fileList <- lapply(setNames(files, namesEnv), read.csv) # Create a list of dfs

list2env(fileList, envir = .GlobalEnv) # Send dfs to environment


varClass <- data.frame(lapply(fileList, function(x) sapply(x, class))) 
# varClass is a table that shows what is the type of variable in every dataframe

# THIS IS WHERE IT BREAKS
bind_rows(fileList) 

# Goal is to set variable types in each dataframe from a list (preferrably with a function)
# which will apply variable types to all dataframes

# Names and types of variables are declared in helper script 
# (objects order_header_premerge and var_type_header)




# -------------------------------------------------------------------------
# This is the part that I'm working on


declare_types <- function(df_list, var_types) {
     
     # Declare variable types
     for(i in 1:length(df_list)) {
          df_list[[i]] <- df_list[[i]][, var_types]
     }
     
     return(df_list)
}

declare_types(fileList, var_type_header)


colnames(fileList[[2]]) 
names(var_type_header)

AU_premerged_covid[, var_type_header]

setequal(colnames(fileList[[2]]),
         names(var_type_header))

fileList[[2]] <- fileList[[2]][, var_type_header]


fileList[[1]][, c("id", "data_source")]
