# Merge script

dir <- "data/premerged_data"

files <- list.files(dir, pattern = "*.csv", full.names = TRUE)
namesEnv <- gsub("data/premerged_data/", "", gsub(".csv", "", files))

fileList <- lapply(setNames(files, namesEnv), read.csv)

list2env(fileList, envir = .GlobalEnv)

bind_rows(fileList)


varClass <- data.frame(lapply(fileList, function(x) sapply(x, class)))


# I need to continue with the following

var_types <- c(col1 = "numeric", col2 = "character", col3 = "factor")

declare_types <- function(df_list, var_types) {
     
     # Declare variable types
     for(i in 1:length(df_list)) {
          df_list[[i]] <- df_list[[i]][, var_types]
     }
     
     return(df_list)
}
