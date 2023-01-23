###### CREATE EPI TABLES ######

### 2016 ###

epi_dates <- seq(as.Date("2016/01/03"), as.Date("2023/12/23"), "7 days")

epi_wk_table_16 <- as.data.frame(epi_dates)
epi_wk_table_16$epi_wk_no <- rep(seq(1,52,1),times=8)

