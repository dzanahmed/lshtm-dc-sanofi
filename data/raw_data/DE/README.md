# Germany raw data

## Source 1: WHO FluNET/FluID: https://www.who.int/teams/global-influenza-programme/surveillance-and-monitoring/influenza-surveillance-outputs
- Data pulled from raw_data/FluNET
  - FluNET: https://github.com/dzanahmed/lshtm-dc-sanofi/blob/main/data/raw_data/FluNet/VIW_FNT.csv
  - FluID: https://github.com/dzanahmed/lshtm-dc-sanofi/blob/main/data/raw_data/FluNet/VIW_FID_EPI.csv

## Source 2: Robert Koch Institut SurvStat2.0 Query Tool: https://survstat.rki.de/Content/Query/Create.aspx
- Lab data (every year for every virus is a separate file)
  - After downloading the csv, top row and total row need to be deleted for proper formatting of the csv.
  - Age groups in the columns
  - Weeks in the rows

## Source 3: Robert Koch Institut GitHub COVID-19 hospitalization data: https://github.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland
- COVID-19 hospitalization data
  - Daily breakdown by age and Federal states
  - Provides absolute 7-day hospitalizations and 7-day incidence for appropriate age groups
