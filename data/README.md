# Data

Contains data from :australia:, :chile:, :de:, :fr:, :uk:, :us: & :united_nations: WHO across different stages. 

- **raw_data** - Data from online databases is placed in here and contains files exported from online sources in .csv or other tabular forms
  - Every country has its own subfolder
- **processed_data** contains files that are made as an output from cleaning scripts performed on raw data. 
  - Every country has its own subfolder
- **premerged_data** contains files that are a result of standardizing outputs from cleaned data to a premade template
  - Countries have separate files (usually one for total, and one for age stratified data)
- **merged_data** contains final standardized csv file
  - merged_data.csv



Additional csv files  
- data_journey.csv - for creation of Data processing flowchart / Sankey graph
- epi_weeks.csv - for standardizing weeks according to CDC MMWR standard
- template.csv - to provide structure for premerged_data
