# Scripts

Our code was written using R (v4.2.2) and R Studio.

In this section, there are two types of scripts:
- Cleaning: Scripts used to scrape data from raw files and convert it to the finalised format
- Analysis: Scripts used to build figures and tables for the final report

Before running any script, please ensure the following:
1. Your working directory is set to the lshtm-dc-sanofi folder
  - You can do this either through selecting the 'More' options section in the Files section of R-Studio
  - Or, you can edit `setwd()` to the correct path on your computer to the folder
  
2. You have installed all the required packages, these are:
  - `tidyverse`
  - `cowplot`
  - `patchwork`
  - `ggsankey`
  - `lubridate`

3. For any package not installed, uncomment the relevant `#install.packages('package')` in the file you are trying to run
