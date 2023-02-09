# Scripts

Our code uses R (v 4.2.2) and R Studio

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
&nbsp; <br>
In this section, there are two types of scripts:
- Cleaning: Scripts used to scrape data from raw files and convert it to the finalised format
- Analysis: Scripts used to build figures and tables for the final report
