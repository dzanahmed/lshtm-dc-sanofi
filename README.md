# :chart_with_upwards_trend: Data Challenge Project 

:bookmark: LSHTM 2491 Data Challenge Module  
:mortar_board: MSc in Health Data Science 22/23

**Client:** Sanofi\
**Supervisor:** Dr Naomi Waterlow, Research Fellow, LSHTM

**Members**:
- Jessica Caterson [jjcato9](https://github.com/jjcato9)
- Dzan Ahmed Jesenkovic [dzanahmed](https://github.com/dzanahmed)
- Ignacio Leiva Escobar [IgnaLeiva](https://github.com/IgnaLeiva)
- Naomi Medina-Jaudes [naomedina](https://github.com/naomedina)
&nbsp;<br>
### :question: How do hospitalisations as a result of the tri-demic of RSV, influenza and SARS-CoV-2 in the 2022 post-pandemic season compare to the 2016 - 2019 pre-pandemic circulation of RSV and influenza?
:grey_question: Include analyses of different geographical locations (e.g. Northern/Southern hemisphere) and by age group.  
:grey_question: Would the assessment be different if burden is disaggregated by hospitalisations vs. laboratory confirmed cases? 

## :page_with_curl: Summary 
> The project idea was conceputalized by Sanofi, to compare hospitalizations during the current post-pandemic season with pre-pandemic seasons between 2016 and 2019.   
> Six countries were selected based on data availability and source reputability, with priority given to countries specified by Sanofi. Hospitalization data (rates or absolutes) were collected and standardised, and seasonality of hospitalizations were compared using time series analysis methods.   
> The results showed that hospitalizations due to RSV, influenza, and SARS-CoV-2 were higher than in pre-pandemic seasons, with similar rates of influenza and RSV hospitalizations and the addition of SARS-CoV-2 hospitalizations. The shift in hospitalization peaks led to 2-5 times higher hospitalizations, with adults over 65 and children under 5 having the highest hospitalization rates.   
> Report recommends reducing hospitalizations in future winter seasons through continued vaccination programs and public engagement in spread-reducing measures.   

## :gear: Requirements

- R 4.2.2 
- RStudio
- R Packages:
  - `tidyverse`
  - `cowplot`
  - `patchwork`
  - `lubridate`
  - `ggsankey`

## :desktop_computer: Running the scripts
You can fork or download the repository as a .zip file and extract it on your computer. 
It is recommended to load the `LSHTM-DC-Sanofi.Rproj` into your R environment to ensure reproducibility.
Otherwise, scripts can be run by setting the working directory into folder where repository files are located using `setwd()` command in R console.

All the dependancies can be installed by running the following code:

`install.packages("tidyverse")
install.packages("cowplot")
install.packages("patchwork")
install.packages("lubridate")
install.packages("devtools")
devtools::install_github("davidsjoberg/ggsankey")`

After these, you can run the scripts in scripts/cleaning for each country.
- Check `README.MD` in scripts folder to understand the workflow.
- In case new raw data is added to `data/raw_data`, running scripts for each country will result in updated processed and premerged datasets. 
- Finally, `merge.R` from `scripts/cleaning` can be run to coalesce the processed data to `data/merged_data/merged_data.csv`.

## :bar_chart: Script outputs
After the data has been coalesced into one aggregated dataset, scripts from scripts/analysis can be run to create the figures.
Figure outputs are placed in output folder, and exported as high-res PNG or PDF files. 

## :memo: Final report
Final report was created through Google Docs in the form of a research paper, exported to PDF and submitted through Moodle.
