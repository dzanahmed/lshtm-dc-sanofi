library(tidyverse)
library(RColorBrewer)
library(ggsankey)
library(DiagrammeR)

data_journey <- read_csv(file='data/other/data_journey.csv')
colnames(data_journey)

df <- data_journey |> make_long("Database (Raw data source)", "Processed data",
                                "Country data", "Merged data") 

#df$node[df$node=="Robert Koch Institut SurvStat 2.0"] <- "Robert Koch Institut\n SurvStat 2.0"
                                
sankey_data_journey <- ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
     geom_sankey(flow.alpha = 1.0, node.color = 1, width=0.5) +
     geom_sankey_label(size = 2.5, color = 1, fill = "white", alpha=0.99) +
     scale_fill_viridis_d(option="plasma") +
     theme_sankey(base_size = 12) +
     guides(fill = guide_legend(title = "Title"))+
     theme(legend.position = "none",
           axis.title = element_blank())

sankey_data_journey

ggsave(
     sankey_data_journey,
     file = 'output/data_journey_sankey_flowchart.png',
     width = 12,
     height = 4.2
)


write_csv(file="data/other/data_journey2.csv", df)

# Alterantive -------------------------------------------------------------



grViz("digraph flow {
graph [layout = dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish

# raw nodes
node [shape = oval, style=filled, fillcolor = '#dddddd']
uk_data [label='SARI Watch']
us_flu_data [label = 'CDC\n FluSurv-NET']
us_rsv_data [label = 'CDC\n RSV-NET']
us_covid_data [label = 'CDC\n COVID-NET']
de_covid_hsp_data [label = 'RKI\n GitHub']
de_covid_lab_data [label = 'RKI\n SurvStat 2.0']
us_flu_data [label = 'CDC\n FluSurv-NET']
us_rsv_data [label = 'CDC\n RSV-NET']
us_covid_data [label = 'CDC\n COVID-NET']
de_covid_hsp_data [label = 'RKI\n GitHub']
de_covid_lab_data [label = 'RKI\n SurvStat 2.0']
au_covid [label='Australian\n MoH']
cl_moh_data [label = 'Chile MoH']
cl_mos_data [label = 'Chile MOS']
fr_data [label = 'Sante Publique']
flunet_data [label = 'WHO FluNet']

# Processed nodes
node [shape = box, style = filled, fillcolor = '#8ac0a7']

uk_proc [label = 'UK\n Influenza,\n RSV, \nCOVID-19']
us_flu_proc [label =  'US\n Influenza']
us_rsv_proc [label =  'US\n RSV']
us_covid_proc [label =  'US\n COVID-19']
de_covid_hsp_proc [label =  'Germany\n COVID-19\n (Hospital data)']
de_covid_lab_proc [label =  'Germany\n COVID-19\n (Lab data)']
au_covid_proc [label = 'Australia\n COVID-19']
cl_covid_proc [label = 'Chile\n COVID-19\n (age-stratified)']
cl_flu_rsv_covid_proc [label = 'Chile\n Influenza,\n RSV, \nCOVID-19']
fr_covid_proc [label='France\n COVID-19']
fr_flu_rsv [label='France\n Influenza, RSV']
au_flu_rsv [label='Australia\n Influenza, RSV']
de_flu_rsv [label='Germany\n Influenza, RSV']

# Premerged data
node [shape = box, style=filled, fillcolor='#96a0c7']
cl_premerged [label='Chile\nCleaned data'] 
de_premerged [label='Germany\nCleaned data'] 
fr_premerged [label='France\nCleaned data'] 
au_premerged [label='Australia\nCleaned data'] 
us_premerged [label='United States\nCleaned data'] 
uk_premerged [label='United Kingdom\nCleaned data'] 

#Merged data
node [shape = box, style=filled, fillcolor='#e1926c']
merged [label='Final aggregated dataset']

# edge definitions with the node IDs
uk_data -> uk_proc -> uk_premerged -> merged
us_flu_data -> us_flu_proc -> us_premerged-> merged
us_rsv_data -> us_rsv_proc -> us_premerged-> merged
us_covid_data -> us_covid_proc -> us_premerged-> merged
de_covid_hsp_data -> de_covid_hsp_proc -> de_premerged-> merged
de_covid_lab_data -> de_covid_lab_proc -> de_premerged-> merged
au_covid -> au_covid_proc -> au_premerged-> merged
cl_moh_data -> cl_covid_proc -> cl_premerged-> merged
cl_mos_data -> cl_flu_rsv_covid_proc -> cl_premerged-> merged
fr_data -> fr_covid_proc -> fr_premerged-> merged
flunet_data -> fr_flu_rsv-> fr_premerged-> merged
flunet_data -> au_flu_rsv -> au_premerged-> merged
flunet_data -> de_flu_rsv -> de_premerged-> merged

}")
