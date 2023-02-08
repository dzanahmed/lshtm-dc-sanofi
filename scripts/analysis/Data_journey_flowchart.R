# This is to create a data processing flowchart as a Sankey diagram 
# and explain the workflow. 
# DiagrammeR has been used at first, however ggsankey solution proved more elegant.

### INSTALL PACKAGES // UNCOMMENT IF NECESSARY ----
# install.packages("tidyverse")
# install.packages("ggsankey")

# INIT --------------------------------------------------------------------

library(tidyverse)
library(ggsankey)

# Load and transform data -------------------------------------------------

# Call data journey CSV
data_journey <- read_csv(file='data/data_journey.csv')
colnames(data_journey)

# Pivot to long for ggsankey format, declare df
df <- data_journey |> make_long("Database (Raw data source)",
                                "Processed data",
                                "Country data",
                                "Merged data") 


# Create Sankey plot ---------------------------------------------------

# Create gg object + theming
sankey_data_journey <- ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
     geom_sankey(flow.alpha = 1.0, node.color = 0, width=0.5) +
     geom_sankey_label(size = 3.0, color = 1, fill = "white", alpha=0.99) +
     scale_fill_viridis_d(option="plasma") +
     theme_sankey(base_size = 12) +
     guides(fill = guide_legend(title = "Title"))+
     theme(legend.position = "none",
           axis.title = element_blank())

# See the output
sankey_data_journey


# Outputs -----------------------------------------------------------------

# This is for the report (wide 12x5)
ggsave(
     sankey_data_journey,
     file = 'output/Data_journey_flowchart/sankey_flowchart_report.png',
     width = 12,
     height = 5.8
)

# This is for the presentation (narrower 4:3 aspect ratio - almost square)
# Also increase the font size for legibility
ggsave(
        sankey_data_journey+
                geom_sankey_label(size = 3.0, color = 1, fill = "white", alpha=0.99),
        file = 'output/Data_journey_flowchart/sankey_flowchart_presentation.png',
        width = 8,
        height = 6
)