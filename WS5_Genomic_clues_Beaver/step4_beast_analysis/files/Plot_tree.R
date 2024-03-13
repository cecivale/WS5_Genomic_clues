## Define path
setwd("path/to/folder")

## Library
library(ggtree)
library(ggplot2)
library(treeio)
library(dplyr)
library(lubridate)
library(viridis)
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("ggtree")




### Beaver -------------------------------------------------------------------


# Load tree
beast <- read.beast("beaver.summary.tree")

# Plot tree with long labels 
tree_figure <- ggtree(
  tr = beast, 
  mrsd="2024-03-01", 
  color="gray30",
  as.Date = T,
  aes()) +
  geom_tiplab(aes(), size=4) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  coord_cartesian(clip = 'off') +
  theme_tree2(plot.margin=margin(6, 200, 2, 20)) + # top, right , bottom, left 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1, size=40),
        panel.grid.major.x =  element_line(colour = "grey80", linewidth = 0.2, linetype = 2))
#options(ignore.negative.edge=TRUE)
show(tree_figure)
#ggsave("Results/beaver_mcc_tree1.tiff", tree_figure, width = 25, height = 30, units = "cm") 
ggsave("beaver_summary_tree1.pdf", tree_figure, width = 20, height = 30) 



# Plot tree with no labels and rate per branch
beast_data <- treeio::get.data(beast)

summary(beast_data$rate)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000716 0.0002510 0.0002988 0.0034174 0.0003944 1.0000000 
beast@data$rate <- ifelse(beast@data$rate== 1, NA, beast_data$rate)

tree_figure <- ggtree(
  tr = beast, 
  mrsd="2024-03-01", 
  as.Date = T,
  aes(color=log(rate)), size = 3) +
  theme_tree2() + # top, right , bottom, left 
  scale_colour_viridis(name = "log(mean clock rate)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_tippoint(aes(), size = 4, shape = 16, color = "black") +
  theme(legend.position="right",
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25), 
        axis.text.x = element_text(angle = 45, hjust = 1, size=40),
        panel.grid.major.x =  element_line(colour = "grey80", linewidth = 0.2, linetype = 2))
#options(ignore.negative.edge=TRUE)
show(tree_figure)
ggsave("beaver_summary_tree2.pdf", tree_figure, width = 20, height = 30) 


