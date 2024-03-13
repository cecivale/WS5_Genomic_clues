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



### Moose -------------------------------------------------------------------

# Load tree
beast <- read.beast("moose.summary.tree")


# Plot tree with long labels colored per deme
tree_figure <- ggtree(
  tr = beast, 
  mrsd="2024-03-01", 
  color="gray30",
  as.Date = T,
  aes()) +
  geom_tiplab(aes(color = type), size=3) + 
  scale_colour_manual(values = c("darkgreen", "darkorange"), name = "type") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  coord_cartesian(clip = 'off') +
  theme_tree2(plot.margin=margin(6, 100, 2, 20)) + # top, right , bottom, left 
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, hjust = 1, size=40),
        panel.grid.major.x =  element_line(colour = "grey80", linewidth = 0.2, linetype = 2))
#options(ignore.negative.edge=TRUE)
show(tree_figure)
ggsave("moose_summmary_tree1.pdf", tree_figure, width = 20, height = 30) 


# Plot tree with long labels colored per deme
tree_figure <- ggtree(
  tr = beast, 
  mrsd="2024-03-01", 
  as.Date = T,
  aes(color = type), size = 2) +
  geom_tippoint(aes(color = type), size = 4) + 
  scale_colour_manual(values = c("darkgreen", "darkorange"), name = "type") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  coord_cartesian(clip = 'off') +
  theme_tree2(plot.margin=margin(6, 100, 2, 20)) + # top, right , bottom, left 
  theme(legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size = 40), 
        axis.text.x = element_text(angle = 45, hjust = 1, size=40),
        panel.grid.major.x =  element_line(colour = "grey80", linewidth = 0.2, linetype = 2))
#options(ignore.negative.edge=TRUE)
show(tree_figure)
ggsave("moose_summary_tree2.pdf", tree_figure, width = 20, height = 30) 




