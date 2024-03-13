## Define path
setwd("/path/to/the/folder")

## Library
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(RColorBrewer)
library(cowplot)


# init --------------------------------------------------------------------

adm <- readRDS("adm.rds")
swe <- filter(adm, country=="Sweden")
dat_moose <- read.csv2("moose_dat.csv") %>%
  mutate(date = ymd(Observation_date) + (ymd("2024-03-01") - max(ymd(Observation_date))),
         year = year(date))

# moose in Sweden ---------------------------------------------------------

moose1 <- ggplot(swe) +
  geom_sf(fill ="ivory2", color = "black") +
  geom_jitter(data = dat_moose, aes(x = Longitude, y = Latitude, fill = Species), shape = 21, size = 2, alpha = 0.5) +
  scale_fill_manual(values = c("darkgreen", "darkorange")) +
  facet_wrap(~Species) +
  guides(fill=guide_legend(title = "Species")) +
  theme_classic() +
  theme(legend.position = "none",
        strip.text = element_text(size = 14),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())

moose2 <- ggplot(dat_moose) +
  geom_histogram(aes(date, fill = Species), binwidth = 15) +  
  scale_fill_manual(values = c("darkgreen", "darkorange")) +
  theme_classic() +
  theme(legend.text = element_text(size = 14),
  legend.title = element_text(size = 0),
  axis.text = element_text(size = 14),
  panel.grid.major.y =  element_line())

moose <- plot_grid(moose1, moose2, rel_widths = c(0.4, 0.6))
pdf("Figure_step1_moose.pdf", width = 15, height = 8)
moose
dev.off()

