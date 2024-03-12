## Define path
setwd("/Users/cguinat/Documents/CR_INRAE/Conferences/SVEPM_2024/Workshop")

## Library
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(RColorBrewer)


# init --------------------------------------------------------------------

adm <- readRDS("Script/adm.rds")
swe <- filter(adm, country=="Sweden")
dat_moose <- read.csv2("Script/moose_dat.csv")
dat_beaver <- read.csv2("Script/beaver_dat.csv")



# moose in sweden ---------------------------------------------------------

moose <- ggplot(swe) +
    geom_sf(fill ="ivory2", color = "black") +
    geom_point(data = dat_moose, aes(x = Longitude, y = Latitude, fill = Species), shape = 21, size = 5) +
    scale_fill_manual(values = c("darkgreen", "darkorange")) +
    facet_grid(Species ~ year) +
    guides(fill=guide_legend(title = "Species")) +
    theme_classic() +
    theme(legend.position = "none",
          #legend.text = element_text(size = 14),
          #legend.title = element_text(size = 14),
          strip.text = element_text(size = 30),
          axis.title = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank())

pdf("Results/Figure_step1_moose.pdf", width = 18, height = 25)
moose
dev.off()


# beaver in scandanavia ---------------------------------------------------

beaver <- ggplot(adm) +
    geom_sf(fill ="ivory2", color = "black") +
    geom_point(data = dat_beaver, aes(x = Longitude, y = Latitude), size = 2, shape = 21, fill = "darkgreen", col = "black") +
    # coord_sf(xlim=c(-8, 30), ylim=c(35, 54)) +
    facet_wrap("Year", ncol = 3) +
    #guides(fill=guide_legend(title = "Country")) +
    theme_classic() +
    theme(legend.position = "none",
          strip.text = element_text(size = 30),
          axis.title = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank())

pdf("Results/Figure_step1_beaver.pdf", width = 18, height = 25)
beaver
dev.off()
