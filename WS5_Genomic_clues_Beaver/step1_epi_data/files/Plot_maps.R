## Define path
setwd("/path/to/the/folder")

## Library
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(RColorBrewer)


# init --------------------------------------------------------------------

adm <- readRDS("adm.rds")
swe <- filter(adm, country=="Sweden")
dat_beaver <- read.csv2("beaver_dat.csv") %>%
  mutate(date = ymd(OBStartDate) + (ymd("2024-03-01") - max(ymd(OBStartDate))),
         Year = year(date)) %>% filter(Year < "2024")


# beaver in Scandinavia ---------------------------------------------------

beaver1 <- ggplot(adm) +
    geom_sf(fill ="ivory2", color = "black") +
    geom_point(data = dat_beaver, aes(x = Longitude, y = Latitude), size = 2, shape = 21, fill = "darkgreen", col = "black") +
    # coord_sf(xlim=c(-8, 30), ylim=c(35, 54)) +
    facet_wrap("Year", ncol = 4) +
    #guides(fill=guide_legend(title = "Country")) +
    theme_classic() +
    theme(legend.position = "none",
          strip.text = element_text(size = 30),
          axis.title = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank())

beaver2 <- ggplot(dat_beaver) +
  geom_histogram(aes(date), fill = "darkgreen", binwidth = 15) +  
  theme_classic() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 0),
        axis.text = element_text(size = 20),
        panel.grid.major.y =  element_line())

beaver <-  plot_grid(beaver1, beaver2, ncol = 1, rel_heights = c(0.7, 0.3))

pdf("Figure_step1_beaver.pdf", width = 18, height = 20)
beaver
dev.off()
