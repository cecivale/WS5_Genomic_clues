## Define path
setwd("/Users/cguinat/Documents/CR_INRAE/Conferences/SVEPM_2024/Workshop")

## Library
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(RColorBrewer)


### MERS-CoV -------------------------------------------------------------------

## Load epi data
epi_data <- read.csv("Data/mers-cov/epi-data.csv", sep=";")

summary(epi_data)
epi_data$Animal.type <- as.factor(epi_data$Animal.type)
epi_data$Species <- as.factor(epi_data$Species)

Sys.setlocale("LC_TIME", "C") #to convert in English
epi_data$Observation.date <- as.Date(epi_data$Observation.date, format="%d/%m/%Y")

epi_data$Country <- as.factor(epi_data$Country)

# Keep only those between 2013 and 2015 and remove those with NA
epi_data <- epi_data[epi_data$Observation.date < "2015-12-31",]
epi_data <- epi_data[epi_data$Observation.date > "2013-01-01",]
epi_data <- epi_data[complete.cases(epi_data$Observation.date),]

# Add human in the column
epi_data$Species_new <- ifelse(epi_data$Species =="Camelidae (Unidentified)", "Camel", "Human")
epi_data$Species_new <- as.factor(epi_data$Species_new)

summary(epi_data$Observation.date)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2013-01-26" "2014-04-30" "2015-02-21" "2014-12-15" "2015-06-15" "2015-12-29" 

summary(epi_data$Species_new)
# Camel Human 
# 11  1127 

summary(epi_data$Country)
# Algeria                                    Austria                                      China 
# 2                                          1                                          1 
# Egypt                                     France                                    Germany 
# 1                                          2                                          2 
# Greece                Iran  (Islamic Republic of)                                       Iraq 
# 1                                          8                                          5 
# Italy                                     Jordan                                     Kuwait 
# 1                                         26                                          6 
# Lebanon                                   Malaysia                                Netherlands 
# 1                                          1                                          2 
# Oman                                Philippines                                      Qatar 
# 8                                          1                                         15 
# Republic of Korea                               Saudi Arabia                                   Thailand 
# 173                                        803                                          1 
# Tunisia                                     Turkey U.K. of Great Britain and Northern Ireland 
# 1                                          1                                          3 
# United Arab Emirates                   United States of America                                      Yemen 
# 69                                          2                                          1 

tab <- epi_data %>% group_by(Country, Species_new) %>% summarize(count = n())


# Plot over time the data

cases_temp <- epi_data %>%  
  ggplot(aes(x=Observation.date, fill=Species_new)) +
  geom_histogram(binwidth=20, alpha = 0.8, color = "gray20") +
  facet_wrap(~Species_new, ncol=1, strip.position="right") +
  ylab("Number of MERS-CoV cases") +
  theme_classic() +
  scale_x_date(limits = c(ymd("2013-01-01"), ymd("2015-12-31")), date_breaks = "1 month", date_labels = "%b %y", expand = c(0,0)) +
  scale_fill_brewer(palette = "Set2", name ="Species") +
  #scale_fill_manual(name = "", values = c("slategray1","peachpuff"), labels = c("Domestic" = "Poultry farm", "Wild" = "Wild bird")) +
  theme(legend.position="none",
        #legend.box = "horizontal",
        #legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.grid.major.x =  element_line(colour = "grey80", linewidth = 0.2, linetype = 2),
        strip.text = element_text(size = 12),
        axis.text=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 

tiff("Results/Figure 1_merscov.tiff", width = 30, height = 10, units = "cm",
     compression = "lzw", res = 300)
cases_temp
dev.off()


# Plot epi data on maps

epi_data <- epi_data %>% mutate(y = format(Observation.date, "%Y"))

admShp <-read_sf("Data/Vector/countries.shp") 

cases_map <- ggplot(admShp) +
  geom_sf(fill ="ivory2", color = "black") +
  geom_point(data = epi_data, aes(x = Longitude, y = Latitude, fill = Species_new), size = 1.5, shape = 21) +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(Species_new ~ y) +
  guides(fill=guide_legend(title = "Species")) +
  theme_classic() +
  theme(legend.position = "none",
        #legend.text = element_text(size = 14),
        #legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())

tiff("Results/Figure 2_merscov.tiff", width = 30, height = 20, units = "cm",
     compression = "lzw", res = 300)
cases_map
dev.off()


## Load genetic data

gen_data <- read.csv("Data/mers-cov/MERS_CoV_10_274.csv", sep=";")
#284 obs
summary(gen_data)
gen_data$Host <- as.factor(gen_data$Host )
Sys.setlocale("LC_TIME", "C") #to convert in English
gen_data$Date <- as.Date(gen_data$Date, format="%Y-%m-%d")
summary(gen_data$Date)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
#"2012-04-15" "2013-08-05" "2014-05-04" "2014-05-27" "2015-03-02" "2015-09-17"         "80" 
#some sequences have only year and month!!!!
# Camel Human 
# 103  181

# Plot over time the data

gen_temp <- gen_data %>%  
  ggplot(aes(x=Date, fill=Host)) +
  geom_histogram(binwidth=20, alpha = 0.8, color = "gray20") +
  facet_wrap(~Host, ncol=1, strip.position="right") +
  ylab("Number of MERS-CoV sequences") +
  theme_classic() +
  scale_x_date(limits = c(ymd("2013-01-01"), ymd("2015-12-31")), date_breaks = "1 month", date_labels = "%b %y", expand = c(0,0)) +
  scale_fill_brewer(palette = "Set2", name ="Host") +
  #scale_fill_manual(name = "", values = c("slategray1","peachpuff"), labels = c("Domestic" = "Poultry farm", "Wild" = "Wild bird")) +
  theme(legend.position="none",
        #legend.box = "horizontal",
        #legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.grid.major.x =  element_line(colour = "grey80", linewidth = 0.2, linetype = 2),
        strip.text = element_text(size = 12),
        axis.text=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 

tiff("Results/Figure 1_merscov_gen.tiff", width = 30, height = 10, units = "cm",
     compression = "lzw", res = 300)
gen_temp
dev.off()





### Bluetongue -------------------------------------------------------------------

## Load data
epi_data <- read.csv("Data/bluetongue/OIEdataset.csv", sep=",")

summary(epi_data)
epi_data$Country <- as.factor(epi_data$Country)

Sys.setlocale("LC_TIME", "C") #to convert in English
#epi_data$Observation.date <- as.Date(epi_data$Observation.date, format="%d/%m/%Y")
epi_data$OBStartDate <- as.Date(epi_data$OBStartDate, format="%d/%m/%y")

summary(epi_data$OBStartDate)
#Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2006-08-14" "2007-08-20" "2007-09-14" "2009-08-03" "2007-11-16" "2018-09-01"

epi_data$Species <- factor(epi_data$Species)
summary(epi_data$Species)
#            Buffaloes       Camelidae          Cattle           Goats Pan troglodytes           Sheep   Sheep / goats 
#                   33               9               1            8603              40               1            5299              76 
#Wild species 
#           4 
summary(epi_data$Country)
# Austria, Republic of                          Belgium, Kingdom of 
# 14                                         1626 
# Cyprus, Republic of                               Czech Republic 
# 140                                           10 
# Denmark, Kingdom of                      France, French Republic 
# 18                                         3218 
# Germany                    Greece, Hellenic Republic 
# 1414                                            3 
# Hungary, Hungarian People's Republic                      Italy, Italian Republic 
#                                            1                                          106 
#                   Luxembourg, Grand Duchy of                  Netherlands, Kingdom of the 
#                                          304                                         6951 
#                           Norway, Kingdom of                         Spain, Spanish State 
#                                            4                                            2 
#                           Sweden, Kingdom of             Switzerland, Swiss Confederation 
#                                           31                                           79 
# United Kingdom of Great Britain & N. Ireland 
#                                          145 
                                         
# Plot over time the data
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(17)

cases_temp <- epi_data %>%  
  ggplot(aes(x=OBStartDate, fill=Country)) +
  geom_histogram(binwidth=50, alpha = 0.8, color = "gray20") +
  ylab("Number of BTV-8 cases") +
  theme_classic() +
  scale_x_date(limits = c(ymd("2006-01-01"), ymd("2018-12-31")), date_breaks = "6 month", date_labels = "%b %y", expand = c(0,0)) +
  scale_fill_manual(values = mycolors, name ="Countries") +
  #scale_fill_manual(name = "", values = c("slategray1","peachpuff"), labels = c("Domestic" = "Poultry farm", "Wild" = "Wild bird")) +
  theme(legend.position="right",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.grid.major.x =  element_line(colour = "grey80", linewidth = 0.2, linetype = 2),
        axis.text=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 

tiff("Results/Figure 1_bluetongue.tiff", width = 30, height = 10, units = "cm",
     compression = "lzw", res = 300)
cases_temp
dev.off()


# Plot data on maps

epi_data <- epi_data %>% mutate(y = format(OBStartDate, "%Y"))

admShp <-read_sf("Data/Vector/countries.shp") 

cases_map <- ggplot(admShp) +
  geom_sf(fill ="ivory2", color = "black") +
  geom_point(data = epi_data, aes(x = Longitude, y = Latitude, fill = Country), size = 1.5, shape = 21) +
  scale_fill_manual(values = mycolors) +
  coord_sf(xlim=c(-8, 30), ylim=c(35, 54)) +
  facet_wrap("y", ncol = 3) +
  guides(fill=guide_legend(title = "Country")) +
  theme_classic() +
  theme(legend.position = "none",
        strip.text = element_text(size = 14),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())

tiff("Results/Figure 2_bluetongue.tiff", width = 30, height = 30, units = "cm",
     compression = "lzw", res = 300)
cases_map
dev.off()
