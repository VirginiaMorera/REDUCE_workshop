# 0. Housekeeping ####
rm(list = ls())

library(rnaturalearth)
library(tidyverse)
library(sf)

# 1. Load data ####
data <- read.csv("Data/data.csv") %>% 
  dplyr::select(-X)
map <- ne_countries(scale = "medium", returnclass = "sf")

data_sf <- data %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

ggplot() + 
  geom_sf(data = map, fill = "lightgray", col ="darkgray") +
  geom_sf(data = data_sf) + 
  coord_sf(xlim = st_bbox(data_sf)[c(1,3)], st_bbox(data_sf)[c(2,4)], expand = TRUE) +
  theme_bw()
