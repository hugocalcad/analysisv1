library(ggspatial)
library(RColorBrewer)
library(raster)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
fileList <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_reptiles_062020.csv"
folderResults <- "/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/Reptiles/"
rstSpecie <- raster(paste0(folderResults, 'sumTotalReptiles.tif'))
title <- 'Reptiles_Relative_Threat'
op <- 2
folderGraph <- "graphics05_opC"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
## plotting order
df <- read.csv(fileList)
orders <- df %>% distinct(orderName) %>% pull()
## plotting threatment
rst_EN <- raster(paste0(folderResults, 'sum_EN.tif'))
rst_VU <- raster(paste0(folderResults, 'sum_VU.tif'))
rst_CR <- raster(paste0(folderResults, 'sum_CR.tif'))
rst_DD <- raster(paste0(folderResults, 'sum_DD.tif'))
rstRichnesThreath <- rst_EN + rst_VU + rst_CR
## creating the grid centroids
sf_world <- st_read("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/shps/countries2.shp")
sf_world_line <- st_cast(sf_world, "MULTILINESTRING")
tif <- st_as_stars(rstMask)
sf=st_as_sf(tif)
sf2 <- st_centroid(sf)

dist <- st_distance(sf_world_line, sf2)

df <- data.frame(dist = as.vector(dist)/1000,  st_coordinates(sf2))

col_dist <- brewer.pal(11, "RdGy")

ggplot(df, aes(X, Y, fill = dist))+ #variables
  geom_tile()+ #geometry
  scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
  labs(fill = "Distance (km)")+ #legend name
  theme_void()+ #map theme
  theme(legend.position = "bottom") #legend position
