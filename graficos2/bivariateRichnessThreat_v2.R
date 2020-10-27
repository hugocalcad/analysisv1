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
rstRichnesThreathG <- projectRaster(rstRichnesThreath, crs = crs_to, method = "ngb")
rstSpecieG <-projectRaster(rstSpecie, crs = crs_to, method = "ngb")
rstMaskG <- projectRaster(rstMask, crs = crs_to, method = "ngb")
rstRichnesThreathG[is.na(rstMaskG)] <- NA
rstSpecieG[is.na(rstMaskG)] <- NA
#Create the bivariate palette
d <- expand.grid(x = seq(0, 1, 0.01), y = seq(0, 1, 0.01)) %>%
  mutate(fill_val = atan(y/x),
         transparency = x + y)
ggplot(d, aes(x, y, fill = fill_val, alpha = transparency)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "none")
ggplot(d, aes(x, y, fill = fill_val, alpha = transparency)) +
  geom_tile() +
  scale_fill_paletteer_c("harrypotter::hermionegranger") +
  theme_void() +
  theme(legend.position = "none")
ggplot(d, aes(x, y, fill = fill_val, alpha = transparency)) +
  geom_tile() +
  scale_fill_paletteer_c("scico::berlin") +
  theme_void() +
  theme(legend.position = "none")
ggplot(d, aes(x, y, fill = fill_val, alpha = transparency)) +
  geom_tile() +
  scale_fill_paletteer_c("harrypotter::harrypotter") +
  theme_void() +
  theme(legend.position = "none")
d <- d %>%
  mutate(mix2 = rgb(red = x, green = y, blue = 0.5))
p <- ggplot(d, aes(x, y, fill = fill_val, alpha = transparency)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "none")
pal_d <- ggplot_build(p)$data[[1]] %>%
  select(x, y, fill) %>%
  mutate(x = as.character(x),
         y = as.character(y))
pal_d$index <- seq(1:nrow(pal_d))
## working with the data
s <- stack(rstSpecieG, rstRichnesThreathG)
tif <- st_as_stars(s)
sf=st_as_sf(tif)
names(sf)<-c('richness', "threath", "geometry")
sf2 <- sf %>%
  mutate(x = as.character(round(scales::rescale(richness), 2)),
         y = as.character(round(scales::rescale(threath), 2))) %>%
  left_join(pal_d)
sf3 <- sf2%>% filter(x != "0")
map <- ggplot(sf3) +
  geom_sf(aes(fill = fill),# use thin white stroke for municipalities
          color = "white",
          size = 0.1) +
  guides(fill = "none")
legend <- ggplot(pal_d, aes(x, y)) +
  geom_tile(aes(fill = fill)) +
  scale_fill_identity() +
  labs(x = "Higher Percent Hispanic -->",
       y = "Higher Earnings -->") +
  theme_void() +
  theme(axis.title = element_text(size = 4),
        axis.title.y = element_text(angle = 90)) +
  coord_fixed()
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.02, 0.05, 0.15, 0.15)

rstMaskG2 <- rstRichnesThreathG
values(rstMaskG2) <- sf2$index
