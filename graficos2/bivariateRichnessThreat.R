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
rstSpecieG[rstSpecieG == 0] <- NA
#rstRichnesThreath[rstRichnesThreath == 0] <- NA
s <- stack(rstSpecieG, rstRichnesThreathG)
v <- data.frame(values(s))
v$index <- seq(1: nrow(v))
v2 <- v %>% drop_na()
names(v)<-c('richness', "threath")
tif=st_as_stars(s)
sf=st_as_sf(tif)
sf2 <- sf%>% drop_na()
names(sf2)<-c('richness', "threath", "geometry")

#data <- bi_class(sf2, x = richness, y = threath, style = "quantile", dim = 3)

###bivariate color TEST
# create 3 buckets for gini
quantiles_richness <- sf2 %>%
  pull(richness) %>%
  unique() %>%
  quantile(probs = seq(0, 1, length.out = 5))

# create 3 buckets for mean income
quantiles_threat <- sf2 %>%
  pull(threath) %>%
  unique %>%
  quantile(probs = seq(0, 1, length.out = 5))

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
# ["#e8e8e8", "#bddede", "#8ed4d4", "#5ac8c8", 
#   "#dabdd4", "#bdbdd4", "#8ebdd4", "#5abdc8",   
#   "#cc92c1", "#bd92c1", "#8e92c1", "#5a92c1", "
#   #be64ac", "#bd64ac", "#8e64ac", "#5a64ac"]
bivariate_color_scale <- tibble(
  "3 - 4" = "#5a64ac", # high inequality, high income
  "4 - 4" = "#8e64ac", # high inequality, high income
  "2 - 4" = "#bd64ac",
  "1 - 4" = "#be64ac", # low inequality, high income
  "4 - 3" = "#5a92c1", # high inequality, high income
  "3 - 3" = "#8e92c1", # high inequality, high income
  "2 - 3" = "#bd92c1",
  "1 - 3" = "#cc92c1", # low inequality, high income
  "4 - 2" = "#5abdc8",
  "3 - 2" = "#8ebdd4",
  "2 - 2" = "#bdbdd4", # medium inequality, medium income
  "1 - 2" = "#dabdd4",
  "4 - 1" = "#5ac8c8", # high inequality, low income
  "3 - 1" = "#8ed4d4", # high inequality, low income
  "2 - 1" = "#bddede",
  "1 - 1" = "#e8e8e8" # low inequality, low income
) %>%
  gather("group", "fill")
# cut into groups defined above and join fill
sf2 %<>%
  mutate(
    gini_quantiles = cut(
      richness,
      breaks = quantiles_richness,
      include.lowest = TRUE
    ),
    mean_quantiles = cut(
      threath,
      breaks = quantiles_threat,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(gini_quantiles), "-",
      as.numeric(mean_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")

map <- ggplot(data = sf2) +
  geom_sf(
    aes(
      fill = fill
    ),# use thin white stroke for municipalities
    color = "white",
    size = 0.1
 
  ) +
  # as the sf object municipality_prod_geo has a column with name "fill" that
  # contains the literal color as hex code for each municipality, we can use
  # scale_fill_identity here
  scale_fill_identity() +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Reptiles - Species Richness vs Threath",
       subtitle = paste0("Richness -> numbre of species",
                         "Threat -> numbre of species treathened ")) +
  # add the theme
  theme_map()

bivariate_color_scale %<>%
  separate(group, into = c("richness", "threath"), sep = " - ") %>%
  mutate(richness = as.integer(richness),
         threath = as.integer(threath))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = richness,
      y = threath,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Richness⟶️",
       y = "Threat ⟶️") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)
