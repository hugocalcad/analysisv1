library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(egg)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
fileList <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_reptiles_062020.csv"
folderResults <- "/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/Reptiles/"
rstSpecie <- raster(paste0(folderResults, 'sumTotalReptiles.tif'))
title <- 'Reptiles'
folderGraph <- "graphics04"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
myGraph1(rstSpecie, rstMask, crs_to, title, "", paste0(folderResults, folderGraph))

