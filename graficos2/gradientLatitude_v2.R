library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(egg)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
className <- "Reptiles"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS2/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphics04_borrar"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
myGraph1(rstSpecie, rstMask, crs_to, title, "", paste0(folderResults, folderGraph), fun = "mean")

# Data deficient latitude
rst_DD <- raster(paste0(folderResults, 'sum_DD.tif'))
title2 <- paste0(title, "_Data_Deficient")
#myMap(rst_DD, rstMask, crs_to, title2,"(DD)", 1, paste0(folderResults, folderGraph), oceans = 0)
myGraph1(rst_DD, rstMask, crs_to, title2, "", paste0(folderResults, folderGraph), fun = "mean")

# threat latitude (CR + EN + VU)
rst_EN <- raster(paste0(folderResults, 'sum_EN.tif'))
rst_VU <- raster(paste0(folderResults, 'sum_VU.tif'))
rst_CR <- raster(paste0(folderResults, 'sum_CR.tif'))
rst_Threat <- rst_VU +rst_EN + rst_CR
title2 <- paste0("Threatened_", title)
myGraph1(rst_Threat, rstMask, crs_to, title2, "(CR + EN + VU)", paste0(folderResults, folderGraph), fun = "mean")
