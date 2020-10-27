library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(egg)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
className <- "Reptiles"
className2 <- stringr::str_to_title(className)
fileList1 <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_",className,"_082020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")
rstSpecie1 <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
names(rstSpecie1) <- className
className <- "Amphibians"
className2 <- stringr::str_to_title(className)
fileList2 <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_",className,"_082020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")
rstSpecie2 <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
names(rstSpecie2) <- className
className <- "Birds"
className2 <- stringr::str_to_title(className)
fileList3 <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_",className,"_082020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")
rstSpecie3 <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
names(rstSpecie3) <- className
className <- "Mammals"
className2 <- stringr::str_to_title(className)
fileList4 <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_",className,"_082020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")
rstSpecie4 <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
names(rstSpecie4) <- className

folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/")
folderGraph <- "graphicsGeneral"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA02/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
title <- "All Classes"
myGraphLatitudeAll(rstSpecie1,rstSpecie2,rstSpecie3,rstSpecie4, rstMask, crs_to, title, "", paste0(folderResults, folderGraph), fun = "median")
myGraphLatitudeAllRelative(rstSpecie1,rstSpecie2,rstSpecie3,rstSpecie4, rstMask, crs_to, title, "", paste0(folderResults, folderGraph), fun = "median")
