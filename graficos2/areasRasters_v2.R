library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(stars)
library(tidyverse)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "reptiles"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_",className,"_082020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/sumAreas/")
folderGraph <- "graphics09"
scaleFun <- "fisher"
title <- paste0(className2) 
subtitle ='Coast Distance - Relative Threat'
#folderGraph <- "graphics01"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA02/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../fun/richnessMap.R")
##
dist <- c(1000,5000,10000,15000,20000,30000,40000,50000)
for(d in dist){
  d2 <- format(d, big.mark = ",")
  rasterOut <- raster(paste0(folderResults, "sum_",className2,"_",d,"km.tif"))
  title<-paste0(className2,"_",d2)
  title2<-paste0(className2,"_",d)
  subtitle<-paste0("Species richness less than ",d2 ," square kilometers")
  myMapViridis(rasterOut, rstMask, crs_to = crs_to , title, 
      subtitle, palette = 2, folderResults, oceans = 1,
      annotateScale = "", scaleFun = scaleFun, 
      nameSave = paste0("sum_all_",title2, "_0fisher"))
}
