library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
fileList <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_amphibians_062020.csv"
folderResults <- "/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/Amphibians/"
rstSpecie <- raster(paste0(folderResults, 'sumTotalAmphibians.tif'))
title <- 'Amphibians'
folderGraph <- "graphics01"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## ploting total sum (richness distribution)
source("../fun/richnessMap.R")
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
myMap(rstSpecie, rstMask, crs_to, title, "", 1, paste0(folderResults, folderGraph), oceans = 0)
## plotting order
df <- read.csv(fileList)
orders <- df %>% distinct(orderName) %>% pull()
for(order in orders){
  print(paste0('Order :', order))
  rstSpecie <- raster(paste0(folderResults, 'sum_',order,'.tif'))
  title2 <- paste0(title,'_',order)
  myMap(rstSpecie, rstMask, crs_to, title2,"", 1, paste0(folderResults, folderGraph), oceans = 0)
}
## plotting threatment
rst_EN <- raster(paste0(folderResults, 'sum_EN.tif'))
rst_VU <- raster(paste0(folderResults, 'sum_VU.tif'))
rst_CR <- raster(paste0(folderResults, 'sum_CR.tif'))
rstThreatment <- rst_EN + rst_VU + rst_CR
title <- paste0("Threatened_", title)
myMap(rstThreatment, rstMask, crs_to, title,"(CR + EN +VU)", 1, paste0(folderResults, folderGraph), oceans = 0)
##plottin threatment in each order
rstMask2 <- rstMask
rstMask2[!is.na(rstMask2)] <- 0
for(order in orders){
  print(paste0('Order :', order))
  if(file.exists(paste0(folderResults, 'sum_',order,'_EN.tif')))
    rst_EN <- raster(paste0(folderResults, 'sum_',order,'_EN.tif'))
  else
    rst_EN <- rstMask2
  if(file.exists(paste0(folderResults, 'sum_',order,'_VU.tif')))
    rst_VU <- raster(paste0(folderResults, 'sum_',order,'_VU.tif'))
  else
    rst_VU <- rstMask2
  if(file.exists(paste0(folderResults, 'sum_',order,'_CR.tif')))
    rst_CR <- raster(paste0(folderResults, 'sum_',order,'_CR.tif'))
  else
    rst_CR <- rstMask2
  rstThreatment <- rst_EN + rst_VU + rst_CR
  title2 <- paste0(title,'_',order)
  myMap(rstThreatment, rstMask, crs_to, title2,"(CR + EN +VU)", 1, paste0(folderResults, folderGraph), oceans = 0)
}
