library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
fileList <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_amphibians_062020.csv"
folderResults <- "/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/Amphibians/"
rstSpecie <- raster(paste0(folderResults, 'sumTotalAmphibians.tif'))
title <- 'Amphibians_Relative_Threat'
folderGraph <- "graphics03"
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
## ajuste directo al porcentaje
if(op == 1){
  rstThreatment <- (2*rst_EN + rst_VU + 3*rst_CR)/(rstSpecie + 2*rst_CR + rst_EN)
}else{
  rstThreatment <- (2*rst_EN + rst_VU + 3*rst_CR)/rstSpecie
  maxV <- maxValue(rstThreatment)
  minV <- maxValue(rstThreatment)
  rstThreatment <- (rstThreatment - minV)/(maxV -minV)
}
##
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
myMap3(rstThreatment, rstMask, crs_to, title, 1, paste0(folderResults, folderGraph))
##plottin threatment in each order
rstMask2 <- rstMask
rstMask2[!is.na(rstMask2)] <- 0
for(order in orders){
  print(paste0('Order :', order))
  rstSpecie <- raster(paste0(folderResults, 'sum_',order,'.tif'))
  if(file.exists(paste0(folderResults, 'sum_',order,'_EN.tif'))){
    rst_EN <- raster(paste0(folderResults, 'sum_',order,'_EN.tif'))
  }else{
    rst_EN <- rstMask2}
  if(file.exists(paste0(folderResults, 'sum_',order,'_VU.tif'))){
    rst_VU <- raster(paste0(folderResults, 'sum_',order,'_VU.tif'))
  }else{
    rst_VU <- rstMask2}
  if(file.exists(paste0(folderResults, 'sum_',order,'_CR.tif'))){
    rst_CR <- raster(paste0(folderResults, 'sum_',order,'_CR.tif'))
  }else{
    rst_CR <- rstMask2}
  if(op == 1){
    rstThreatment <- (2*rst_EN + rst_VU + 3*rst_CR)/(rstSpecie + 2*rst_CR + rst_EN)
  }else{
    rstThreatment <- (2*rst_EN + rst_VU + 3*rst_CR)/rstSpecie
    maxV <- maxValue(rstThreatment)
    minV <- maxValue(rstThreatment)
    rstThreatment <- (rstThreatment - minV)/(maxV -minV)
  }
  title2 <- paste0(title,'_',order)
  myMap3(rstThreatment, rstMask, crs_to, title2, 1, paste0(folderResults, folderGraph))
}
