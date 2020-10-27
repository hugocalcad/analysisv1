library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "reptiles"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_",className,"_082020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2,'_Relative_Threat')
scaleFun <- "fisher"
op <- 2
folderGraph <- "graphics05_opC"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA02/mask_50.tif')
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
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
if(op == 1){
  rstThreatment <- (rst_EN + rst_VU + rst_CR)/(rstSpecie-rst_DD)
  formula <- "(CR+EN+VU)/(Richness-DD)"
}else{
  rstThreatment <- (rst_EN + rst_VU + rst_CR +rst_DD)/(rstSpecie)
  formula <- "(CR+EN+VU+DD)/(Richness)"
}
myMapGeneric(rstThreatment, rstMask, crs_to, title, formula, 1, paste0(folderResults, folderGraph), 
             oceans = 0, annotateScale = "", scaleFun = scaleFun, 
             nameSave = paste0("sum_all_",title, "_0",scaleFun))
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
  if(file.exists(paste0(folderResults, 'sum_',order,'_DD.tif'))){
    rst_DD <- raster(paste0(folderResults, 'sum_',order,'_DD.tif'))
  }else{
    rst_DD <- rstMask2}
  title2 <- paste0(title,'_',order)
  if(op == 1){
    rstThreatment <- (rst_EN + rst_VU + rst_CR)/(rstSpecie-rst_DD)
    formula <- "(CR+EN+VU)/(Richness-DD)"
  }else{
    rstThreatment <- (rst_EN + rst_VU + rst_CR +rst_DD)/(rstSpecie)
    formula <- "(CR+EN+VU+DD)/(Richness)"
  }
  myMapGeneric(rstThreatment, rstMask, crs_to, title, formula, 1, paste0(folderResults, folderGraph), 
               oceans = 0, annotateScale = "", scaleFun = scaleFun, 
               nameSave = paste0("sum_all_",title, "_", scaleFun))
}
