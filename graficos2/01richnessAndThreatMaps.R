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
title <- paste0(className2) 
scaleFun <- "fisher"
folderGraph <- "graphics01"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA02/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## ploting total sum (richness distribution)
source("../fun/richnessMap.R")
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
#myMap2(rstSpecie, rstMask, crs_to, title, "", 1, paste0(folderResults, folderGraph), oceans = 0)
myMapGeneric(rstSpecie, rstMask, crs_to, title, "", 1, paste0(folderResults, folderGraph), 
             oceans = 0, annotateScale = "", scaleFun = scaleFun, 
             nameSave = paste0("sum_all_",title,"_0",scaleFun,"v2"))
## plotting order
df <- read.csv(fileList)
orders <- df %>% distinct(orderName) %>% pull()
for(order in orders){
  print(paste0('Order :', order))
  rstSpecie <- raster(paste0(folderResults, 'sum_',order,'.tif'))
  title2 <- paste0(title,'_',order)
  myMapGeneric(rstSpecie, rstMask, crs_to, title2, "", 1, paste0(folderResults, folderGraph), 
               oceans = 0, annotateScale = "", scaleFun = scaleFun, 
               nameSave = paste0("sum_all_",title2))
}
## plotting threatment
rst_EN <- raster(paste0(folderResults, 'sum_EN.tif'))
rst_VU <- raster(paste0(folderResults, 'sum_VU.tif'))
rst_CR <- raster(paste0(folderResults, 'sum_CR.tif'))
rstThreatment <- rst_EN + rst_VU + rst_CR
title <- paste0("Threatened_", title)
##calculate the number
rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
            "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
            "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
            "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")
df$rlCodes<-""
df$rlCodesNro<-""
for(i in 1:length(rlCodes)){
  df$rlCodes[which(df$redlistCategory == names(rlCodes[i]))] = rlCodes[i]
}
df3 <- df %>% filter(rlCodes %in% c("CR","EN","VU"))
anotateScale <- paste0("Threatened/Species = ",nrow(df3),"/",nrow(df), " = ", round((nrow(df3)/nrow(df))*100, 2), " %")
##
#myMap2_1(rstThreatment, rstMask, rstSpecie, crs_to, title,"(CR + EN +VU)", 1, paste0(folderResults, folderGraph), oceans = 0, anotateScale)
myMapGeneric(rstThreatment, rstMask, crs_to, title, "", 1, paste0(folderResults, folderGraph), 
             oceans = 0, annotateScale = "", scaleFun = scaleFun, 
             nameSave = paste0("sum_all_",title,"_0",sclaeFun))
myMapGeneric2(rstThreatment, rstMask, crs_to, title, "", 1, paste0(folderResults, folderGraph), 
             oceans = 0, annotateScale = "", scaleFun = scaleFun, 
             nameSave = paste0("sum_all_",title,"_0",sclaeFun))
myMapViridis(rstThreatment, rstMask, crs_to, title, "", 1, paste0(folderResults, folderGraph), 
             oceans = 0, annotateScale = "", scaleFun = scaleFun, 
             nameSave = paste0("sum_all_",title,"_0",sclaeFun))
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
  myMapGeneric(rstThreatment, rstMask, crs_to, title2, "", 1, paste0(folderResults, folderGraph), 
               oceans = 0, annotateScale = "", scaleFun = scaleFun, 
               nameSave = paste0("sum_all_",title2,"_0", scaleFun))
}
