library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(egg)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "reptiles"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphics04"
fun = "median"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA02/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
myGraphLatitude(rstSpecie, rstMask, crs_to, title, "", paste0(folderResults, folderGraph), fun = fun)
myGraphLatitudeGround( rstMask, crs_to, paste0(folderResults, folderGraph))
# Data deficient latitude
rst_DD <- raster(paste0(folderResults, 'sum_DD.tif'))
title2 <- paste0(title, "_Data_Deficient")
myMapGeneric(rst_DD, rstMask, crs_to, title2,"(DD)", 1, paste0(folderResults, folderGraph), oceans = 0, 
             annotateScale = "", scaleFun = "equal", nameSave= title2)
myGraphLatitude(rst_DD, rstMask, crs_to, title2, "", paste0(folderResults, folderGraph), fun = fun)

# threat latitude (CR + EN + VU)
rst_EN <- raster(paste0(folderResults, 'sum_EN.tif'))
rst_VU <- raster(paste0(folderResults, 'sum_VU.tif'))
rst_CR <- raster(paste0(folderResults, 'sum_CR.tif'))
rst_Threat <- rst_VU +rst_EN + rst_CR
title2 <- paste0("Threatened_", title)
myGraphLatitude(rst_Threat, rstMask, crs_to, title2, "(CR + EN + VU)", paste0(folderResults, folderGraph), fun = fun)
