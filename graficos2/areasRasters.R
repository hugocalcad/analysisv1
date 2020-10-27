library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(stars)
sumRaster <- function(listRaster, outRaster){
  library(raster)
  i <- 1
  t <- length(listRaster)
  for(fr in listRaster){
    if(i == 1){
      raster_out <- raster(fr)
      raster_out[is.na(raster_out)] <- 0
    }
    else{
      raster_add <- raster(fr)
      raster_add[is.na(raster_add)] <- 0
      raster_out <- raster_out + raster_add
    }
    if(i%%100 ==0){
      print(paste0(i," de : ",t))
    }
    i <- i + 1 
  }
  raster_out <- writeRaster(raster_out, outRaster, format="GTiff", overwrite=TRUE) 
  return(raster_out)
}
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "reptiles"
className2 <- stringr::str_to_title(className)
folderRoute <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/",className2,"_geographic_082020/")
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_",className,"_082020.csv")

file_areas <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/shps/area/",className,"AreaPresence.csv")

folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")

folderAreas <- "sumAreas"
#folderGraph <- "graphics01"


##Open Files
df_list <- read.csv(fileList)
df_areas <- read.csv(file_areas)

df_list_areas <- df_list %>% left_join(df_areas)
df_list_areas$route <- paste0(folderRoute, df_list_areas$rasterFile)
dist <- c(1000,5000,10000,15000,20000,30000,40000,50000)
dir.create(paste0(folderResults,folderAreas), showWarnings = FALSE)
for(i in 1:length(dist)){
  title <- paste0(className2, "_",dist[i],"km") 
  if(i == 1){
    df_list_areas_sub <- df_list_areas %>% filter(areaFin <= dist[i])
    list_raster_sum <- df_list_areas_sub$route
  }else{
    df_list_areas_sub <- df_list_areas %>% filter(areaFin > dist[i-1] & areaFin <= dist[i])
    list_raster_sum <- df_list_areas_sub$route
    list_raster_sum <- append(list_raster_sum,  paste0(folderResults,folderAreas,"/sum_",className2, "_",dist[i-1],"km",".tif"))
  }
  rasterOut <- sumRaster(list_raster_sum, paste0(folderResults,folderAreas,"/sum_",title,".tif"))
}
