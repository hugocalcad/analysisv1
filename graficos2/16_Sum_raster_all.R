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

library(raster)
library(dplyr)
# Setting variables (change Amphibians, Birds, Mammals, Reptiles)
list_rl <- c("EN","CR","VU")

specis <- "Reptiles"
folderResults <- paste0("/media/victor/DATA/GRA2020/SEND_DATA02/RESULTS/", specis)
fsrep <- paste0(folderResults,"/sum_",list_rl, ".tif")
rst_rep <- sumRaster(fsrep, paste0("/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/graphicsGeneral/FINAL/Threat_",specis,".tif"))

specis <- "Mammals"
folderResults <- paste0("/media/victor/DATA/GRA2020/SEND_DATA02/RESULTS/", specis)
fsmam <- paste0(folderResults,"/sum_",list_rl, ".tif")
rst_mam <- sumRaster(fsmam, paste0("/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/graphicsGeneral/FINAL/Threat_",specis,".tif"))

specis <- "Birds"
folderResults <- paste0("/media/victor/DATA/GRA2020/SEND_DATA02/RESULTS/", specis)
fsbir <- paste0(folderResults,"/sum_", list_rl,".tif")
rst_bir <- sumRaster(fsbir, paste0("/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/graphicsGeneral/FINAL/Threat_",specis,".tif"))

specis <- "Amphibians"
folderResults <- paste0("/media/victor/DATA/GRA2020/SEND_DATA02/RESULTS/", specis)
fsamp <- paste0(folderResults,"/sum_", list_rl,".tif")
rst_amb <- sumRaster(fsamp, paste0("/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/graphicsGeneral/FINAL/Threat_",specis,".tif"))

rst_all <-rst_mam+rst_bir+ rst_amb
rst_all[rst_all > 0] <- 1

rst_rep2 <- rst_rep

rst_rep2[rst_all > 0] <- 0

writeRaster(rst_rep2, paste0("/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/graphicsGeneral/FINAL/reptil_noRest.tif"), 
            format="GTiff", overwrite=TRUE)

rst_rep2[rst_rep2 != 1] <- 0

writeRaster(rst_rep2, paste0("/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/graphicsGeneral/FINAL/reptil_noRest.tif"), 
            format="GTiff", overwrite=TRUE)

fileList04 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_reptiles_082020.csv"
df_SpeciesReptiles <- read.csv(fileList04, stringsAsFactors = F)

df2 <- df_SpeciesReptiles %>% filter(redlistCategory %in% c("Critically Endangered", "Vulnerable", "Endangered", "Critically Endangered (Possibly Extinct)"))

file_areas <- paste0("/media/victor/DATA/GRA2020/SEND_DATA02/RESULTS/shps/area/reptilesAreaPresence.csv")
df_areaReptiles <- read.csv(file_areas, stringsAsFactors = F)

df3 <- df2 %>% inner_join(df_areaReptiles) %>%
  filter(areaFin < 50000)
df4 <- df3 %>% filter(scientificName %in% c("Oligosoma fallai","Lepidodactylus euaensis", "Podarcis gaigeae"))

write.csv(df4, "/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/graphicsGeneral/FINAL/reptil_noRest.csv")
