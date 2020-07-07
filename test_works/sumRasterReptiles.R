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
folderSpecies <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/Reptiles_geographic_062020"
fileList <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_reptiles_062020.csv"
folderResults <- "/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/Reptiles/"
# Listing Raster Files
fs1 <- list.files(path=folderSpecies, pattern = "tif$", full.names = TRUE)
# Sum all raster for richness
rasterResult <- sumRaster(fs1)
# Open the list with red list categories and groups as dataframe
df <- read.csv(folder)
# Abbreviation of categories
rlCat <- list(LC = c('Least Concern', 'Lower Risk/least concern', 'Lower Risk/conservation dependent', ""), VU = 'Vulnerable', 
              DD = 'Data Deficient', EX = 'Extinct',
              CR = c('Critically Endangered', 'Critically Endangered (Possibly Extinct)'), EN = 'Endangered', 
              NT = c('Near Threatened', 'Lower Risk/near threatened'), EW = 'Extinct in the Wild')
df$rlCat <- `levels<-`(df$redlistCategory, rlCat)
# Agreggating full route to the specie
df$route <- paste0(folderSpecies, df$rasterFile)
## listing values de orderName (subgroup of Family)
orders <- df %>% distinct(orderName) %>% pull()
## Generating All Map order with rl category
for(order in orders){
  df_subGroup <- df %>% filter(orderName == order)
  rlCat2 <- df_subGroup %>% distinct(rlCat) %>% pull()
  for(rl in rlCat2){
    df_subGroup2 <- df_subGroup %>% filter(rlCat == rl)
    print(paste("Order: ", order, ", Category :", rl))
    sumRaster(df_subGroup2$route, paste0(folderResults,"sum_",order,"_",rl,".tif"))
  }
}
## Generating All Map orderName
for(order in orders){
  fs2 <- list.files(path=folderResults, pattern = glob2rx(paste0("sum_",order,"_*.tif$")), full.names = TRUE)
  sumRaster(fs2, paste0(folderResults,"sum_",order,".tif"))
}
## Generating all map by category
## listing values de orderName (subgroup of Family)
rlCat2 <- df %>% distinct(rlCat) %>% pull()
for(rl in rlCat2){
  fs2 <- list.files(path=folderResults, pattern = glob2rx(paste0("sum_*_*",rl,".tif$")), full.names = TRUE)
  sumRaster(fs2, paste0(folderResults,"sum_",rl,".tif"))
}
