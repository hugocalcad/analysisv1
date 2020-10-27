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
library(tidyverse)
# Setting variables (change amphibians, birds, mammals, reptiles)
className <- "birds"
className2 <- stringr::str_to_title(className)
folderSpecies <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/",className2,"_geographic_062020/")
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS2/",className2,"/")
newList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS2/terrestrial_",className,".csv")
# Listing Raster Files
fs1 <- list.files(path=folderSpecies, pattern = "tif$", full.names = F)
fs1 <- gsub("\\.tif$", "", fs1)
  
fs_new_list <- read.csv(newList, header = F, stringsAsFactors = F)$V1
fs_new_list <- gsub("_", " ", fs_new_list)

fs2 <- intersect(fs1, fs_new_list)

# Sum all raster for richness

# Open the list with red list categories and groups as dataframe
df <- read.csv(fileList)
df2 <- df %>% filter(scientificName %in% fs2)
fs2 <- paste0(folderSpecies, "/", fs2, ".tif")
#rasterResult <- sumRaster(fs2)
# Abbreviation of categories
rlCat <- list(LC = c('Least Concern', 'Lower Risk/least concern', 'Lower Risk/conservation dependent', ""), VU = 'Vulnerable', 
              DD = 'Data Deficient', EX = 'Extinct',
              CR = c('Critically Endangered', 'Critically Endangered (Possibly Extinct)'), EN = 'Endangered', 
              NT = c('Near Threatened', 'Lower Risk/near threatened'), EW = 'Extinct in the Wild')
df2$rlCat <- `levels<-`(df2$redlistCategory, rlCat)
# Agreggating full route to the specie
df2$route <- paste0(folderSpecies, df2$rasterFile)

## listing values de orderName (subgroup of Family)
orders <- df2 %>% distinct(orderName) %>% pull()
## Generating All Map order with rl category
dir.create(paste0(folderResults), showWarnings = FALSE)
for(order in orders){
  df_subGroup <- df2 %>% filter(orderName == order)
  rlCat2 <- df_subGroup %>% distinct(rlCat) %>% pull()
  for(rl in rlCat2){
    df_subGroup2 <- df_subGroup %>% filter(rlCat == rl)
    print(paste("Order: ", order, ", Category :", rl))
    sumRaster(df_subGroup2$route, paste0(folderResults,"sum_",order,"_",rl,".tif"))
  }
}
## Generating All Map orderName
for(order in orders){
  fs3 <- list.files(path=folderResults, pattern = glob2rx(paste0("sum_",order,"_*.tif$")), full.names = TRUE)
  sumRaster(fs3, paste0(folderResults,"sum_",order,".tif"))
}
## Generating all map by category
## listing values de orderName (subgroup of Family)
rlCat2 <- df2 %>% distinct(rlCat) %>% pull()
for(rl in rlCat2){
  fs3 <- list.files(path=folderResults, pattern = glob2rx(paste0("sum_*_*",rl,".tif$")), full.names = TRUE)
  sumRaster(fs3, paste0(folderResults,"sum_",rl,".tif"))
}
fs3 <- list.files(path=folderResults, pattern = glob2rx(paste0("sum_*_*.tif$")), full.names = TRUE)
sumRaster(fs3, paste0(folderResults,"sumTotal",className2,".tif"))



