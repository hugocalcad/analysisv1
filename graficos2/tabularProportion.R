library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
fileList01 <- "/media/victor/DATA/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_amphibians_062020.csv"
fileList02 <- "/media/victor/DATA/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_birds_062020.csv"
fileList03 <- "/media/victor/DATA/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_mammals_062020.csv"
fileList04 <- "/media/victor/DATA/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_reptiles_062020.csv"
folderResults <- "/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/"
folderGraph <- "graphicsGeneral"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
##
listNames <- c("kingdomName", "phylumName", "className", "orderName",
               "familyName", "genusName", "speciesName",
               "scientificName", "redlistCategory", "rasterFile")
df_Species01 <- read.csv(fileList01, stringsAsFactors = F)
df_Species02 <- read.csv(fileList02, stringsAsFactors = F)
df_Species03 <- read.csv(fileList03, stringsAsFactors = F)
df_Species04 <- read.csv(fileList04, stringsAsFactors = F)
names(df_Species02) <- listNames
# preparing datas
df_Species <-rbind(df_Species01, df_Species02, df_Species03, df_Species04)
df_Species[df_Species == ""] <- "Least Concern"
rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
           "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
           "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
           "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")
df_Species$rlCodes<-df_Species$redlistCategory
df_Species$rlCodesNro<-df_Species$redlistCategory
for(i in 1:length(rlCodes)){
  df_Species$rlCodes[which(df_Species$rlCodes == names(rlCodes[i]))] = rlCodes[i]
}
##grafica all terrestrials
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
title <- 'Terrestrial_vertebrates'
df_SpecieClass <- df_Species %>% filter( rlCodes != "EX")
myGraph2(df_Species, title, "className", paste0(folderResults, folderGraph), hasAnnotate = 1)
myGraph2_1(df_SpecieClass, title, "className", paste0(folderResults, folderGraph), hasAnnotate = 1)
## grafica by class
df_SpecieClass <- df_Species %>% filter( className == "REPTILIA")
title <- 'Reptiles'
myGraph2(df_SpecieClass, title, "orderName", paste0(folderResults, folderGraph), hasAnnotate = 0)

df_SpecieClass <- df_Species %>% filter( className == "MAMMALIA")
title <- 'Mammals'
myGraph2(df_SpecieClass, title, "orderName", paste0(folderResults, folderGraph), hasAnnotate = 0)

df_SpecieClass <- df_Species %>% filter( className == "AMPHIBIA")
title <- 'Amphibians'
myGraph2(df_SpecieClass, title, "orderName", paste0(folderResults, folderGraph), hasAnnotate = 0)

df_SpecieClass <- df_Species %>% filter( className == "AVES")
title <- 'Birds'
myGraph2(df_SpecieClass, title, "orderName", paste0(folderResults, folderGraph), hasAnnotate = 0)
