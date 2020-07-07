library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
fileList01 <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_amphibians_062020.csv"
fileList02 <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_birds_062020.csv"
fileList03 <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_mammals_062020.csv"
fileList04 <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_reptiles_062020.csv"
folderResults <- "/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/Amphibians/"
title <- 'Amphibians'
folderGraph <- "graphics05"
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
## 
df_Species <-rbind(df_Species01, df_Species02, df_Species03, df_Species04)
df_Species[df_Species == ""] <- "Least Concern"
rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
           "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
           "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
           "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")
rlCodesNro <-c("Least Concern" = 2, "Vulnerable" = 4, "Data Deficient" = 1, "Extinct" = 8,
            "Critically Endangered" = 6, "Endangered" = 5, "Near Threatened" = 3,"Extinct in the Wild" = 7,
            "Lower Risk/least concern" = 2, "Critically Endangered (Possibly Extinct)" = 6,
            "Lower Risk/near threatened" = 3, "Lower Risk/conservation dependent" = 2)
df_Species$rlCodes<-df_Species$redlistCategory
df_Species$rlCodesNro<-df_Species$redlistCategory
for(i in 1:length(rlCodes)){
  df_Species$rlCodes[which(df_Species$rlCodes == names(rlCodes[i]))] = rlCodes[i]
  df_Species$rlCodesNro[which(df_Species$rlCodesNro == names(rlCodesNro[i]))] = rlCodesNro[i]
}
df_SpeciesAves <- df_Species %>% select (className == "AVES")
  arrange(rlCodesNro)
colors <- c("LC" = "#38c457", "VU" = "#fbe946", "DD" = "#dfd9d3", "EX" = "#37292f",
            "CR" = "#d8001d", "EN" = "#ff683f", "NT" = "#bee447","EW" = "#471c36")
colors2 <- c("Least Concern" = "darkgreen", "Vulnerable" = "yellow", "Data Deficient" = "gray", "Extinct" = "black",
             "Critically Endangered" = "red", "Endangered" = "orange", "Near Threatened" = "greenyellow", "Extinct in the Wild" = "purple")
##grafica
totals <- df_Species %>%
  group_by(className) %>%
  tally()
df_Species %>% 
  dplyr::mutate(rlCodes = factor(rlCodes, 
                                    levels = c("EX", "EW", "CR",
                                               "EN", "VU", "NT", "LC", "DD"))) %>%
ggplot(aes(x=factor(className),fill=rlCodes))+
  geom_bar(position = "fill", width = 0.25) +
  scale_fill_manual(values = colors)
rlCodes
