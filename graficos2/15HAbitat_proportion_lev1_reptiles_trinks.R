library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(stars)
library(RPostgreSQL)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "amphibians"
classNameBS <- list("amphibians" = "AMPHIBIA","birds" = "AVES", "mammals" ="MAMMALIA", "reptiles" = "REPTILIA")
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/")
#rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphicsGeneral/FINAL4"
#rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../fun/richnessMap.R")
##BAse de Datos
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv , dbname = "rlSpecies01", host="localhost", port="5432",
                 user="postgres", password="123456hu")  
##query
query <- paste0("SELECT tax.\"scientificName\", ass.\"idAssessment\", 
                  			 ass.\"redlistCategory\", tax.\"className\", 
                  			 tax.\"orderName\",	 tax.\"familyName\",
                         ass.\"assessmentDate\", th.code, th.\"name\"
                  FROM 	taxonomy AS tax
                  			LEFT JOIN (SELECT DISTINCT ON (\"idAssessment\")
                                           *
                                    FROM   assessment
                                    ORDER  BY \"idAssessment\", \"assessmentDate\" DESC) 
                                          as ass ON (tax.\"internalTaxonId\" = ass.\"internalTaxonId\")
                  			LEFT JOIN habitat as th ON (ass.\"idAssessment\" = th.\"idAssessment\")")
df_postgres <- dbGetQuery(con, query)
# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)

df_postgres$level1 = NA
df_postgres$level2 = NA

for (i in seq(1:nrow(df_postgres))) {            # 2. sequence
  if(!is.na(df_postgres$code[[i]])){
    df_postgres$level1[[i]] <- as.numeric(unlist(strsplit(df_postgres$code[[i]], "\\."))[1])
    df_postgres$level2[[i]] <- as.numeric(unlist(strsplit(df_postgres$code[[i]], "\\."))[2])
  }
}
df_postgres$level2S <- paste0(df_postgres$level1, ".", df_postgres$level2)
df_postgres <-na.omit(df_postgres)
rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
            "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
            "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
            "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")
df_postgres$rlCodes<-df_postgres$redlistCategory
df_postgres$rlCodesNro<-df_postgres$redlistCategory
for(i in 1:length(rlCodes)){
  df_postgres$rlCodes[which(df_postgres$rlCodes == names(rlCodes[i]))] = rlCodes[i]
}

## puttin SAURIA y SERPENTES instead of SQUAMATA
fileList04 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_reptiles_082020.csv"
df_SpeciesReptiles <- read.csv(fileList04, stringsAsFactors = F)
df_postgres$orderName2 <- df_postgres$orderName
df_SpeciesReptilesSQUAMATAS <- df_SpeciesReptiles %>%
  filter(orderName %in% c("SAURIA"))
df_postgres$orderName2[which(df_postgres$familyName%in%unique(df_SpeciesReptilesSQUAMATAS$familyName))] <- "SAURIA"
df_SpeciesReptilesSQUAMATAS <- df_SpeciesReptiles %>%
  filter(orderName %in% c("SERPENTES"))
df_postgres$orderName2[which(df_postgres$familyName%in%unique(df_SpeciesReptilesSQUAMATAS$familyName))] <- "SERPENTES"
df_postgres$orderName2[which(df_postgres$familyName%in%c("XENODERMIDAE","PAREIDAE","ATRACTASPIDIDAE"))] <- "SERPENTES"
######

df_postgre3 <- df_postgres %>% 
  select(!c("code", "name")) %>%
  distinct()

df_postgre4 <- df_postgre3 %>%
  filter(className == "REPTILIA")
df_postgre4$nlevel1 <- as.character(df_postgre4$level1)
## create ggplot
listHabitats <- c("Forest" = 1, "Savanna" = 2, "Shrubland" = 3, "Grassland" =4,
                  "Wetlands (inland)"= 5, "Rocky Areas" = 6, "Caves & Subterranean" = 7,
                  "Desert" =8, "Marine Neritic" = 9, "Marine Oceanic" = 10, "Marine Deep Ocean Floor" = 11,
                  "Marine Intertidal"= 12, "Marine Coastal/Supratidal" = 13, "Artificial - Terrestrial" = 14, 
                  "Artificial - Aquatic" = 15)
for(i in 1:length(listHabitats)){
  df_postgre4$nlevel1[which(df_postgre4$nlevel1 == listHabitats[i])] = names(listHabitats[i])
}

`%notin%` <- Negate(`%in%`)

df_reptilsForest <- df_postgre4 %>%
  filter(level1 ==1)%>%
  select(scientificName)

df_postgre5 <- df_postgre4 %>% 
  mutate(flag_dessert = ifelse(level1 %in% c(8) & 
                                 scientificName %notin%(df_postgre4 %>%
                                 filter(level1 !=8)%>%
                                 select(scientificName))$scientificName,1,0),
         flag_grassland = ifelse(level1 %in% c(4) & 
                                 scientificName %notin%(df_postgre4 %>%
                                                          filter(level1 !=4)%>%
                                                          select(scientificName))$scientificName,1,0),
         flag_savanna = ifelse(level1 %in% c(2) & 
                                 scientificName %notin%(df_postgre4 %>%
                                                          filter(level1 !=2)%>%
                                                          select(scientificName))$scientificName,1,0),
         
         flag_three =ifelse(level1 %in% c(2,4,8) & 
                              scientificName %notin%(df_postgre4 %>%
                                                       filter(level1 ==1)%>%
                                                       select(scientificName))$scientificName,1,0))

df_postgre6 <- df_postgre5 %>% 
  filter (level1 %in% c(2,4,8)) %>%
  group_by(className) %>%
  summarize(desert=sum(flag_dessert),
            desert_threat=sum(flag_dessert & rlCodes %in%c("CR","EN","VU")),
            grassland=sum(flag_grassland),
            grassland_threat=sum(flag_dessert & rlCodes %in%c("CR","EN","VU")),
            savanna=sum(flag_savanna),
            savanna_threat=sum(flag_savanna & rlCodes %in%c("CR","EN","VU")),
            noForest=sum(flag_three),
            noForest_threat=sum(flag_three & rlCodes %in%c("CR","EN","VU")),) %>%
  mutate(desert_threat_percent = (desert_threat/desert)*100,
         grassland_threat_percent = (grassland_threat/grassland)*100,
         savanna_threat_percent = (savanna_threat/savanna)*100,
         noForest_threat_percent = (noForest_threat/noForest)*100)

write.csv(df_postgre6, paste0(folderResults, folderGraph, "/03reptilesHabitatAridsNoForest.csv"))

