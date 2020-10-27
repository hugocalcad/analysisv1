library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(stars)
library(RPostgreSQL)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "reptiles"
classNameBS <- list("amphibians" = "AMPHIBIA","birds" = "AVES", "mammals" ="MAMMALIA", "reptiles" = "REPTILIA")
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/")
#rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphicsGeneral/FINAL5"
#rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../fun/richnessMap.R")
##BAse de Datos
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv , dbname = "rlSpecies01", host="localhost", port="5432",
                 user="postgres", password="123456hu")  
##query
query <- paste0("SELECT tax.\"scientificName\", tax.\"className\",
                        tax.\"orderName\", tax.\"familyName\",
                         ass.\"idAssessment\", ass.\"redlistCategory\", 
                         ass.\"assessmentDate\", ass.threats, th.code, th.\"name\",
                         th.\"scope\", th.\"severity\"
                  FROM 	taxonomy AS tax
                  			LEFT JOIN (SELECT DISTINCT ON (\"idAssessment\")
                                           *
                                    FROM   assessment
                                    ORDER  BY \"idAssessment\", \"assessmentDate\" DESC) 
                                          as ass ON (tax.\"internalTaxonId\" = ass.\"internalTaxonId\")
                  			LEFT JOIN threat as th ON (ass.\"idAssessment\" = th.\"idAssessment\")")
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
#df_postgres$level1[df_postgres$level1 == 5 & df_postgres$level2 == 3  ] <- 13

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

listThreats <- c("Residential & commercial development" = 1, "Agriculture & aquaculture" = 2, 
                 "Energy production & mining" = 3, "Transportation & service corridors" =4,
                 "Biological resource use"= 5, "Human intrusions & disturbance" = 6, 
                 "Natural system modifications" = 7, "Invasive & other problematic species, genes & diseases" =8, 
                 "Pollution" = 9, "Geological events" = 10, "Climate change & severe weather" = 11,
                 "Others"= 12)
df_postgres$nlevel1 <- as.character(df_postgres$level1)
for(i in 1:length(listThreats)){
  df_postgres$nlevel1[which(df_postgres$nlevel1 == listThreats[i])] = names(listThreats[i])
}
df_postgres$level1 <- as.factor(df_postgres$level1)
df_postgres$nlevel1 <- factor(df_postgres$nlevel1, levels = c("Residential & commercial development", "Agriculture & aquaculture", 
                                                              "Energy production & mining", "Transportation & service corridors",
                                                              "Biological resource use", "Logging & wood harvesting",
                                                              "Human intrusions & disturbance", 
                                                              "Natural system modifications", "Invasive & other problematic species, genes & diseases", 
                                                              "Pollution", "Geological events", "Climate change & severe weather",
                                                              "Others"))

listThreats <- c("Residential & commercial development" = "Urban Development", 
                 "Agriculture & aquaculture" = "Agricultural Activity", 
                 "Energy production & mining" = "Urban Development", 
                 "Transportation & service corridors" ="Urban Development",
                 "Biological resource use"= "Over-Exploitation", 
                 "Human intrusions & disturbance" = "Urban Development", 
                 "Natural system modifications" = "System Modification", 
                 "Invasive & other problematic species, genes & diseases" ="Invasion & Disease", 
                 "Pollution" = "Pollution", 
                 "Geological events" = "System Modification",
                 "Climate change & severe weather" = "Climate Change",
                 "Others"= "Invasion & Disease", 
                 "Logging & wood harvesting" = "Over-Exploitation")

df_postgres$nlevel2 <- as.character(df_postgres$nlevel1)
for(i in 1:length(listThreats)){
  df_postgres$nlevel2[which(df_postgres$nlevel2 == names(listThreats[i]))] <- listThreats[i]
}

df_postgres$nlevel2 <- factor(df_postgres$nlevel2, levels = c("Climate Change", "System Modification",
                                                              "Pollution", "Invasion & Disease",
                                                              "Over-Exploitation", "Agricultural Activity",
                                                              "Urban Development"))
##Super level2 creation
listThreatslevel2 <- c("Housing & urban areas" = "1.1", "Commercial & industrial areas" = "1.2", 
                       "Tourism & recreation areas" = "1.3", "Annual & perennial non-timber crops " ="2.1",
                       "Wood & pulp plantations"= "2.2", "Livestock farming & ranching" = "2.3", 
                       "Marine & freshwater aquaculture" = "2.4", "Oil & gas drilling" ="3.1", 
                       "Mining & quarrying" = "3.2", "Renewable energy" = "3.3", "Roads & railroads" = "4.1",
                       "Utility & service lines"= "4.2", "Shipping lanes"= "4.3", "Flight paths"= "4.4",
                       "Hunting & collecting terrestrial animals" = "5.1","Gathering terrestrial plants " = "5.2","Logging & wood harvesting " = "13.3",
                       "Fishing & harvesting aquatic resources " = "5.4","Recreational activities" = "6.1","War, civil unrest & military exercises" = "6.2",
                       "Work & other activities" = "6.3", "Fire & fire suppression " = "7.1", "Dams & water management/use" = "7.2", 
                       "Other ecosystem modifications" = "7.3","Invasive non-native/alien species/diseases " = "8.1","Problematic native species/diseases " = "8.2",
                       "Introduced genetic material" = "8.3","Problematic species/diseases of unknown origin " = "8.4","Viral/prion-induced diseases " = "8.5",
                       "Diseases of unknown cause " = "8.6","Domestic & urban waste water " = "9.1","Industrial & military effluents " = "9.2",
                       "Agricultural & forestry effluents " = "9.3","Garbage & solid waste" = "9.4","Air-borne pollutants " = "9.5",
                       "Excess energy "="9.6", "Volcanoes"="10.1","Earthquakes/tsunamis"="10.2",
                       "Avalanches/landslides"="10.3","Habitat shifting & alteration"="11.1","Droughts"="11.2",
                       "Temperature extremes"="11.3","Storms & flooding"="11.4","Other impacts"="11.5", "Other threat"="12.1")
df_postgres$nlevel2S <- df_postgres$level2S
for(i in 1:length(listThreatslevel2)){
  df_postgres$nlevel2S[which(df_postgres$nlevel2S == listThreatslevel2[i])] = names(listThreatslevel2[i])
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
df_postgres$nlevel2S2 <- paste0(df_postgres$level2S, " - ", df_postgres$nlevel2S)

df_postgre2 <- df_postgres[!duplicated(df_postgres), ]

df_postgre3 <- df_postgres %>% 
  select(!c("code", "name", "scope", "severity")) %>%
  distinct()

df_postgres4 <- df_postgre3 %>%
  filter(level2S %in% c("5.1", "5.4") & className=="REPTILIA")

`%notin%` <- Negate(`%in%`)

df_postgres5 <- df_postgres4%>%
  mutate(flag_5_1 = ifelse(level2S %in% c("5.1") & 
                                 scientificName %notin%(df_postgres4 %>%
                                                          filter(level2S =="5.4")%>%
                                                          select(scientificName))$scientificName,1,0),
         flag_5_4 = ifelse(level2S %in% c("5.4") & 
                             scientificName %notin%(df_postgres4 %>%
                                                      filter(level2S =="5.1")%>%
                                                      select(scientificName))$scientificName,1,0),
         flag_5_1_5_4 = ifelse(flag_5_1==0 & flag_5_4==0,1,0)
         )

df_table_level1_2_2 <- df_postgres5 %>% 
  group_by(className, level2S) %>%
  summarize(n=n(),
            threath =sum(rlCodes %in% c("CR", "EN", "VU")),
            unique2 = sum(flag_5_1_5_4 ==TRUE),
            repeat2 = sum(flag_5_1_5_4 ==FALSE),
            uniqueThreath = sum(rlCodes %in% c("CR", "EN", "VU") & flag_5_1_5_4 ==TRUE),
            repeatThreath = sum(rlCodes %in% c("CR", "EN", "VU") & flag_5_1_5_4 ==FALSE)) 

df_result <- df_postgres5 %>%
  group_by(className) %>%
  summarize(lev_5_1=sum(flag_5_1),
            lev_5_1_threat=sum(flag_5_1 & rlCodes %in% c("CR","EN","VU")),
            lev_5_4=sum(flag_5_4),
            lev_5_4_threat=sum(flag_5_4 & rlCodes %in% c("CR","EN","VU")),
            lev_5_1_5_4=sum(flag_5_1_5_4),
            lev_5_1_5_4_threat=sum(flag_5_1_5_4& rlCodes %in% c("CR","EN","VU")))

write.csv(df_postgres5, paste0(folderResults, folderGraph, "/Reptile_hunting_fishing.csv"))
write.csv(df_table_level1_2_2, paste0(folderResults, folderGraph, "/Reptile_hunting_fishingResume.csv"))


