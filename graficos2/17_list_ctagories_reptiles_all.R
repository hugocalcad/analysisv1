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
folderGraph <- "graphicsGeneral"
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
                         ass.\"assessmentDate\"
                  FROM 	taxonomy AS tax
                  			LEFT JOIN (SELECT DISTINCT ON (\"idAssessment\")
                                           *
                                    FROM   assessment
                                    ORDER  BY \"idAssessment\", \"assessmentDate\" DESC) 
                                          as ass ON (tax.\"internalTaxonId\" = ass.\"internalTaxonId\")")

df_postgres <- dbGetQuery(con, query)
# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)

df_postgres$level1 = NA
df_postgres$level2 = NA


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
df_postgres$orderName2[which(df_postgres$familyName%in%c("TROGONOPHIDAE"))] <- "SAURIA"
######
df_postgres$rlCodes[which(df_postgres$rlCodes=="")]<- "LC"

df_table <- df_postgres %>% filter(className == "REPTILIA") %>%
  group_by(orderName2, rlCodes) %>%
  summarize(n = n()) %>%
  tidyr::spread(key=rlCodes, value = n)

write.csv(df_table, "/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/graphicsGeneral/FINAL/reptils_categories_list.csv")
