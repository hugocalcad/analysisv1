library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(stars)
library(RPostgreSQL)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
fileList01 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_amphibians_082020.csv"
fileList02 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_birds_082020.csv"
fileList03 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_mammals_082020.csv"
fileList04 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_reptiles_082020.csv"
folderResults <- "/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/"
folderGraph <- "graphicsGeneral"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
##
listNames <- c("kingdomName", "phylumName", "className", "orderName",
               "familyName", "genusName", "speciesName",
               "scientificName", "redlistCategory", "rasterFile")
df_Species_amp <- read.csv(fileList01, stringsAsFactors = F)
df_Species_bid <- read.csv(fileList02, stringsAsFactors = F)
df_Species_mam <- read.csv(fileList03, stringsAsFactors = F)
df_Species_rep <- read.csv(fileList04, stringsAsFactors = F)
names(df_Species_bid) <- listNames
df_Species <-rbind(df_Species_amp, df_Species_bid, df_Species_mam, df_Species_rep)
#rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')

##BAse de Datos
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv , dbname = "rlSpecies01", host="localhost", port="5432",
                 user="postgres", password="123456hu")  
##query
query <- paste0("SELECT tax.\"kingdomName\",	 tax.\"phylumName\",
                        tax.\"className\", tax.\"orderName\",	 tax.\"familyName\",
                        tax.\"genusName\", tax.\"scientificName\",  
                  			ass.\"redlistCategory\"
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

rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
            "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT",
            "Extinct in the Wild" = "EW" )
df_postgres$rlCodes<-df_postgres$redlistCategory
for(i in 1:length(rlCodes)){
  df_postgres$rlCodes[which(df_postgres$rlCodes == rlCodes[i])] = names(rlCodes[i])
}
df_postgres$rlCodes[which(df_postgres$rlCodes=="")]<- "Least Concern"
df_Species$redlistCategory[which(df_Species$redlistCategory=="")]<- "Least Concern"

df_postgres2 <- df_postgres%>%dplyr::select(!redlistCategory)
names(df_postgres2) <- c("kingdomName", "phylumName", "className",
                         "orderName", "familyName", "genusName",     
                         "scientificName", "redlistCategory")


df_Species$hasSpatial <- 1
df_postgres2$hasTabular <- 1

df_todo <- df_Species %>% full_join(df_postgres2)

df_pppp <- df_postgres2 %>% filter(className == "AVES")
df_select <- df_todo %>% filter(className == "REPTILIA")

write.csv(df_pppp, "/media/victor/DATA/GRA2020/SEND_DATA/RESULTS/graphicsGeneral/FINAL/birds_list.csv")
