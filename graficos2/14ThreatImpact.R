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
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphics12"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../fun/richnessMap.R")
##BAse de Datos
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv , dbname = "rlSpecies01", host="localhost", port="5432",
                 user="postgres", password="123456hu")  
##query
query <- paste0("SELECT tax.\"scientificName\", tax.\"className\", 
                  			 ass.\"redlistCategory\",
                         ass.\"assessmentDate\", th.code, th.\"name\",
                         th.scope, th.severity, th.timing
                  FROM 	taxonomy AS tax
                  			LEFT JOIN (SELECT DISTINCT ON (\"idAssessment\")
                                           *
                                    FROM   assessment
                                    ORDER  BY \"idAssessment\", \"assessmentDate\" DESC) 
                                          as ass ON (tax.\"internalTaxonId\" = ass.\"internalTaxonId\")
                  			LEFT JOIN Threat as th ON (ass.\"idAssessment\" = th.\"idAssessment\")
                  ") #AND
# tax.\"className\" = '",classNameBS[className],"'")


df_postgres <- dbGetQuery(con, query)
# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)

rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
            "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
            "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
            "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")

df_postgres$rlCodes<-df_postgres$redlistCategory

for(i in 1:length(rlCodes)){
  df_postgres$rlCodes[which(df_postgres$rlCodes == names(rlCodes[i]))] = rlCodes[i]
}
#statisctics
df_datos <- df_postgres %>%
  filter(rlCodes %in% c("EN","VU", "CR")) %>%
  group_by(className) %>%
  summarize(
    Scope = sum(scope %notin% c("", NA, "Unknown")),
    Severity = sum(severity %notin% c("", NA, "Unknown")),
    Timing = sum(timing %notin% c("", NA, "Unknown")),
    noScope = sum(scope %in% c("", NA, "Unknown")),
    noSeverity = sum(severity %in% c("", NA, "Unknown")),
    noTiming = sum(timing %in% c("", NA, "Unknown")),
    noThree = sum(timing %in% c("", NA, "Unknown") | severity %in% c("", NA, "Unknown") | timing %in% c("", NA, "Unknown"))
  ) %>%
  mutate(total = Scope + noScope,
         Three =  total - noThree)
df_datos2 <- df_datos %>% select(className,Three, noThree) %>%
  gather(values, count, -className)

ggplot(data=df_datos2, aes(x=className, y=count, fill=values)) + 
  geom_bar(position = 'dodge', stat = "identity") +
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25)

df_species <- df_postgres %>%
  filter(rlCodes %in% c("EN","VU", "CR")) %>%
  group_by(className, scientificName) %>%
  summarize(total = n(),
            Scope = sum(scope %notin% c("", NA, "Unknown")),
            Severity = sum(severity %notin% c("", NA, "Unknown")),
            Timing = sum(timing %notin% c("", NA, "Unknown")),
            noScope = sum(scope %in% c("", NA, "Unknown")),
            noSeverity = sum(severity %in% c("", NA, "Unknown")),
            noTiming = sum(timing %in% c("", NA, "Unknown")),
            noThree = sum(timing %in% c("", NA, "Unknown") | severity %in% c("", NA, "Unknown") | timing %in% c("", NA, "Unknown")))

df_species2 <- df_species %>%
  group_by(className) %>%
  summarize(total = n(),
            Scope = sum(Scope>0),
            Severity = sum(Severity>0),
            Timing = sum(Timing > 0),
            noThree = sum(noThree>0),
            three = total - noThree)
df_datos22 <- df_species2 %>% select(className,three, noThree) %>%
  gather(values, count, -className)

ggplot(data=df_datos22, aes(x=className, y=count, fill=values)) + 
  geom_bar(position = 'dodge', stat = "identity") +
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25)
