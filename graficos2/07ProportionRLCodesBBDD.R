library(ggplot2)
library(RPostgreSQL)
library(RColorBrewer)
library(raster)
library(dplyr)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
folderResults <- "/media/victor/DATA/GRA2020/SEND_DATA02/RESULTS/"
folderGraph <- "graphicsGeneral"

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv , dbname = "rlSpecies01", host="localhost", port="5432",
                 user="postgres", password="123456hu")  
##query
query <- paste0("SELECT tax.\"scientificName\", tax.\"className\", 
                  			 ass.\"redlistCategory\", 
                         ass.\"assessmentDate\"
                  FROM 	taxonomy AS tax
                  			LEFT JOIN (SELECT DISTINCT ON (\"idAssessment\")
                                           *
                                    FROM   assessment
                                    ORDER  BY \"idAssessment\", \"assessmentDate\" DESC) 
                                          as ass ON (tax.\"internalTaxonId\" = ass.\"internalTaxonId\")
                  		 ")
df_Species02 <- dbGetQuery(con, query)
# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)

rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
            "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
            "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
            "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")

df_Species02$rlCodes<-df_Species02$redlistCategory
df_Species02$rlCodesNro<-df_Species02$redlistCategory
for(i in 1:length(rlCodes)){
  df_Species02$rlCodes[which(df_Species02$rlCodes == names(rlCodes[i]))] = rlCodes[i]
}
df_Species02 <- df_Species02 %>% 
  mutate(rlCodes=replace(rlCodes, rlCodes=="", "Least Concern"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
##
listNames <- c("scientificName", "className", "redlistCategory2", "assessmentDate",  "rlCodes",         "rlCodesNro")

names(df_Species02) <- listNames
# preparing datas
##grafica all terrestrials
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
title <- 'Terrestrial_vertebrates_BD22'
df_SpecieClass <- df_Species %>% filter( rlCodes != "EX")
myGraph2(df_Species02, title, "className", paste0(folderResults, folderGraph), hasAnnotate = 1)
myGraph2_2(df_Species02, title, "className", paste0(folderResults, folderGraph), hasAnnotate = 1, dpi=600)
