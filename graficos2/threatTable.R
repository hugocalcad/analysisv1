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
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphics09"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../fun/richnessMap.R")
##BAse de Datos
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv , dbname = "rlSpecies01", host="localhost", port="5432",
                 user="postgres", password="67huguito82")  
##query
query <- paste0("SELECT tax.\"scientificName\", ass.\"idAssessment\", 
                  			 ass.\"redlistCategory\", 
                         ass.\"assessmentDate\", th.code, th.\"name\"
                  FROM 	taxonomy AS tax
                  			LEFT JOIN (SELECT DISTINCT ON (\"idAssessment\")
                                           *
                                    FROM   assessment
                                    ORDER  BY \"idAssessment\", \"assessmentDate\" DESC) 
                                          as ass ON (tax.\"internalTaxonId\" = ass.\"internalTaxonId\")
                  			LEFT JOIN threat as th ON (ass.\"idAssessment\" = th.\"idAssessment\")
                  WHERE 
                  	ass.\"redlistCategory\" IN ('Critically Endangered', 'Endangered', 'Vulnerable') AND
                  	tax.\"className\" = '",classNameBS[className],"'")
df_postgres <- dbGetQuery(con, query)
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
##preparate data
rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
            "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
            "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
            "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")
df_postgres$rlCodes<-df_postgres$redlistCategory
df_postgres$rlCodesNro<-df_postgres$redlistCategory
for(i in 1:length(rlCodes)){
  df_postgres$rlCodes[which(df_postgres$rlCodes == names(rlCodes[i]))] = rlCodes[i]
}
colors <- c("LC" = "#38c457", "VU" = "#ffeda0", "DD" = "#dfd9d3", "EX" = "#37292f",
            "CR" = "#f03b20", "EN" = "#feb24c", "NT" = "#bee447","EW" = "#471c36")

## create ggplot
p<- ggplot(df_postgres) +
  geom_bar(aes(x=level2S, fill= level2S)) +
  #scale_y_log10()+
  facet_wrap(~level1, scales = "free")+
  #scale_y_log10()+
  labs(title = paste0(title," - Threat category - level 1, level 2"))+
  #scale_fill_manual(values = colors)+
  theme(axis.text.x = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

print(p)
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
ggsave(
  paste0(folderResults,folderGraph,"/threatTables_",title,"_free.png"),
  #ggplot_alternative(),
  width = 10,
  height = 5,
  dpi = 300
)

# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)