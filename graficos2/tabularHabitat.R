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
query <- paste0("SELECT tax.\"scientificName\", ass.\"idAssessment\", 
                  			 ass.\"redlistCategory\", tax.\"className\",
                         ass.\"assessmentDate\", th.code, th.\"name\"
                  FROM 	taxonomy AS tax
                  			LEFT JOIN (SELECT DISTINCT ON (\"idAssessment\")
                                           *
                                    FROM   assessment
                                    ORDER  BY \"idAssessment\", \"assessmentDate\" DESC) 
                                          as ass ON (tax.\"internalTaxonId\" = ass.\"internalTaxonId\")
                  			LEFT JOIN habitat as th ON (ass.\"idAssessment\" = th.\"idAssessment\")
                  WHERE 
                  	ass.\"redlistCategory\" IN ('Critically Endangered', 'Endangered', 'Vulnerable')") #AND
                  	# tax.\"className\" = '",classNameBS[className],"'")
df_postgres <- dbGetQuery(con, query)
df_postgres$level1 = NA
df_postgres$level2 = NA

for (i in seq(1:nrow(df_postgres))) {            # 2. sequence
  if(!is.na(df_postgres$code[[i]])){
    df_postgres$level1[[i]] <- as.numeric(unlist(strsplit(df_postgres$code[[i]], "\\."))[1])
    df_postgres$level2[[i]] <- as.numeric(unlist(strsplit(df_postgres$code[[i]], "\\."))[2])
  }
}
df_postgres <- df_postgres %>%
  arrange(level1 , level2)
df_postgres$level2S <- paste0(df_postgres$level1, ".", df_postgres$level2)
df_postgres$level2S <- factor(df_postgres$level2S, levels = unique(df_postgres$level2S))
df_postgres <-na.omit(df_postgres)
##preparate data
rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
            "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
            "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
            "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")
df_postgres$rlCodes<-df_postgres$redlistCategory
df_postgres$rlCodesNro<-df_postgres$redlistCategory
listHabitats <- c("Forest" = 1, "Savanna" = 2, "Shrubland" = 3, "Grassland" =4,
                  "Wetlands (inland)"= 5, "Rocky Areas" = 6, "Caves & Subterranean Habitats" = 7,
                  "Desert" =8, "Marine Neritic" = 9, "Marine Oceanic" = 10, "Marine Deep Ocean Floor" = 11,
                  "Marine Intertidal"= 12, "Marine Coastal/Supratidal" = 13, "Artificial - Terrestrial" = 14, 
                  "Artificial - Aquatic" = 15)
for(i in 1:length(rlCodes)){
  df_postgres$rlCodes[which(df_postgres$rlCodes == names(rlCodes[i]))] = rlCodes[i]
}
df_postgres$level1N <- df_postgres$level1
for(i in 1:length(listHabitats)){
  df_postgres$level1N[which(df_postgres$level1N == as.numeric(listHabitats[i]))] = names(listHabitats[i])
}
df_postgres$level1N <- factor(df_postgres$level1N, levels =names(listHabitats))
colors <- c("LC" = "#38c457", "VU" = "#ffeda0", "DD" = "#dfd9d3", "EX" = "#37292f",
            "CR" = "#f03b20", "EN" = "#feb24c", "NT" = "#bee447","EW" = "#471c36")
df_postgres2 <- df_postgres %>% filter(!level1 %in% c(9,10,11,12,13))
## create ggplot
p<- ggplot(df_postgres2) +
  geom_bar(aes(x=level1N, fill= rlCodes), position = "dodge") +
  #scale_y_log10()+
  facet_wrap(~className, scales = "free_y")+
  #scale_y_log10()+
  labs(title = paste0(title," - Habitat - Threatened species"))+
  scale_fill_manual(values = colors)+
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1 , hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

print(p)
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
ggsave(
  paste0(folderResults,folderGraph,"/habitatTables_all_free_2.png"),
  #ggplot_alternative(),
  width = 10,
  height = 5,
  dpi = 300
)
##

# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)