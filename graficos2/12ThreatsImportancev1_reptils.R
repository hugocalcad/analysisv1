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
query <- paste0("SELECT tax.\"scientificName\", tax.\"className\",
                        tax.\"orderName\", tax.\"familyName\",
                         ass.\"idAssessment\", ass.\"redlistCategory\", 
                         ass.\"assessmentDate\", th.code, th.\"name\"
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
df_postgres$level1[df_postgres$level1 == 5 & df_postgres$level2 == 3  ] <- 13

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
                 "Others"= 12, "Logging & wood harvesting" = 13)
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

df2 <- df_postgre3 %>%
  filter(className == "REPTILIA") %>%
  group_by(orderName2, nlevel2) %>%
  summarize(richness = n(),
            threat = sum(rlCodes %in% c("CR","EN", "VU"))) %>%
  mutate(percent = (threat/richness)*100) %>%
  filter(percent > 0)
df3 <- df2 %>%
  group_by(orderName2) %>%
  summarise(richness = sum(richness),
            threat = sum(threat))

df2$totalThreat <- 0
df2$totalRichness <- 0
for(i in 1:nrow(df3)){
  df2$totalThreat[which(df2$orderName2 == df3$orderName2[i])] <- df3$threat[i]
  df2$totalRichness[which(df2$orderName2 == df3$orderName2[i])] <- df3$richness[i]
}
df2$percent2 <- (df2$threat/df2$totalThreat)*100


## create ggplot

# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73")
cbp2 <- c("#528be1",  "#b87750",  "#50aa65",  "#666760")

cbp3 <- c( "#67001f", "#f4a582", "#d6604d", "#ffffbf")
cbp3 <- c("#cccccc", "#969696", "#636363", "#252525")
p<- ggplot(df2) +
  geom_bar(aes(x=as.factor(nlevel2), y = percent2, fill= orderName2), stat = "identity", position = "dodge") +
  #scale_y_log10()+
  #facet_wrap(~level1, scales = "free")+
  #scale_y_log10()+
  labs(y = paste0(" Percent of Threatened Species"))+
  #scale_fill_manual(values = colors)+
  theme(legend.title = element_blank(),
        legend.position ="top",
        plot.title=element_text(size=15, 
                                face="bold",
                                family="American Typewriter",
                                hjust=0.5,
                                lineheight=1.2),# title
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey50"),
        #strip.background =element_rect(fill="white"),
        plot.subtitle=element_text(size=15, 
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),  # subtitle
        axis.text.x  =  element_text(size=8, 
                                     family="American Typewriter",
                                     angle = 63,
                                     hjust=1,
                                     vjust =1),
        axis.text.y  =  element_text(size=8, 
                                     family="American Typewriter",
                                     hjust=0.5),
        axis.title =  element_text(size=10, 
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbp3) 

print(p)
# dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
dpi <- 400
ggsave(
  paste0(folderResults,folderGraph,"/FINAL3/threatTables_all_percentage_threat_reptiles_",dpi,"v1.png"),
  #ggplot_alternative(),
  width = 12,
  height = 5,
  dpi = dpi
)
