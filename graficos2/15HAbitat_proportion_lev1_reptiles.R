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
  select(!c("code", "name", "level2", "level2S")) %>%
  distinct()

listHabitats <- c("Forest" = 1, "Savanna" = 2, "Shrubland" = 3, "Grassland" =4,
                  "Wetlands (inland)"= 5, "Rocky Areas" = 6, "Caves & Subterranean" = 7,
                  "Desert" =8, "Marine Neritic" = 9, "Marine Oceanic" = 10, "Marine Deep Ocean Floor" = 11,
                  "Marine Intertidal"= 12, "Marine Coastal/Supratidal" = 13, "Artificial - Terrestrial" = 14, 
                  "Artificial - Aquatic" = 15)
df_postgre3$nlevel1 <- as.character(df_postgre3$level1)
for(i in 1:length(listHabitats)){
  df_postgre3$nlevel1[which(df_postgre3$nlevel1 == listHabitats[i])] = names(listHabitats[i])
}
df_postgre3$level1 <- as.factor(df_postgre3$level1)

df2 <- df_postgre3 %>%
  filter(!level1 %in% c(9,10,11,12,13,14,15,16,17,18))%>%
  filter(className == "REPTILIA") %>%
  group_by(orderName2, nlevel1) %>%
  summarize(richness = n(),
            threat = sum(rlCodes %in% c("CR","EN", "VU"))) %>%
  mutate(percent = (threat/richness)*100) %>%
  filter(percent != 0)
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
df2$percent <- (df2$richness/df2$totalRichness)*100
df2$percent2 <- (df2$threat/df2$totalThreat)*100
df2$percent3 <- (df2$threat/df2$totalRichness)*100
## create ggplot


# df2$nlevel1 <- factor(df2$nlevel1, levels = c("Residential & commercial development", "Agriculture & aquaculture", 
#                                               "Energy production & mining", "Transportation & service corridors",
#                                               "Biological resource use", "Human intrusions & disturbance", 
#                                               "Natural system modifications", "Invasive & other problematic species, genes & diseases", 
#                                               "Pollution", "Geological events", "Climate change & severe weather",
#                                               "Others"))
# The palette with grey:
df2 <- df2 %>% tidyr::complete(orderName2, nlevel1= unique(df2$nlevel1), fill = list(richness=0, threat = 0, percent = 0, percent2 = 0, percent3 = 0) )
df4 <- df2
df4$orderName2 <- paste0(df4$orderName2, " THREAT")
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73")
cbp2 <- c("#528be1","#528be1",  "#b87750","#b87750",  "#50aa65","#50aa65",  "#666760","#666760")

cbp3 <- c( "#67001f", "#67001f", "#f4a582", "#f4a582", "#d6604d", "#d6604d", "#ffffbf", "#ffffbf")
cbp3 <- c( "#cccccc", "#cccccc", "#969696", "#969696","#636363", "#636363", "#252525", "#252525")
p<- ggplot() +
  geom_bar(data = df2,aes(x=as.factor(nlevel1), y = percent, fill= orderName2), alpha = 0.5, stat = "identity", position = "dodge") +
  geom_bar(data= df4, aes(x=as.factor(nlevel1), y = percent3, fill= orderName2), stat = "identity", position = "dodge")+
  #scale_y_log10()+
  #facet_wrap(~level1, scales = "free")+
  #scale_y_log10()+
  labs(y = paste0("Proportion of Species"), x = "Habitat")+
  #scale_fill_manual(values = colors)+
  theme(legend.title = element_blank(),
        legend.position ="top",
        plot.title=element_text(size=15, 
                                face="bold",
                                family="American Typewriter",
                                hjust=0.5,
                                lineheight=1.2),# title
        strip.background =element_rect(fill="white"),
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
                                   hjust=0.5))+
  #axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbp3)+
  guides(fill = guide_legend(override.aes = list(alpha = c(0.25,1,0.25,1,0.25,1,0.25,1))))+
  geom_text(data = df2,aes(x=as.factor(nlevel1), y = percent, fill= orderName2, label = ifelse(percent == 0, "", paste0(round(percent,1),"%"))), 
            size=1.85, hjust=0.5, vjust =-0.2, position=position_dodge(width=0.9), 
            angle = 0)+
  geom_text(data = df4,aes(x=as.factor(nlevel1), y = -2, fill= orderName2, label = ifelse(percent3 == 0, "", paste0(round(percent3,1),"%"))), 
            size=1.85, hjust=0.5, vjust =-0.2, position=position_dodge(width=0.9), 
            angle = 0)

# guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)),
#                                             colour=NA))) 

print(p)
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
dpi <- 400
ggsave(
  paste0(folderResults,folderGraph,"/FINAL4/ThreathHabitatLEvel1ProportionsReptils_",dpi,"_v2.png"),
  #ggplot_alternative(),
  width = 10,
  height = 5,
  dpi = dpi
)
df_postgres4 <-df_postgre3 %>%
  filter(!level1 %in% c(9,10,11,12,13,14,15,16,17,18))

write.csv(df_postgres4, paste0(folderResults, folderGraph, "/FINAL4/ThreathHabitatLEvel1_",dpi,"_v1.csv"))
