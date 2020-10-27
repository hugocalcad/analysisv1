library(ggplot2)
library(RColorBrewer)
library(dplyr)
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
query <- paste0("SELECT tax.\"scientificName\", tax.\"className\", tax.\"orderName\",
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
df_SpeciesReptilesSQUAMATAS <- df_SpeciesReptiles %>%
  filter(orderName %in% c("SAURIA","SERPENTES"))


df2 <- df_postgres %>%
  filter(className == "REPTILIA") %>%
  group_by(orderName, nlevel2) %>%
  summarize(richness = n(),
            threat = sum(rlCodes %in% c("CR","EN", "VU"))) %>%
  mutate(percent = (threat/richness)*100)
df3 <- df2 %>%
  group_by(orderName) %>%
  summarise(richness = sum(richness),
            threat = sum(threat))

df2$totalThreat <- 0
df2$totalRichness <- 0
for(i in 1:nrow(df3)){
  df2$totalThreat[which(df2$orderName == df3$orderName[i])] <- df3$threat[i]
  df2$totalRichness[which(df2$orderName == df3$orderName[i])] <- df3$richness[i]
}
df2$percent2 <- (df2$threat/df2$totalThreat)*100

df4 <- df2 %>% filter(percent2 !=0)
df_postgres2 <- df_postgres %>% filter(rlCodes %in% c("CR","EN", "VU"))

## create ggplot

df <- df_postgres2 %>% filter(className == "REPTILIA") %>%
  dplyr::group_by(.dots = c('orderName', 'nlevel2')) %>%
  dplyr::summarize(counts = n()) %>%
  dplyr::mutate(perc = (counts / sum(counts)) * 100) %>%
  dplyr::arrange(desc(perc))

# preparing the plot
ggplot2::ggplot(df, aes('', counts)) +
  geom_col(
    position = 'fill',
    color = 'black',
    width = 1,
    aes(fill = factor(nlevel2))
  ) +
  facet_wrap(~orderName) +
  geom_label(
    aes(label = paste0(round(perc), "%"), group = factor(nlevel2)),
    position = position_fill(vjust = 0.5),
    color = 'black',
    size = 3,
    show.legend = FALSE
  ) +
  coord_polar(theta = "y")+
  labs (fill="Threat Level 1", x= "", y= "")


# dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
dpi <- 400
ggsave(
  paste0(folderResults,folderGraph,"/FINAL/threatTables_all_percentage_threat_reptiles_",dpi,"v2.png"),
  #ggplot_alternative(),
  width = 15,
  height = 5,
  dpi = dpi
)
