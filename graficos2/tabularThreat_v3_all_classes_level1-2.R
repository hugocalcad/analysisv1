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
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/")
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
                 user="postgres", password="67huguito82")  
##query
query <- paste0("SELECT tax.\"scientificName\", tax.\"className\",
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

df2 <- df_postgres %>%
  group_by(className, level1, level2S) %>%
  summarize(richness = n(),
            threat = sum(rlCodes %in% c("CR","EN", "VU"))) %>%
  mutate(percent = (threat/richness)*100)
## create ggplot
listThreats <- c("Residential & commercial development" = 1, "Agriculture & aquaculture" = 2, 
                 "Energy production & mining" = 3, "Transportation & service corridors" =4,
                 "Biological resource use"= 5, "Human intrusions & disturbance" = 6, 
                 "Natural system modifications" = 7, "Invasive & other problematic species, genes & diseases" =8, 
                 "Pollution" = 9, "Geological events" = 10, "Climate change & severe weather" = 11,
                 "Others"= 12)
df2$nlevel1 <- as.character(df2$level1)
for(i in 1:length(listThreats)){
  df2$nlevel1[which(df2$nlevel1 == listThreats[i])] = names(listThreats[i])
}
##Super level2 creation
listThreatslevel2 <- c("Housing & urban areas" = "1.1", "Commercial & industrial areas" = "1.2", 
                 "Tourism & recreation areas" = "1.3", "Annual & perennial non-timber crops " ="2.1",
                 "Wood & pulp plantations"= "2.2", "Livestock farming & ranching" = "2.3", 
                 "Marine & freshwater aquaculture" = "2.4", "Oil & gas drilling" ="3.1", 
                 "Mining & quarrying" = "3.2", "Renewable energy" = "3.3", "Roads & railroads" = "4.1",
                 "Utility & service lines"= "4.2", "Shipping lanes"= "4.3", "Flight paths"= "4.4",
                 "Hunting & collecting terrestrial animals" = "5.1","Gathering terrestrial plants " = "5.2","Logging & wood harvesting " = "5.3",
                 "Fishing & harvesting aquatic resources " = "5.4","Recreational activities" = "6.1","War, civil unrest & military exercises" = "6.2",
                 "Work & other activities" = "6.3", "Fire & fire suppression " = "7.1", "Dams & water management/use" = "7.2", 
                 "Other ecosystem modifications" = "7.3","Invasive non-native/alien species/diseases " = "8.1","Problematic native species/diseases " = "8.2",
                 "Introduced genetic material" = "8.3","Problematic species/diseases of unknown origin " = "8.4","Viral/prion-induced diseases " = "8.5",
                 "Diseases of unknown cause " = "8.6","Domestic & urban waste water " = "9.1","Industrial & military effluents " = "9.2",
                 "Agricultural & forestry effluents " = "9.3","Garbage & solid waste" = "9.4","Air-borne pollutants " = "9.5",
                 "Excess energy "="9.6", "Volcanoes"="10.1","Earthquakes/tsunamis"="10.2",
                 "Avalanches/landslides"="10.3","Habitat shifting & alteration"="11.1","Droughts"="11.2",
                 "Temperature extremes"="11.3","Storms & flooding"="11.4","Other impacts"="11.5", "Other threat"="12.1")
df2$nlevel2S <- df2$level2S
for(i in 1:length(listThreatslevel2)){
  df2$nlevel2S[which(df2$nlevel2S == listThreatslevel2[i])] = names(listThreatslevel2[i])
}

##
df2$level1 <- as.factor(df2$level1)
df2$nlevel1 <- factor(df2$nlevel1, levels = c("Residential & commercial development", "Agriculture & aquaculture", 
                                              "Energy production & mining", "Transportation & service corridors",
                                              "Biological resource use", "Human intrusions & disturbance", 
                                              "Natural system modifications", "Invasive & other problematic species, genes & diseases", 
                                              "Pollution", "Geological events", "Climate change & severe weather",
                                              "Others"))
# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73")
cbp2 <- c("#528be1",  "#b87750",  "#50aa65",  "#666760")
df3 <- df2 %>% filter(level1 %in% c("10","11","12") )
p<- ggplot(df3, aes(x=nlevel2S, y = percent, fill = className)) +
  geom_col(position = position_dodge()) +
  #theme_void() +
  labs(y = paste0(" Percent of Threatened Species"))+
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x=element_text(size=12),
    axis.text.y=element_text(size=10),
    axis.title.y=element_blank(),
    strip.placement="outside",
    strip.text.y=element_text(hjust=0, face="bold", size=10)
  ) +
  coord_flip() +
  facet_wrap(nlevel1 ~ ., scales = "free_y", ncol = 1, switch = "y", labeller=label_wrap_gen(width=15, multi_line=T)) +
  #scale_fill_brewer(palette = "Spectral")
  #scale_fill_viridis(discrete = T, option = "C")
  scale_fill_manual(values = cbp2) 
  
print(p)
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
# ggsave(
#   paste0(folderResults,folderGraph,"/threatTables_all_percentage_level2_4.png"),
#   #ggplot_alternative(),
#   width = 10,
#   height = 5,
#   dpi = 300,
#   scale = 1
# )


