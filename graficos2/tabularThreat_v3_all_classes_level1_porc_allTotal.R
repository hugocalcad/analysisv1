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
  group_by(className, level1) %>%
  summarize(richness = n(),
            threat = sum(rlCodes %in% c("CR","EN", "VU"))) %>%
  mutate(percent = (threat/richness)*100)
df3 <- df2 %>%
  group_by(className) %>%
  summarise(richness = sum(richness),
    threat = sum(threat))

df2$totalThreat <- 0
df2$totalRichness <- 0
for(i in 1:nrow(df3)){
  df2$totalThreat[which(df2$className == df3$className[i])] <- df3$threat[i]
  df2$totalRichness[which(df2$className == df3$className[i])] <- df3$richness[i]
}
df2$percent2 <- (df2$threat/df2$totalThreat)*100
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
p<- ggplot(df2) +
  geom_bar(aes(x=as.factor(nlevel1), y = percent2, fill= className), stat = "identity", position = "dodge") +
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
                                   hjust=0.5),
        axis.title.x = element_blank()) + 
  scale_fill_manual(values = cbp2)

print(p)
# dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
ggsave(
  paste0(folderResults,folderGraph,"/threatTables_all_percentage_threat_totalThreat.png"),
  #ggplot_alternative(),
  width = 10,
  height = 5,
  dpi = 300
)


