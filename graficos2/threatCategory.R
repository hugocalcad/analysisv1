library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(stars)
library(RPostgreSQL)
library(tidyverse)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "mammals"
classNameBS <- list("amphibians" = "AMPHIBIA","birds" = "AVES", "mammals" ="MAMMALIA", "reptiles" = "REPTILIA")
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
subtitle ='Coast Distance - Relative Threat'
folderGraph <- "graphics10"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../fun/richnessMap.R")
df_postgres = read_csv(fileList)

#df_postgres2 <-na.omit()
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
colors <- c("LC" = "#38c457", "VU" = "#fbe946", "DD" = "#dfd9d3", "EX" = "#37292f",
            "CR" = "#d8001d", "EN" = "#ff683f", "NT" = "#bee447","EW" = "#471c36")

df2 <- df_postgres %>% 
  dplyr::mutate(rlCodes = factor(rlCodes, 
                                 levels = c("EX", "EW", "CR",
                                            "EN", "VU", "NT", "LC", "DD"))) %>%
  group_by(rlCodes) %>%
  drop_na() %>%
  summarise(n=n(),
            per = (n/nrow(df_postgres)),2)

dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
## create ggplot
p<- ggplot(df2, aes(x=rlCodes, y = n, fill= rlCodes)) +  
  geom_bar(position = 'dodge', stat='identity',width = 0.5) +
  #scale_y_log10()+
  labs(title = paste0(title," - Red List Categories"), 
       fill = "Red List Category")+
  scale_fill_manual(values = colors)+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

print(p)
ggsave(
  paste0(folderResults,folderGraph,"/sum_categories_",title,".png"),
  #ggplot_alternative(),
  width = 10,
  height = 10,
  dpi = 300
)
codesGroup <- c("EX" = 1, "EW" = 1, "CR" = 2,
                "EN" = 2, "VU" = 2, "NT" = 3, "LC" = 3, "DD" = 3)

df2$group <- ""
for(i in 1:length(codesGroup)){
  df2$group[which(df2$rlCodes == names(codesGroup[i]))] = codesGroup[i]
}

p2<- ggplot(df2, aes(x=rlCodes, y = n, fill= rlCodes)) +  
  geom_bar(position = 'dodge', stat='identity',width = 0.5) +
  #scale_y_log10()+
  labs(title = paste0(title," - Red List Categories"), 
       fill = "Red List Category")+
  scale_fill_manual(values = colors)+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  facet_wrap(~group, scales = "free")
print(p2)
ggsave(
  paste0(folderResults,folderGraph,"/sum_categories_grouped_",title,".png"),
  #ggplot_alternative(),
  width = 10,
  height = 10,
  dpi = 300
)
p3<- ggplot(df2, aes(x=rlCodes, y = per, fill= rlCodes)) +  
  geom_bar(position = 'dodge', stat='identity',width = 0.5) +
  scale_y_continuous(labels=scales::percent) +
  #scale_y_log10()+
  labs(title = paste0(title," - Red List Categories"), 
       fill = "Red List Category")+
  scale_fill_manual(values = colors)+
  geom_text(aes(label=paste0(round(per*100,2), " %")), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
print(p3)
ggsave(
  paste0(folderResults,folderGraph,"/porcentaje_categories_grouped_",title,".png"),
  #ggplot_alternative(),
  width = 10,
  height = 10,
  dpi = 300
)

