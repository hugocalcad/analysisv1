library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(stars)
library(tidyverse)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "reptiles"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_",className,"_082020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")
folderGraph <- "graphics08"
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
subtitle ='Coast Distance - Relative Threat'
#folderGraph <- "graphics01"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA02/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../fun/richnessMap.R")

## open raster con distancia a la costa
rstDist <- raster("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/shps/dist_coast/dist1.tif")
rst_EN <- raster(paste0(folderResults, 'sum_EN.tif'))
rst_VU <- raster(paste0(folderResults, 'sum_VU.tif'))
rst_CR <- raster(paste0(folderResults, 'sum_CR.tif'))
rst_DD <- raster(paste0(folderResults, 'sum_DD.tif'))
rstRichnesThreat <- rst_EN + rst_VU + rst_CR
rstRelativeThreat <-  (rstRichnesThreat/rstSpecie)*100
rstStack <- stack(rstRichnesThreat,rstRelativeThreat, rstSpecie, rstDist)
names(rstStack) <- c("threat", "relativeThreat", "richness", "dist")
tif=st_as_stars(rstStack)
sf_specie_dist=st_as_sf(tif)
sf_specie_dist2 <- sf_specie_dist%>% drop_na()
##
hist(sf_specie_dist2$relativeThreat, breaks = 30)
ggplot(sf_specie_dist2) +
  geom_histogram(aes(x=relativeThreat), bins = 240)
# ggplot(sf_specie_dist2, aes(x = layer.2, y = dist1)) +
#   geom_bin2d()
#   #geom_segment(aes(xend = species, yend = threat_p), alpha = .2) +
#   # > Color adjustments made here...
#   geom_point(aes(color = residuals)) +  # Color mapped here
#   scale_color_gradient2(low = "blue", mid = "yellow", high = "red") +  # Colors to use here
#   guides(color = FALSE) +
#   # <
#   #geom_point(aes(y = threat_p), shape = 1) +
#   geom_smooth(method = "lm", se = FALSE, color = "red") +
#   theme_bw()+
#   labs(title = title, x = "Richness", y = "Threatened")
  
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
## firsta graph
p <-ggplot(sf_specie_dist2) + geom_hex(aes(dist, relativeThreat), bins = 50) +
  #scale_fill_gradientn("", colours = rev(rainbow(10, end = 4/6)), trans = "log10")+
  #theme_bw() +
  viridis::scale_fill_viridis(trans = "log10") +
  labs(title = title, subtitle = subtitle, x = "Distance (km)", 
       y = "Relative Threat", fill = "Pixel density") +
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family="American Typewriter",
                                hjust=0.5,
                                lineheight=1.2),# title
        strip.background =element_rect(fill="white"),
        plot.subtitle=element_text(size=12, 
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),  # subtitle
        axis.text =  element_text(size=15, 
                                  family="American Typewriter",
                                  face="bold",
                                  hjust=0.5),
        axis.title =  element_text(size=15, 
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),
        legend.title = element_text(size=10, 
                                    family="American Typewriter",
                                    face="bold",
                                    hjust=0.5),
        legend.text =  element_text(size=10, 
                                    family="American Typewriter",
                                    hjust=0.5),
        legend.position = c(0.93,0.85)
  )
print(p)
ggsave(
    paste0(folderResults, folderGraph,"/distCoastRelatThreat_",title,".png"),
    #ggplot_alternative(),
    width = 10,
    height = 5,
    dpi = 300
)


## Secon graph option B
subtitle <- "Coast Distance - Threatened species"
p <-ggplot(sf_specie_dist2, aes(dist, threat)) +
  geom_hex(bins = 50)+
  viridis::scale_fill_viridis(trans = "log10") +
  #scale_y_log2()+
  labs(title = title, subtitle = subtitle, x = "Distance (km)", 
       y = "Threatened species", fill = "Pixel density") +
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family="American Typewriter",
                                hjust=0.5,
                                lineheight=1.2),# title
        strip.background =element_rect(fill="white"),
        plot.subtitle=element_text(size=12, 
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),  # subtitle
        axis.text =  element_text(size=15, 
                                  family="American Typewriter",
                                  face="bold",
                                  hjust=0.5),
        axis.title =  element_text(size=15, 
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),
        legend.title = element_text(size=10, 
                                    family="American Typewriter",
                                    face="bold",
                                    hjust=0.5),
        legend.text =  element_text(size=10, 
                                    family="American Typewriter",
                                    hjust=0.5),
        legend.position = c(0.93,0.85)
  )
print(p)
ggsave(
  paste0(folderResults, folderGraph,"/distCoastThreat_",title,".png"),
  #ggplot_alternative(),
  width = 10,
  height = 5,
  dpi = 300
)
