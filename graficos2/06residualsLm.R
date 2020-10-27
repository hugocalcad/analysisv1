library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "reptiles"
classNameBS <- list("amphibians" = "AMPHIBIA","birds" = "AVES", "mammals" ="MAMMALIA", "reptiles" = "REPTILIA")
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_",className,"_082020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphics06"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA02/mask_50.tif')
crs_to <- '+proj=igh'
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
## plotting order
df <- read.csv(fileList)
orders <- df %>% distinct(orderName) %>% pull()
## plotting threatment
rst_EN <- raster(paste0(folderResults, 'sum_EN.tif'))
rst_VU <- raster(paste0(folderResults, 'sum_VU.tif'))
rst_CR <- raster(paste0(folderResults, 'sum_CR.tif'))
rst_DD <- raster(paste0(folderResults, 'sum_CR.tif'))
rstThreatment <- rst_EN + rst_VU + rst_CR
rstSpecie[rstSpecie == 0] <- NA
rstThreatment[rstSpecie == 0] <- NA 
s <- stack(rstSpecie, rstThreatment)
v <- data.frame(na.omit(values(s)))
names(v) <- c("species", "threat")
names(s) <- c("species", "threat")
m <- lm(threat ~ species, data=v)
p <- predict(s$species, m)
residuals <- s$threat - p
residuals[rstSpecie ]
s2 <- stack(rstSpecie, rstThreatment)
v2 <- data.frame(na.omit(values(s2)))
names(v2) <- c("species", "threat")
# 
m2 <- lm(threat ~ species, data = v2)
v2$threat_p <- predict(m2)
v2$residuals <- residuals(m2)
v2 <- v2 %>% filter(species != 0)
##
title2 <- paste0(title, "_Residual_(Threatened ~ Richness)")
title3 = gsub("_", " ", title2)
ggplot(v2, aes(x = species, y = threat)) +
  #geom_segment(aes(xend = species, yend = threat_p), alpha = .2) +
  # > Color adjustments made here...
  geom_point(aes(color = residuals), alpha = 10) +  # Color mapped here
  guides(color = FALSE) +
  # <
  #geom_point(aes(y = threat_p), shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_bw()+
  labs(title = title3, x = "Richness", y = "Threatened") +
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family="American Typewriter",
                                hjust=0.5,
                                lineheight=1.2),# title
        strip.background =element_rect(fill="white"),
        plot.subtitle=element_text(size=15, 
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),  # subtitle
        axis.text =  element_text(size=20, 
                                  family="American Typewriter",
                                  hjust=0.5),
        axis.title =  element_text(size=20, 
                                  family="American Typewriter",
                                  face="bold",
                                  hjust=0.5),
        )
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)

ggsave(
  paste0(paste0(folderResults, folderGraph,"/residual_plot_",title,".png")),
  #ggplot_alternative(),
  width = 10,
  height = 10,
  dpi = 300
)

myMap2_2(residuals, rstMask, crs_to = crs_to , 
                  title2, "", palette = 1, paste0(folderResults, folderGraph), oceans = 0)
