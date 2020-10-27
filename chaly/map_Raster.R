library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(rnaturalearth)
library(classInt)
# Open Data and set variables to change (amphibians, birds, mammals, reptiles)
className <- "birds"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/ResultsChaly01/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
scaleFun <- "fisher"
folderGraph <- "graphics01"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'

countries <- ne_countries(country = c('Sierra Leone', 'Liberia'), type='countries', returnclass = 'sf') %>%
  dplyr::select(name)
linesL = st_cast(countries,"LINESTRING")
maxV <- maxValue(rstSpecie)
rstSpecie[rstSpecie == 0] <- NA
value.vector <- values(rstSpecie)
breaks.qt <- classIntervals(value.vector, n = 4, style = "fisher", intervalClosure = "left")
breaks.qt$brks <- round(breaks.qt$brks, 0)
breaks.qt$brks[length(breaks.qt$brks)] <- maxV
ggplot()+
  layer_spatial(rstSpecie, aes(colour = stat(band1)))+
  scale_fill_viridis_c("Species", guide = guide_colourbar(),
                       na.value = NA, breaks = breaks.qt$brks)+
  geom_sf(data = linesL)+
  labs(title = paste0(className2," (Liberia- Sierra Lione)"))+
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family="American Typewriter",
                                hjust=0.5,
                                lineheight=1.2),
        legend.key = element_rect(fill = "lightblue", color = NA),
        legend.key.size = unit(2.5, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.text = element_text(size=8, 
                                   family="American Typewriter",
                                   hjust=0.5),
        legend.title = element_text(size=10, 
                                    face="bold", 
                                    family="American Typewriter",
                                    hjust=0.5,
                                    lineheight=1.2))
