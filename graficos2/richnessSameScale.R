library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)

myMap4 <- function(rstSpecie, rstMask, rstTable, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                     title, formula, palette = 1, folderResults, oceans = 1){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(stars)
  library(classInt)
  #rstMask[!is.na(rstMask)] <- 1
  if(oceans == 0){
    rstSpecie[is.na(rstMask)] <- NA
  }
  rstSpecie[rstSpecie == 0] <- NA
  rstSpecieG <- projectRaster(rstSpecie, crs = crs_to, method = 'ngb')
  rstMaskG <- projectRaster(rstMask, crs = crs_to, method = 'ngb')
  purpleToRed <- c('#320aee', '#2596e1',  '#7AB2D4', '#E85D00', '#FF0000')
  blueToRed <- c( '#08589e', '#2b8cbe', '#4eb3d3','#7bccc4', '#a8ddb5','#ccebc5',
                  '#ffff00', '#feb24c','#fd8d3c', '#fc4e2a', '#e31a1c', '#b10026')
  if (palette == 1){
    scale_colors <- blueToRed %>%
      colorRampPalette()
  }
  else{
    scale_colors <- purpleToRed %>%
      colorRampPalette()
  }
  title2 <- gsub("_", " ", title)
  maxV <- maxValue(rstTable)
  minV <-minValue(rstSpecieG)
  value.vector <- values(rstSpecieG)
  if(maxV >= 8){
    nbreak <- 8
  }else{
    nbreak <- maxV
  }
  if(nbreak == 0){
    nbreak <- 1
  }
  #breaks.qt <- classIntervals(value.vector, n = nbreak, style = "headtails", intervalClosure = "right")
  scale_c <- 10
  valuesBreak <- ceiling(seq((minV),maxV,
                             (maxV-(minV))/nbreak))
  valuesBreak[length(valuesBreak)] <- maxV
  
  tif <- st_as_stars(rstMaskG)
  sf=st_as_sf(tif)
  sf_MaskG <- st_combine(sf)
  ggplot() + 
    #layer_spatial(rstMask, aes(fill = stat(band1)))+
    layer_spatial(sf_MaskG, col = "gray")+
    layer_spatial(rstSpecieG, aes(colour = stat(band1))) +
    scale_fill_gradientn("Richness", colours = scale_colors(scale_c),
                         guide = "colourbar",
                         na.value = NA, breaks = valuesBreak,
                         limits = c(minV, maxV)) +
    # scale_fill_viridis_c("Richness", #colours = scale_colors(scale_c),
    #                      guide = "colourbar",
    #                      option = "plasma",
    #                      na.value = NA, 
    #                      breaks = breaks.qt$brks,
    #                      limits = c(minV, maxV))+
    labs(title=title2, subtitle = formula,
         fill = "Richness") +
    theme_light() +
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
          plot.caption=element_text(size=15),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_blank(),
          legend.key = element_rect(fill = "lightblue", color = NA),
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(0.5,"cm"),
          legend.text = element_text(size=6, 
                                     family="American Typewriter",
                                     hjust=0.5),
          legend.title = element_text(size=10, 
                                      face="bold", 
                                      family="American Typewriter",
                                      hjust=0.5,
                                      lineheight=1.2)) 
  ggsave(
    paste0(folderResults,"/sum_all_",title,".png"),
    #ggplot_alternative(),
    width = 10,
    height = 5,
    dpi = 300,
    bg = "transparent"
  )
}

# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
className <- "birds"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
folderGraph <- "graphics11"
title <- paste0(className2) 
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
myMap4(rstSpecie, rstMask,rstSpecie, crs_to, title, "", 1, paste0(folderResults, folderGraph), oceans = 0)

##
rstTable <- rstSpecie
##

className <- "mammals"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphics11"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
myMap4(rstSpecie, rstMask,rstTable, crs_to, title, "", 1, paste0(folderResults, folderGraph), oceans = 0)

className <- "reptiles"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphics11"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
myMap4(rstSpecie, rstMask,rstTable, crs_to, title, "", 1, paste0(folderResults, folderGraph), oceans = 0)

className <- "amphibians"
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphics11"
rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
crs_to <- '+proj=igh'
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
myMap4(rstSpecie, rstMask,rstTable, crs_to, title, "", 1, paste0(folderResults, folderGraph), oceans = 0)