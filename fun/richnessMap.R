# Open Data and set variables
myMap <- function(rstSpecie, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                  title, formula, palette = 1, folderResults, oceans = 1){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  rstMask[!is.na(rstMask)] <- 1
  if(oceans == 1){
    rstSpecie[is.na(rstMask) & rstSpecie < 1] <- NA 
  }else{
    rstSpecie[is.na(rstMask)] <- NA
  }
  #rstSpecie[rstSpecie == 0] <- NA
  rstSpecieG <- projectRaster(rstSpecie, crs = crs_to, method = 'ngb')
  purpleToRed <- c('#49006a', '#7a0177', '#ae017e', '#dd3497', '#f768a1', '#fa9fb5', '#fcc5c0', 
                 '#c7e9b4', '#7fcdbb', '#41b6c4', '#1d91c0',
                 '#ffffb2', '#ffffb2', '#feb24c', '#fd8d3c', '#ef3b2c', '#cb181d', '#99000d')
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
  maxV <- maxValue(rstSpecieG)
  minV <-minValue(rstSpecieG)
  if(maxV >= 10){
    nbreak <- 10
  }else{
    nbreak <- maxV
  }
  if(nbreak == 0){
    nbreak <- 1
  }
  scale_c <- 10
  valuesBreak <- ceiling(seq(minV,maxV,
                             (maxV-minV)/nbreak))
  valuesBreak[length(valuesBreak)] <- maxV
  ggplot() + 
    #layer_spatial(rstMask, aes(fill = stat(band1)))+
    layer_spatial(rstSpecieG, aes(colour = stat(band1))) +
    scale_fill_gradientn("Richness", colours = scale_colors(scale_c),
                         guide = "colourbar",
                        na.value = NA, breaks = valuesBreak) +
    labs(title=title2, subtitle = formula,
         fill = "Richness") +
    theme(plot.title=element_text(size=20, 
                                  face="bold", 
                                  family="American Typewriter",
                                  hjust=0.5,
                                  lineheight=1.2),  # title
          plot.subtitle=element_text(size=12, 
                                     family="American Typewriter",
                                     face="bold",
                                     hjust=0.5),  # subtitle
          plot.caption=element_text(size=15),
          legend.key = element_rect(fill = "lightblue", color = NA),
          # Change legend key size and key width
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
    dpi = 300
  )
}

myMap3 <- function(rstSpecie, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                  title, formula, palette = 1, folderResults){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(scales)
  rstMask[!is.na(rstMask)] <- 0
  rstSpecie[is.na(rstMask)] <- NA
  #rstSpecie[rstSpecie == 0] <- NA
  rstSpecieG <- projectRaster(rstSpecie, crs = crs_to, method = 'ngb')
  rstMaskG <- projectRaster(rstMask, crs = crs_to, method = 'ngb')
  purpleToRed <- c('#49006a', '#7a0177', '#ae017e', '#dd3497', '#f768a1', '#fa9fb5', '#fcc5c0', 
                   '#c7e9b4', '#7fcdbb', '#41b6c4', '#1d91c0',
                   '#ffffb2', '#ffffb2', '#feb24c', '#fd8d3c', '#ef3b2c', '#cb181d', '#99000d')
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
  maxV <- maxValue(rstSpecieG)
  minV <-minValue(rstSpecieG)
  valuesBreak <- seq(minV,maxV,round(((maxV-minV)/7),2))
  valuesBreak[length(valuesBreak)] <- maxV
  ggplot() + 
    layer_spatial(rstMaskG, aes(fill = stat(band1)))+
    layer_spatial(rstSpecieG, aes(colour = stat(band1))) +
    scale_fill_gradientn("", colours = scale_colors(10),
                         na.value = NA, breaks = valuesBreak) +
    labs(title=title2, subtitle = formula,
         fill = "") +
    theme(plot.title=element_text(size=20, 
                                  face="bold", 
                                  family="American Typewriter",
                                  hjust=0.5,
                                  lineheight=1.2),  # title
          plot.subtitle=element_text(size=12, 
                                     family="American Typewriter",
                                     face="bold",
                                     hjust=0.5),  # subtitle
          plot.caption=element_text(size=15),
          legend.key = element_rect(fill = "lightblue", color = NA),
          # Change legend key size and key width
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
    dpi = 300
  )
}

myGraph1 <- function(rstSpecie, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                   title = "",formula = "", folderResults){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(scales)
  #Projecting raster
  #Projecting raster to equalarea (Goodie homolisine)
  rstSpecieG <-projectRaster(rstSpecie, crs = crs_to, method = 'ngb')
  rstMaskG <-projectRaster(rstMask, crs = crs_to)
  # Get a raster only values > 0
  rstSpecieG2 <- rstSpecieG
  rstSpecieG2[is.na(rstMaskG)] <- 0
  rstSpecieG2[rstSpecieG2 == 0] <- NA
  ##
  rstSpecieG3 <- rstSpecieG2
  rstSpecieG3[rstSpecieG3 > 0] <- 1
  ## Getting the mean species distribution by latitude
  sumSpeciesLat <- rowSums(as.matrix(rstSpecieG2), na.rm = T)
  sumPixelUsedLat <- rowSums(as.matrix(rstSpecieG3), na.rm = T)
  meanSpecieaLat <- rowMeans(as.matrix(rstSpecieG2), na.rm = T)
  meanSpecieaLat[is.na(meanSpecieaLat)] <- 0
  varSpecieaLat <- rowVars(as.matrix(rstSpecieG2), na.rm = T)
  sdSpeciesLat <- sqrt(varSpecieaLat)
  dtSpecieaLat <- rowVars(as.matrix(rstSpecieG2), na.rm = T)
  meanSpecieaLat[is.na(meanSpecieaLat)] <- 0
  denPixelSpeciesLat <- sumSpeciesLat/sumPixelUsedLat
  ##Geting y-axis values to plot
  stepsize = (rstSpecieG@extent@ymax - rstSpecieG@extent@ymin) / rstSpecieG@nrows
  yvals = seq(rstSpecieG@extent@ymax - stepsize / 2, rstSpecieG@extent@ymin, -stepsize)
  seqSpecies <- seq(1:384)
  ## 
  df_latitudeMean <- data.frame(seqSpecies, meanSpecieaLat, varSpecieaLat, sdSpeciesLat)
  p<-ggplot() + 
    layer_spatial(rstSpecieG2, aes(colour = stat(band1))) 
  colors <- c("Mean" = "green", "Standard deviation" = "red")
  ggplot(df_latitudeMean) +
    geom_line(aes(x=meanSpecieaLat, y=yvals, color = "Mean"), orientation = "y", ) +
    geom_line(aes(x=sdSpeciesLat, y=yvals, color = "Standard deviation"), orientation = "y") +
    labs(x=paste0("Mean species density - ",title))+
    theme(axis.title.y=element_blank(),
          axis.text.y =element_blank(),
          axis.ticks.y =element_blank(),
          legend.title =element_blank(),
          legend.position = "top",
          legend.text = element_text(size=10, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.title.x = element_text(size=10, 
                                      family="American Typewriter",
                                      hjust=0.5))+
    scale_color_manual(values = colors) 
  ggsave(
    paste0(folderResults,"/mean_species_density_",title,".png"),
    #ggplot_alternative(),
    width = 10,
    height = 5,
    dpi = 300
  )
}