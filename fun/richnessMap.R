# Open Data and set variables
myMapGeneric <- function(rstSpecie, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" ,
                         title = "", formula ="", palette = 1, folderResults, oceans = 1,
                         annotateScale = "", scaleFun = "quantile", nameSave= "test"){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(stars)
  library(classInt)
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
  }else{
    scale_colors <- purpleToRed %>%
      colorRampPalette()
  }
  title2 <- gsub("_", " ", title)
  maxV <- maxValue(rstSpecieG)
  minV <-minValue(rstSpecieG)
  value.vector <- values(rstSpecieG)
  nbreak <- 0
  if(is.na(minV)){
    minV <- 0
  }
  if(is.na(maxV)){
    scale_colors <- colorRampPalette(c('gray'))
    maxV <- 0
  }else if (maxV >= 8){
    nbreak <- 8
  }else{
    nbreak <- maxV
  }
  
  if(nbreak == 0){
    nbreak <- 1
  }
  scale_c <- 8
  nbreak <- 5
  if(!is.na(maxV) & maxV != minV){
    breaks.qt <- classIntervals(value.vector, n = nbreak, style = scaleFun, intervalClosure = "left", pal = blueToRed)
    if(scaleFun == "equal"){
      breaks.qt$brks <- round(breaks.qt$brks, 0) 
    }else{
      breaks.qt$brks <- round(breaks.qt$brks, 2) 
      #print(breaks.qt$brks)
    }
  } else{
    breaks.qt <- data.frame(columns = c("brks"))
    breaks.qt$brks <- c(1)
  }
  tif <- st_as_stars(rstMaskG)
  sf=st_as_sf(tif)
  sf_MaskG <- st_combine(sf)
  p <- ggplot() + 
    layer_spatial(sf_MaskG, col = "gray")+
    layer_spatial(rstSpecieG, aes(colour = stat(band1))) +
    scale_fill_gradientn("Richness", colours = scale_colors(scale_c),
                         guide = "colourbar",
                         na.value = NA, breaks = breaks.qt$brks) +
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
          legend.text = element_text(size=8, 
                                     family="American Typewriter",
                                     hjust=0.5),
          legend.title = element_text(size=10, 
                                      face="bold", 
                                      family="American Typewriter",
                                      hjust=0.5,
                                      lineheight=1.2)) 
  if(annotateScale != ""){
    p <- p + annotate(geom = 'label', label = annotateScale, 
                    x = Inf, y = Inf, hjust = 1, vjust = 1, fill = "white") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  }
  print(p)
  ggsave(
    paste0(folderResults,"/",nameSave,".png"),
    #ggplot_alternative(),
    width = 10,
    height = 5,
    dpi = 300,
    bg = "transparent"
  )
}
myMapGeneric2 <- function(rstSpecie, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" ,
                         title = "", formula ="", palette = 1, folderResults, oceans = 1,
                         annotateScale = "", scaleFun = "quantile", nameSave= "test"){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(stars)
  library(classInt)
  library(RColorBrewer)
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
  }else{
    scale_colors <- purpleToRed %>%
      colorRampPalette()
  }
  title2 <- gsub("_", " ", title)
  maxV <- maxValue(rstSpecieG)
  minV <-minValue(rstSpecieG)
  value.vector <- values(rstSpecieG)
  nbreak <- 0
  if(is.na(minV)){
    minV <- 0
  }
  if(is.na(maxV)){
    scale_colors <- colorRampPalette(c('gray'))
    maxV <- 0
  }else if (maxV >= 8){
    nbreak <- 8
  }else{
    nbreak <- maxV
  }
  
  if(nbreak == 0){
    nbreak <- 1
  }
  scale_c <- 8
  nbreak <- 10
  if(!is.na(maxV) & maxV != minV){
    breaks.qt <- classIntervals(value.vector, n = nbreak, style = scaleFun, intervalClosure = "left", pal = blueToRed)
    if(scaleFun == "equal"){
      breaks.qt$brks <- round(breaks.qt$brks, 0) 
    }else{
      breaks.qt$brks <- round(breaks.qt$brks, 2) 
      #print(breaks.qt$brks)
    }
  } else{
    breaks.qt <- data.frame(columns = c("brks"))
    breaks.qt$brks <- c(1)
  }
  tif <- st_as_stars(rstMaskG)
  sf=st_as_sf(tif)
  sf_MaskG <- st_combine(sf)
  p <- ggplot() + 
    layer_spatial(sf_MaskG, col = "gray")+
    layer_spatial(rstSpecieG, aes(colour = stat(band1))) +
    # scale_fill_gradientn("Richness", colours = scale_colors(scale_c),
    #                      guide = "colourbar",
    #                      na.value = NA, breaks = breaks.qt$brks) +
    scale_fill_fermenter(
      palette = "Spectral",
      na.value = NA, breaks = breaks.qt$brks
    )+
  
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
          legend.text = element_text(size=8, 
                                     family="American Typewriter",
                                     hjust=0.5),
          legend.title = element_text(size=10, 
                                      face="bold", 
                                      family="American Typewriter",
                                      hjust=0.5,
                                      lineheight=1.2)) 
  if(annotateScale != ""){
    p <- p + annotate(geom = 'label', label = annotateScale, 
                      x = Inf, y = Inf, hjust = 1, vjust = 1, fill = "white") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  }
  print(p)
  ggsave(
    paste0(folderResults,"/",nameSave,".png"),
    #ggplot_alternative(),
    width = 10,
    height = 5,
    dpi = 300,
    bg = "transparent"
  )
}

myMapViridis <- function(rstSpecie, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                         title = "", formula ="", palette = 1, folderResults, oceans = 1,
                         annotateScale = "", scaleFun = "quantile", nameSave= "test"){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(stars)
  library(classInt)
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
  } else{
    scale_colors <- purpleToRed %>%
      colorRampPalette()
  }
  title2 <- gsub("_", " ", title)
  maxV <- maxValue(rstSpecieG)
  minV <-minValue(rstSpecieG)
  value.vector <- values(rstSpecieG)
  nbreak <- 0
  if(is.na(minV)){
    minV <- 0
  }
  if(is.na(maxV)){
    scale_colors <- colorRampPalette(c('gray'))
    maxV <- 0
  }else if (maxV >= 8){
    nbreak <- 8
  }else{
    nbreak <- maxV
  }
  
  if(nbreak == 0){
    nbreak <- 1
  }
  scale_c <- 8
  nbreak <- 5
  if(!is.na(maxV)){
    breaks.qt <- classIntervals(value.vector, n = nbreak, style = scaleFun, intervalClosure = "right")
    if(scaleFun == "equal"){
      breaks.qt$brks <- round(breaks.qt$brks, 0) 
    }else{
      breaks.qt$brks <- round(breaks.qt$brks, 2) 
      print(breaks.qt$brks)
    }
  } else{
    breaks.qt <- data.frame(columns = c("brks"))
    breaks.qt$brks <- c(1)
  }
  tif <- st_as_stars(rstMaskG)
  sf=st_as_sf(tif)
  sf_MaskG <- st_combine(sf)
  p <- ggplot() + 
    layer_spatial(sf_MaskG, col = "gray")+
    layer_spatial(rstSpecieG, aes(colour = stat(band1))) +
    scale_fill_viridis_c("Richness", #colours = scale_colors(scale_c),
                         guide = "colourbar",
                         option = "magma",
                         na.value = NA, breaks = breaks.qt$brks)+
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
  if(annotateScale != ""){
    p <- p + annotate(geom = 'label', label = annotateScale, 
                      x = Inf, y = Inf, hjust = 1, vjust = 1, fill = "white") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  }
  print(p)
  ggsave(
    paste0(folderResults,"/",nameSave,".png"),
    #ggplot_alternative(),
    width = 10,
    height = 5,
    dpi = 300,
    bg = "transparent"
  )
}

myMap2_2 <- function(rstSpecie, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                   title, formula, palette = 1, folderResults, oceans = 1){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(stars)
  library(classInt)
  library(scales)
  # 
  # log_both <- function(x){ifelse(x == 0, 0, log(abs(x)) * sign(x))}
  # exp_both <- function(x){exp(abs(x)) * sign(x)} # this is the inverse of log_both
  # 
  # log_both_trans <- 
  #   function(){
  #     trans_new(name = 'log_both', 
  #               transform = log_both,
  #               inverse = exp_both)
  #   }
  
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
  blueToRed2 <- c( '#b2182b', '#d6604d', '#f4a582','#fddbc7', '#d1e5f0','#92c5de',
                  '#4393c3', '#2166ac')
  if (palette == 1){
    scale_colors <- blueToRed2 %>%
      colorRampPalette()
  }
  else{
    scale_colors <- purpleToRed %>%
      colorRampPalette()
  }
  title2 <- gsub("_", " ", title)
  maxV <- maxValue(rstSpecieG)
  minV <-minValue(rstSpecieG)
  value.vector <- values(rstSpecieG)
  if(maxV >= 4){
    nbreak <- 4
  }else{
    nbreak <- maxV
  }
  if(nbreak == 0){
    nbreak <- 1
  }
  value.vectorP <- value.vector
  value.vectorP <- value.vectorP[!is.na(value.vectorP)]
  value.vectorP <- value.vectorP[value.vectorP > 0]
  breaks.qt <- classIntervals(value.vectorP, n = nbreak, style = "fisher", intervalClosure = "left")
  breaks.qt$brks[length(breaks.qt$brks)] <- floor(breaks.qt$brks[length(breaks.qt$brks)])
  breaks.qt$brks[1] <- 0
  vals.qt <-round(sort(c(breaks.qt$brks, breaks.qt$brks[2:length(breaks.qt$brks)]*-1)),0)
  scale_c <- 10
  
  tif <- st_as_stars(rstMaskG)
  sf=st_as_sf(tif)
  sf_MaskG <- st_combine(sf)
  ggplot() + 
    #layer_spatial(rstMask, aes(fill = stat(band1)))+
    layer_spatial(sf_MaskG, col = "gray95")+
    layer_spatial(rstSpecieG, aes(colour = stat(band1))) +
    # scale_fill_gradientn("Richness", colours = scale_colors(scale_c),
    #                      guide = "colourbar",
    #                      na.value = NA, breaks = breaks.qt$brks,
    #                      limits = c(maxV*-1, maxV)) +
    scale_fill_gradient2("Richness", low="blue" ,mid="white" ,
                         high = "red",
                         #trans = "reciprocal",
                         guide = guide_colorbar(order = 1),
                         na.value = NA, breaks = vals.qt,
                         limits = c(maxV*-1, maxV)) +
    # scale_fill_viridis_c("Richness", #colours = scale_colors(scale_c),
    #                      guide = "colourbar",
    #                      option = "viridis",
    #                      na.value = NA, breaks = breaks.qt$brks,
    #                      limits = c(maxV*-1, maxV))+
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
          legend.text = element_text(size=8, 
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
myGraphLatitude <- function(rstSpecie, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                   title = "",formula = "", folderResults, fun = "mean"){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(scales)
  library(matrixStats)
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
  meanSpeciesLat <- rowMeans(as.matrix(rstSpecieG2), na.rm = T)
  medianSpeciesLat <- rowMedians(as.matrix(rstSpecieG2), na.rm = T)
  varSpeciesLat <- rowVars(as.matrix(rstSpecieG2), na.rm = T)
  sdSpeciesLat <- sqrt(varSpeciesLat)
  dtSpeciesLat <- rowVars(as.matrix(rstSpecieG2), na.rm = T)
  ##Geting y-axis values to plot
  stepsize = (rstSpecieG@extent@ymax - rstSpecieG@extent@ymin) / rstSpecieG@nrows
  yvals = seq(rstSpecieG@extent@ymax - stepsize / 2, rstSpecieG@extent@ymin, -stepsize)
  seqSpecies <- seq(1:384)
  ## 
  df_latitudeMean <- data.frame(seqSpecies, sumPixelUsedLat, medianSpeciesLat, meanSpeciesLat, varSpeciesLat, sdSpeciesLat)
  df_latitudeMean$seSpeciesLat <- df_latitudeMean$sdSpeciesLat/sqrt(df_latitudeMean$sumPixelUsedLat)
  df_latitudeMean[[paste0("min",fun, "SpeciesLat")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat")]] - df_latitudeMean$sdSpeciesLat
  df_latitudeMean[[paste0("max",fun, "SpeciesLat")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat")]] + df_latitudeMean$sdSpeciesLat
  maxV <-df_latitudeMean[[paste0("max",fun, "SpeciesLat")]]
  sumY <-nrow(df_latitudeMean)
  maxV[is.na(maxV)] <- 0
  maxVComp <- max(maxV)
  print(paste0(maxVComp, " - ", sumY))
  if(maxVComp > 200){
    ratioP <- 12500}
  else{
    if(maxVComp > 101  & maxVComp <= 200){
      ratioP <- 25000}
    else{
      if(maxVComp > 50  & maxVComp <= 100){
        ratioP <- 50000}
      else{
        if(maxVComp > 25  & maxVComp <= 50)
          ratioP <- 200000
        else
          ratioP <- 400000
      }
    }
  }
  
  print(ratioP)
  title2 <- gsub("_", " ", title)
  ggplot(df_latitudeMean) +
    geom_line(aes(x=(!!sym(paste0(fun, "SpeciesLat"))), y=yvals, color = "Mean"), orientation = "y", show.legend = F) +
    geom_ribbon(aes(x=(!!sym(paste0(fun,"SpeciesLat"))), y=yvals, 
                    xmin=(!!sym(paste0("min",fun,"SpeciesLat"))),
                    xmax=(!!sym(paste0("max",fun,"SpeciesLat")))),
                orientation = "y", fill="blue", alpha=0.2) +
  
    scale_y_continuous(n.breaks = 5, labels = c("90°S","45°S","0","45°N","90°N"))+
    labs(x=paste0(stringr::str_to_title(fun), "species density - ", title2),
         y="Latitude")+
    theme(legend.title =element_blank(),
          legend.position = "top",
          legend.text = element_text(size=20, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.title.x = element_text(size=20, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.title.y = element_text(size=20, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.text.x = element_text(size=20, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.text.y = element_text(size=20, 
                                     family="American Typewriter",
                                     hjust=0.5),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color="gray", size = 1),
          axis.line.y = element_line(color="gray", size = 1))+ # get rid of minor grid
    #scale_color_manual(values = colors) + 
    coord_fixed(ratio = 1/ratioP)
  ggsave(
    paste0(folderResults,"/",fun,"_species_density_",title,".png"),
    #ggplot_alternative(),
    width = 8,
    height = 10,
    dpi = 300
  )
}
myGraphLatitudeGround <- function(rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                            folderResults){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(scales)
  library(matrixStats)
  #Projecting raster
  #Projecting raster to equalarea (Goodie homolisine)
  rstMaskG <-projectRaster(rstMask, crs = crs_to, method = 'ngb')
  # Get a raster only values > 0
  rstMaskG[!is.na(rstMaskG)] <- 1
  sumPixelUsedLat <- rowSums(as.matrix(rstMaskG), na.rm = T)
  
  ##Geting y-axis values to plot
  stepsize = (rstMaskG@extent@ymax - rstMaskG@extent@ymin) / rstMaskG@nrows
  yvals = seq(rstMaskG@extent@ymax - stepsize / 2, rstMaskG@extent@ymin, -stepsize)
  seqSpecies <- seq(1:rstMaskG@nrows)
  ## 
  df_latitudeMean <- data.frame(seqSpecies, sumPixelUsedLat, yvals)
  df_latitudeMean$per <- df_latitudeMean$sumPixelUsedLat/rstMaskG@ncols
  
  ggplot(df_latitudeMean) +
    geom_area(aes(x=per*100, y=yvals, color = "Mean"), fill="lightgrey", orientation = "y", show.legend = F) +
    scale_y_continuous(n.breaks = 5, labels = c("90°S","45°S","0","45°N","90°N"))+
    labs(x="Fraction of Ground (%)",
         y="Latitude")+
    theme(legend.title =element_blank(),
          legend.position = "top",
          legend.text = element_text(size=20, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.title.x = element_text(size=20, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.title.y = element_text(size=20, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.text.x = element_text(size=20, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.text.y = element_text(size=20, 
                                     family="American Typewriter",
                                     hjust=0.5),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color="gray", size = 1),
          axis.line.y = element_line(color="gray", size = 1))+ # get rid of minor grid
    #scale_color_manual(values = colors) + 
    coord_fixed(ratio = 1/150000)
  ggsave(
    paste0(folderResults,"/fractionGround.png"),
    #ggplot_alternative(),
    width = 8,
    height = 10,
    dpi = 300
  )
}
myGraphLongitude <- function(rstSpecie, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                            title = "",formula = "", folderResults, fun = "mean"){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(scales)
  library(matrixStats)
  #Projecting raster
  #Projecting raster to equal area (Goodie homolisine)
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
  sumSpeciesLat <- colSums((as.matrix(rstSpecieG2)), na.rm = T)
  sumPixelUsedLat <- colSums((as.matrix(rstSpecieG3)), na.rm = T)
  meanSpeciesLat <- colMeans((as.matrix(rstSpecieG2)), na.rm = T)
  medianSpeciesLat <- colMedians((as.matrix(rstSpecieG2)), na.rm = T)
  varSpeciesLat <- colVars((as.matrix(rstSpecieG2)), na.rm = T)
  sdSpeciesLat <- sqrt(varSpeciesLat)
  dtSpeciesLat <- rowVars((as.matrix(rstSpecieG2)), na.rm = T)
  ##Geting y-axis values to plot
  stepsize = (rstSpecieG@extent@xmax - rstSpecieG@extent@xmin) / rstSpecieG@ncols
  yvals = seq(rstSpecieG@extent@xmax - stepsize / 2, rstSpecieG@extent@xmin, -stepsize)
  seqSpecies <- seq(1:874)
  ## 
  df_latitudeMean <- data.frame(seqSpecies, sumPixelUsedLat, medianSpeciesLat, meanSpeciesLat, varSpeciesLat, sdSpeciesLat)
  df_latitudeMean$seSpeciesLat <- df_latitudeMean$sdSpeciesLat/sqrt(df_latitudeMean$sumPixelUsedLat)
  df_latitudeMean[[paste0("min",fun, "SpeciesLat")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat")]] - df_latitudeMean$sdSpeciesLat
  df_latitudeMean[[paste0("max",fun, "SpeciesLat")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat")]] + df_latitudeMean$sdSpeciesLat
  maxV <-df_latitudeMean[[paste0("max",fun, "SpeciesLat")]]
  sumY <-nrow(df_latitudeMean)
  maxV[is.na(maxV)] <- 0
  maxVComp <- max(maxV)
  print(paste0(maxVComp, " - ", sumY))
  
  title2 <- gsub("_", " ", title)
  ggplot(df_latitudeMean) +
    geom_line(aes(y=(!!sym(paste0(fun, "SpeciesLat"))), x=yvals*-1, color = "Mean"), show.legend = F) +
    geom_ribbon(aes(y=(!!sym(paste0(fun,"SpeciesLat"))), x=yvals*-1, 
                    ymin=(!!sym(paste0("min",fun,"SpeciesLat"))),
                    ymax=(!!sym(paste0("max",fun,"SpeciesLat")))),
                fill="blue", alpha=0.2) +
    
    #scale_y_continuous(n.breaks = 5, labels = c("90°S","45°S","0","45°N","90°N"))+
    labs(y=paste0(stringr::str_to_title(fun), "species density - ", title2),
         x="Longitude")+
    theme(legend.title =element_blank(),
          legend.position = "top",
          legend.text = element_text(size=20, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.title.x = element_text(size=20, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.title.y = element_text(size=20, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.text.x = element_text(size=20, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.text.y = element_text(size=20, 
                                     family="American Typewriter",
                                     hjust=0.5),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color="gray", size = 1),
          axis.line.y = element_line(color="gray", size = 1))#+ # get rid of minor grid
    #scale_color_manual(values = colors) + 
    #coord_fixed(ratio = 1/ratioP)
  ggsave(
    paste0(folderResults,"/",fun,"_species_density_",title,".png"),
    #ggplot_alternative(),
    width = 30,
    height = 10,
    dpi = 300
  )
}
myGraphLatitudeAll <- function(rstSpecie1,rstSpecie2,rstSpecie3,rstSpecie4, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                     title = "",formula = "", folderResults, fun = "mean"){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(scales)
  library(matrixStats)
  #Projecting raster
  #Projecting raster to equalarea (Goodie homolisine)
  rstSpecieG1 <-projectRaster(rstSpecie1, crs = crs_to, method = 'ngb')
  rstSpecieG2 <-projectRaster(rstSpecie2, crs = crs_to, method = 'ngb')
  rstSpecieG3 <-projectRaster(rstSpecie3, crs = crs_to, method = 'ngb')
  rstSpecieG4 <-projectRaster(rstSpecie4, crs = crs_to, method = 'ngb')
  rstMaskG <-projectRaster(rstMask, crs = crs_to)
  # Get a raster only values > 0
  rstSpecieG1_2 <- rstSpecieG1
  rstSpecieG1_2[is.na(rstMaskG)] <- 0
  rstSpecieG1_2[rstSpecieG1_2 == 0] <- NA
  rstSpecieG2_2 <- rstSpecieG2
  rstSpecieG2_2[is.na(rstMaskG)] <- 0
  rstSpecieG2_2[rstSpecieG2_2 == 0] <- NA
  rstSpecieG3_2 <- rstSpecieG3
  rstSpecieG3_2[is.na(rstMaskG)] <- 0
  rstSpecieG3_2[rstSpecieG3_2 == 0] <- NA
  rstSpecieG4_2 <- rstSpecieG4
  rstSpecieG4_2[is.na(rstMaskG)] <- 0
  rstSpecieG4_2[rstSpecieG4_2 == 0] <- NA
  ##
  rstSpecieG1_3 <- rstSpecieG1_2
  rstSpecieG1_3[rstSpecieG1_3 > 0] <- 1
  rstSpecieG2_3 <- rstSpecieG2_2
  rstSpecieG2_3[rstSpecieG2_3 > 0] <- 1
  rstSpecieG3_3 <- rstSpecieG3_2
  rstSpecieG3_3[rstSpecieG3_3 > 0] <- 1
  rstSpecieG4_3 <- rstSpecieG4_2
  rstSpecieG4_3[rstSpecieG4_3 > 0] <- 1
  ## Getting the mean species distribution by latitude specie 1
  sumSpeciesLat1 <- rowSums(as.matrix(rstSpecieG1_2), na.rm = T)
  sumPixelUsedLat1 <- rowSums(as.matrix(rstSpecieG1_3), na.rm = T)
  meanSpeciesLat1 <- rowMeans(as.matrix(rstSpecieG2), na.rm = T)
  medianSpeciesLat1 <- rowMedians(as.matrix(rstSpecieG1_2), na.rm = T)
  varSpeciesLat1 <- rowVars(as.matrix(rstSpecieG1_2), na.rm = T)
  sdSpeciesLat1 <- sqrt(varSpeciesLat1)
  dtSpeciesLat1 <- rowVars(as.matrix(rstSpecieG1_2), na.rm = T)
  ## Getting the mean species distribution by latitude specie 2
  sumSpeciesLat2 <- rowSums(as.matrix(rstSpecieG2_2), na.rm = T)
  sumPixelUsedLat2 <- rowSums(as.matrix(rstSpecieG2_3), na.rm = T)
  meanSpeciesLat2 <- rowMeans(as.matrix(rstSpecieG2_2), na.rm = T)
  medianSpeciesLat2 <- rowMedians(as.matrix(rstSpecieG2_2), na.rm = T)
  varSpeciesLat2 <- rowVars(as.matrix(rstSpecieG2_2), na.rm = T)
  sdSpeciesLat2 <- sqrt(varSpeciesLat2)
  dtSpeciesLat2 <- rowVars(as.matrix(rstSpecieG2_2), na.rm = T)
  ## Getting the mean species distribution by latitude specie 3
  sumSpeciesLat3 <- rowSums(as.matrix(rstSpecieG3_2), na.rm = T)
  sumPixelUsedLat3 <- rowSums(as.matrix(rstSpecieG3_3), na.rm = T)
  meanSpeciesLat3 <- rowMeans(as.matrix(rstSpecieG3_2), na.rm = T)
  medianSpeciesLat3 <- rowMedians(as.matrix(rstSpecieG3_2), na.rm = T)
  varSpeciesLat3 <- rowVars(as.matrix(rstSpecieG3_2), na.rm = T)
  sdSpeciesLat3 <- sqrt(varSpeciesLat3)
  dtSpeciesLat3 <- rowVars(as.matrix(rstSpecieG3_2), na.rm = T)
  ## Getting the mean species distribution by latitude
  sumSpeciesLat4 <- rowSums(as.matrix(rstSpecieG4_2), na.rm = T)
  sumPixelUsedLat4 <- rowSums(as.matrix(rstSpecieG4_3), na.rm = T)
  meanSpeciesLat4 <- rowMeans(as.matrix(rstSpecieG4_2), na.rm = T)
  medianSpeciesLat4 <- rowMedians(as.matrix(rstSpecieG4_2), na.rm = T)
  varSpeciesLat4 <- rowVars(as.matrix(rstSpecieG4_2), na.rm = T)
  sdSpeciesLat4 <- sqrt(varSpeciesLat4)
  dtSpeciesLat4 <- rowVars(as.matrix(rstSpecieG4_2), na.rm = T)
  ##Geting y-axis values to plot
  stepsize = (rstSpecieG1@extent@ymax - rstSpecieG1@extent@ymin) / rstSpecieG1@nrows
  yvals = seq(rstSpecieG1@extent@ymax - stepsize / 2, rstSpecieG1@extent@ymin, -stepsize)
  yvalsRev <- rev(yvals)
  seqSpecies <- seq(1:384)
  ## 
  df_latitudeMean <- data.frame(seqSpecies, sumPixelUsedLat1, medianSpeciesLat1, meanSpeciesLat1, varSpeciesLat1, sdSpeciesLat1,
                                sumPixelUsedLat2, medianSpeciesLat2, meanSpeciesLat2, varSpeciesLat2, sdSpeciesLat2,
                                sumPixelUsedLat3, medianSpeciesLat3, meanSpeciesLat3, varSpeciesLat3, sdSpeciesLat3,
                                sumPixelUsedLat4,medianSpeciesLat4, meanSpeciesLat4, varSpeciesLat4, sdSpeciesLat4)
  df_latitudeMean$seSpeciesLat1 <- df_latitudeMean$sdSpeciesLat1/sqrt(df_latitudeMean$sumPixelUsedLat1)
  df_latitudeMean$seSpeciesLat2 <- df_latitudeMean$sdSpeciesLat2/sqrt(df_latitudeMean$sumPixelUsedLat2)
  df_latitudeMean$seSpeciesLat3 <- df_latitudeMean$sdSpeciesLat3/sqrt(df_latitudeMean$sumPixelUsedLat3)
  df_latitudeMean$seSpeciesLat4 <- df_latitudeMean$sdSpeciesLat4/sqrt(df_latitudeMean$sumPixelUsedLat4)
  df_latitudeMean[[paste0("min",fun, "SpeciesLat1")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat1")]] - df_latitudeMean$seSpeciesLat1
  df_latitudeMean[[paste0("max",fun, "SpeciesLat1")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat1")]] + df_latitudeMean$seSpeciesLat1
  df_latitudeMean[[paste0("min",fun, "SpeciesLat2")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat2")]] - df_latitudeMean$seSpeciesLat2
  df_latitudeMean[[paste0("max",fun, "SpeciesLat2")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat2")]] + df_latitudeMean$seSpeciesLat2
  df_latitudeMean[[paste0("min",fun, "SpeciesLat3")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat3")]] - df_latitudeMean$seSpeciesLat3
  df_latitudeMean[[paste0("max",fun, "SpeciesLat3")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat3")]] + df_latitudeMean$seSpeciesLat3
  df_latitudeMean[[paste0("min",fun, "SpeciesLat4")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat4")]] - df_latitudeMean$seSpeciesLat4
  df_latitudeMean[[paste0("max",fun, "SpeciesLat4")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat4")]] + df_latitudeMean$seSpeciesLat4
  # maxV <-df_latitudeMean[[paste0("max",fun, "SpeciesLat")]]
  # sumY <-nrow(df_latitudeMean)
  # maxV[is.na(maxV)] <- 0
  # maxVComp <- max(maxV)
  # print(paste0(maxVComp, " - ", sumY))
  # if(maxVComp > 200){
  #   ratioP <- 12500}
  # else{
  #   if(maxVComp > 101  & maxVComp <= 200){
  #     ratioP <- 25000}
  #   else{
  #     if(maxVComp > 50  & maxVComp <= 100){
  #       ratioP <- 50000}
  #     else{
  #       if(maxVComp > 25  & maxVComp <= 50)
  #         ratioP <- 200000
  #       else
  #         ratioP <- 400000
  #     }
  #   }
  # }
  
  # print(ratioP)
  title2 <- gsub("_", " ", title)
  ggplot(df_latitudeMean) +
    geom_line(aes(x=(!!sym(paste0(fun, "SpeciesLat1"))), y=yvalsRev, color = names(rstSpecieG1)), orientation = "y") +
    geom_ribbon(aes(x=(!!sym(paste0(fun,"SpeciesLat1"))), y=yvalsRev, 
                    xmin=(!!sym(paste0("min",fun,"SpeciesLat1"))),
                    xmax=(!!sym(paste0("max",fun,"SpeciesLat1")))),
                fill="gray80", orientation = "y", alpha=0.2) +
    geom_line(aes(x=(!!sym(paste0(fun, "SpeciesLat2"))), y=yvalsRev, color = names(rstSpecieG2)), orientation = "y", show.legend = F) +
    geom_ribbon(aes(x=(!!sym(paste0(fun,"SpeciesLat2"))), y=yvalsRev, 
                    xmin=(!!sym(paste0("min",fun,"SpeciesLat2"))),
                    xmax=(!!sym(paste0("max",fun,"SpeciesLat2")))),
                fill="gray80", orientation = "y", alpha=0.2) +
    geom_line(aes(x=(!!sym(paste0(fun, "SpeciesLat3"))), y=yvalsRev, color = names(rstSpecieG3)), orientation = "y", show.legend = F) +
    geom_ribbon(aes(x=(!!sym(paste0(fun,"SpeciesLat3"))), y=yvalsRev, 
                    xmin=(!!sym(paste0("min",fun,"SpeciesLat3"))),
                    xmax=(!!sym(paste0("max",fun,"SpeciesLat3")))),
                fill="gray80", orientation = "y", alpha=0.2) +
    geom_line(aes(x=(!!sym(paste0(fun, "SpeciesLat4"))), y=yvalsRev, color = names(rstSpecieG4)), orientation = "y", show.legend = F) +
    geom_ribbon(aes(x=(!!sym(paste0(fun,"SpeciesLat4"))), y=yvalsRev, 
                    xmin=(!!sym(paste0("min",fun,"SpeciesLat4"))),
                    xmax=(!!sym(paste0("max",fun,"SpeciesLat4")))),
                fill="gray80", orientation = "y", alpha=0.2) +
    
    scale_y_continuous(n.breaks = 5, labels = c("90°N","45°N","0","45°S","90°S"))+
    labs(x=paste0(stringr::str_to_title(fun), " species density - ", title2),
         y="Latitude")+
    theme(legend.title =element_blank(),
          legend.position = "top",
          legend.text = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.title.x = element_text(size=15, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.title.y = element_text(size=15, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.text.x = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.text.y = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color="gray", size = 1),
          axis.line.y = element_line(color="gray", size = 1)) # get rid of minor grid
    #scale_color_manual(values = colors) + 
    #coord_fixed(ratio = 1/ratioP)
  ggsave(
    paste0(folderResults,"/",fun,"_species_density_ALl.png"),
    #ggplot_alternative(),
    # width = 20,
    # height = 10,
    dpi = 300
  )
}
myGraphLongitudeAll <- function(rstSpecie1,rstSpecie2,rstSpecie3,rstSpecie4, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                               title = "",formula = "", folderResults, fun = "mean"){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(scales)
  library(matrixStats)
  #Projecting raster
  #Projecting raster to equalarea (Goodie homolisine)
  rstSpecieG1 <-projectRaster(rstSpecie1, crs = crs_to, method = 'ngb')
  rstSpecieG2 <-projectRaster(rstSpecie2, crs = crs_to, method = 'ngb')
  rstSpecieG3 <-projectRaster(rstSpecie3, crs = crs_to, method = 'ngb')
  rstSpecieG4 <-projectRaster(rstSpecie4, crs = crs_to, method = 'ngb')
  rstMaskG <-projectRaster(rstMask, crs = crs_to)
  # Get a raster only values > 0
  rstSpecieG1_2 <- rstSpecieG1
  rstSpecieG1_2[is.na(rstMaskG)] <- 0
  rstSpecieG1_2[rstSpecieG1_2 == 0] <- NA
  rstSpecieG2_2 <- rstSpecieG2
  rstSpecieG2_2[is.na(rstMaskG)] <- 0
  rstSpecieG2_2[rstSpecieG2_2 == 0] <- NA
  rstSpecieG3_2 <- rstSpecieG3
  rstSpecieG3_2[is.na(rstMaskG)] <- 0
  rstSpecieG3_2[rstSpecieG3_2 == 0] <- NA
  rstSpecieG4_2 <- rstSpecieG4
  rstSpecieG4_2[is.na(rstMaskG)] <- 0
  rstSpecieG4_2[rstSpecieG4_2 == 0] <- NA
  ##
  rstSpecieG1_3 <- rstSpecieG1_2
  rstSpecieG1_3[rstSpecieG1_3 > 0] <- 1
  rstSpecieG2_3 <- rstSpecieG2_2
  rstSpecieG2_3[rstSpecieG2_3 > 0] <- 1
  rstSpecieG3_3 <- rstSpecieG3_2
  rstSpecieG3_3[rstSpecieG3_3 > 0] <- 1
  rstSpecieG4_3 <- rstSpecieG4_2
  rstSpecieG4_3[rstSpecieG4_3 > 0] <- 1
  ## Getting the mean species distribution by latitude specie 1
  sumSpeciesLat1 <- colSums(as.matrix(rstSpecieG1_2), na.rm = T)
  sumPixelUsedLat1 <- colSums(as.matrix(rstSpecieG1_3), na.rm = T)
  meanSpeciesLat1 <- colMeans(as.matrix(rstSpecieG2), na.rm = T)
  medianSpeciesLat1 <- colMedians(as.matrix(rstSpecieG1_2), na.rm = T)
  varSpeciesLat1 <- colVars(as.matrix(rstSpecieG1_2), na.rm = T)
  sdSpeciesLat1 <- sqrt(varSpeciesLat1)
  dtSpeciesLat1 <- colVars(as.matrix(rstSpecieG1_2), na.rm = T)
  ## Getting the mean species distribution by latitude specie 2
  sumSpeciesLat2 <- colSums(as.matrix(rstSpecieG2_2), na.rm = T)
  sumPixelUsedLat2 <- colSums(as.matrix(rstSpecieG2_3), na.rm = T)
  meanSpeciesLat2 <- colMeans(as.matrix(rstSpecieG2_2), na.rm = T)
  medianSpeciesLat2 <- colMedians(as.matrix(rstSpecieG2_2), na.rm = T)
  varSpeciesLat2 <- colVars(as.matrix(rstSpecieG2_2), na.rm = T)
  sdSpeciesLat2 <- sqrt(varSpeciesLat2)
  dtSpeciesLat2 <- colVars(as.matrix(rstSpecieG2_2), na.rm = T)
  ## Getting the mean species distribution by latitude specie 3
  sumSpeciesLat3 <- colSums(as.matrix(rstSpecieG3_2), na.rm = T)
  sumPixelUsedLat3 <- colSums(as.matrix(rstSpecieG3_3), na.rm = T)
  meanSpeciesLat3 <- colMeans(as.matrix(rstSpecieG3_2), na.rm = T)
  medianSpeciesLat3 <- colMedians(as.matrix(rstSpecieG3_2), na.rm = T)
  varSpeciesLat3 <- colVars(as.matrix(rstSpecieG3_2), na.rm = T)
  sdSpeciesLat3 <- sqrt(varSpeciesLat3)
  dtSpeciesLat3 <- colVars(as.matrix(rstSpecieG3_2), na.rm = T)
  ## Getting the mean species distribution by latitude
  sumSpeciesLat4 <- colSums(as.matrix(rstSpecieG4_2), na.rm = T)
  sumPixelUsedLat4 <- colSums(as.matrix(rstSpecieG4_3), na.rm = T)
  meanSpeciesLat4 <- colMeans(as.matrix(rstSpecieG4_2), na.rm = T)
  medianSpeciesLat4 <- colMedians(as.matrix(rstSpecieG4_2), na.rm = T)
  varSpeciesLat4 <- colVars(as.matrix(rstSpecieG4_2), na.rm = T)
  sdSpeciesLat4 <- sqrt(varSpeciesLat4)
  dtSpeciesLat4 <- colVars(as.matrix(rstSpecieG4_2), na.rm = T)
  ##Geting y-axis values to plot
  stepsize = (rstSpecieG1@extent@xmax - rstSpecieG1@extent@xmin) / rstSpecieG1@ncols
  yvals = seq(rstSpecieG1@extent@xmax - stepsize / 2, rstSpecieG1@extent@xmin, -stepsize)
  yvalsRev <- rev(yvals)
  seqSpecies <- seq(1:874)
  ## 
  df_latitudeMean <- data.frame(seqSpecies, sumPixelUsedLat1, medianSpeciesLat1, meanSpeciesLat1, varSpeciesLat1, sdSpeciesLat1,
                                sumPixelUsedLat2, medianSpeciesLat2, meanSpeciesLat2, varSpeciesLat2, sdSpeciesLat2,
                                sumPixelUsedLat3, medianSpeciesLat3, meanSpeciesLat3, varSpeciesLat3, sdSpeciesLat3,
                                sumPixelUsedLat4,medianSpeciesLat4, meanSpeciesLat4, varSpeciesLat4, sdSpeciesLat4)
  df_latitudeMean$seSpeciesLat1 <- df_latitudeMean$sdSpeciesLat1/sqrt(df_latitudeMean$sumPixelUsedLat1)
  df_latitudeMean$seSpeciesLat2 <- df_latitudeMean$sdSpeciesLat2/sqrt(df_latitudeMean$sumPixelUsedLat2)
  df_latitudeMean$seSpeciesLat3 <- df_latitudeMean$sdSpeciesLat3/sqrt(df_latitudeMean$sumPixelUsedLat3)
  df_latitudeMean$seSpeciesLat4 <- df_latitudeMean$sdSpeciesLat4/sqrt(df_latitudeMean$sumPixelUsedLat4)
  df_latitudeMean[[paste0("min",fun, "SpeciesLat1")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat1")]] - df_latitudeMean$seSpeciesLat1
  df_latitudeMean[[paste0("max",fun, "SpeciesLat1")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat1")]] + df_latitudeMean$seSpeciesLat1
  df_latitudeMean[[paste0("min",fun, "SpeciesLat2")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat2")]] - df_latitudeMean$seSpeciesLat2
  df_latitudeMean[[paste0("max",fun, "SpeciesLat2")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat2")]] + df_latitudeMean$seSpeciesLat2
  df_latitudeMean[[paste0("min",fun, "SpeciesLat3")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat3")]] - df_latitudeMean$seSpeciesLat3
  df_latitudeMean[[paste0("max",fun, "SpeciesLat3")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat3")]] + df_latitudeMean$seSpeciesLat3
  df_latitudeMean[[paste0("min",fun, "SpeciesLat4")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat4")]] - df_latitudeMean$seSpeciesLat4
  df_latitudeMean[[paste0("max",fun, "SpeciesLat4")]] <- df_latitudeMean[[paste0(fun, "SpeciesLat4")]] + df_latitudeMean$seSpeciesLat4

  title2 <- gsub("_", " ", title)
  ggplot(df_latitudeMean) +
    geom_line(aes(y=(!!sym(paste0(fun, "SpeciesLat1"))), x=yvalsRev, color = names(rstSpecieG1))) +
    geom_ribbon(aes(y=(!!sym(paste0(fun,"SpeciesLat1"))), x=yvalsRev, 
                    ymin=(!!sym(paste0("min",fun,"SpeciesLat1"))),
                    ymax=(!!sym(paste0("max",fun,"SpeciesLat1")))),
                fill="gray80", alpha=0.2) +
    geom_line(aes(y=(!!sym(paste0(fun, "SpeciesLat2"))), x=yvalsRev, color = names(rstSpecieG2)), show.legend = F) +
    geom_ribbon(aes(y=(!!sym(paste0(fun,"SpeciesLat2"))), x=yvalsRev, 
                    ymin=(!!sym(paste0("min",fun,"SpeciesLat2"))),
                    ymax=(!!sym(paste0("max",fun,"SpeciesLat2")))),
                fill="gray80", alpha=0.2) +
    geom_line(aes(y=(!!sym(paste0(fun, "SpeciesLat3"))), x=yvalsRev, color = names(rstSpecieG3)), show.legend = F) +
    geom_ribbon(aes(y=(!!sym(paste0(fun,"SpeciesLat3"))), x=yvalsRev, 
                    ymin=(!!sym(paste0("min",fun,"SpeciesLat3"))),
                    ymax=(!!sym(paste0("max",fun,"SpeciesLat3")))),
                fill="gray80",  alpha=0.2) +
    geom_line(aes(y=(!!sym(paste0(fun, "SpeciesLat4"))), x=yvalsRev, color = names(rstSpecieG4)), show.legend = F) +
    geom_ribbon(aes(y=(!!sym(paste0(fun,"SpeciesLat4"))), x=yvalsRev, 
                    ymin=(!!sym(paste0("min",fun,"SpeciesLat4"))),
                    ymax=(!!sym(paste0("max",fun,"SpeciesLat4")))),
                fill="gray80",  alpha=0.2) +
    #scale_y_continuous(n.breaks = 5, labels = c("90°N","45°N","0","45°S","90°S"))+
    labs(y=paste0(stringr::str_to_title(fun), " species density - ", title2),
         x="Longitude")+
    theme(legend.title =element_blank(),
          legend.position = "top",
          legend.text = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.title.x = element_text(size=15, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.title.y = element_text(size=15, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.text.x = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.text.y = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color="gray", size = 1),
          axis.line.y = element_line(color="gray", size = 1)) # get rid of minor grid
  #scale_color_manual(values = colors) + 
  #coord_fixed(ratio = 1/ratioP)
  ggsave(
    paste0(folderResults,"/",fun,"_species_density_ALl_Longitude.png"),
    #ggplot_alternative(),
    width = 30,
    height = 10,
    dpi = 300
  )
}

myGraphLatitudeAllRelative <- function(rstSpecie1,rstSpecie2,rstSpecie3,rstSpecie4, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                               title = "",formula = "", folderResults, fun = "mean"){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(scales)
  library(matrixStats)
  #Projecting raster
  #Projecting raster to equalarea (Goodie homolisine)
  rstSpecieG1 <-projectRaster(rstSpecie1, crs = crs_to, method = 'ngb')
  rstSpecieG2 <-projectRaster(rstSpecie2, crs = crs_to, method = 'ngb')
  rstSpecieG3 <-projectRaster(rstSpecie3, crs = crs_to, method = 'ngb')
  rstSpecieG4 <-projectRaster(rstSpecie4, crs = crs_to, method = 'ngb')
  rstMaskG <-projectRaster(rstMask, crs = crs_to)
  # Get a raster only values > 0
  rstSpecieG1_2 <- rstSpecieG1
  rstSpecieG1_2[is.na(rstMaskG)] <- 0
  rstSpecieG1_2[rstSpecieG1_2 == 0] <- NA
  rstSpecieG2_2 <- rstSpecieG2
  rstSpecieG2_2[is.na(rstMaskG)] <- 0
  rstSpecieG2_2[rstSpecieG2_2 == 0] <- NA
  rstSpecieG3_2 <- rstSpecieG3
  rstSpecieG3_2[is.na(rstMaskG)] <- 0
  rstSpecieG3_2[rstSpecieG3_2 == 0] <- NA
  rstSpecieG4_2 <- rstSpecieG4
  rstSpecieG4_2[is.na(rstMaskG)] <- 0
  rstSpecieG4_2[rstSpecieG4_2 == 0] <- NA
  ##
  rstSpecieG1_3 <- rstSpecieG1_2
  rstSpecieG1_3[rstSpecieG1_3 > 0] <- 1
  rstSpecieG2_3 <- rstSpecieG2_2
  rstSpecieG2_3[rstSpecieG2_3 > 0] <- 1
  rstSpecieG3_3 <- rstSpecieG3_2
  rstSpecieG3_3[rstSpecieG3_3 > 0] <- 1
  rstSpecieG4_3 <- rstSpecieG4_2
  rstSpecieG4_3[rstSpecieG4_3 > 0] <- 1
  ## Getting the mean species distribution by latitude specie 1
  sumSpeciesLat1 <- rowSums(as.matrix(rstSpecieG1_2), na.rm = T)
  sumPixelUsedLat1 <- rowSums(as.matrix(rstSpecieG1_3), na.rm = T)
  meanSpeciesLat1 <- rowMeans(as.matrix(rstSpecieG2), na.rm = T)
  medianSpeciesLat1 <- rowMedians(as.matrix(rstSpecieG1_2), na.rm = T)
  varSpeciesLat1 <- rowVars(as.matrix(rstSpecieG1_2), na.rm = T)
  sdSpeciesLat1 <- sqrt(varSpeciesLat1)
  dtSpeciesLat1 <- rowVars(as.matrix(rstSpecieG1_2), na.rm = T)
  ## Getting the mean species distribution by latitude specie 2
  sumSpeciesLat2 <- rowSums(as.matrix(rstSpecieG2_2), na.rm = T)
  sumPixelUsedLat2 <- rowSums(as.matrix(rstSpecieG2_3), na.rm = T)
  meanSpeciesLat2 <- rowMeans(as.matrix(rstSpecieG2_2), na.rm = T)
  medianSpeciesLat2 <- rowMedians(as.matrix(rstSpecieG2_2), na.rm = T)
  varSpeciesLat2 <- rowVars(as.matrix(rstSpecieG2_2), na.rm = T)
  sdSpeciesLat2 <- sqrt(varSpeciesLat2)
  dtSpeciesLat2 <- rowVars(as.matrix(rstSpecieG2_2), na.rm = T)
  ## Getting the mean species distribution by latitude specie 3
  sumSpeciesLat3 <- rowSums(as.matrix(rstSpecieG3_2), na.rm = T)
  sumPixelUsedLat3 <- rowSums(as.matrix(rstSpecieG3_3), na.rm = T)
  meanSpeciesLat3 <- rowMeans(as.matrix(rstSpecieG3_2), na.rm = T)
  medianSpeciesLat3 <- rowMedians(as.matrix(rstSpecieG3_2), na.rm = T)
  varSpeciesLat3 <- rowVars(as.matrix(rstSpecieG3_2), na.rm = T)
  sdSpeciesLat3 <- sqrt(varSpeciesLat3)
  dtSpeciesLat3 <- rowVars(as.matrix(rstSpecieG3_2), na.rm = T)
  ## Getting the mean species distribution by latitude
  sumSpeciesLat4 <- rowSums(as.matrix(rstSpecieG4_2), na.rm = T)
  sumPixelUsedLat4 <- rowSums(as.matrix(rstSpecieG4_3), na.rm = T)
  meanSpeciesLat4 <- rowMeans(as.matrix(rstSpecieG4_2), na.rm = T)
  medianSpeciesLat4 <- rowMedians(as.matrix(rstSpecieG4_2), na.rm = T)
  varSpeciesLat4 <- rowVars(as.matrix(rstSpecieG4_2), na.rm = T)
  sdSpeciesLat4 <- sqrt(varSpeciesLat4)
  dtSpeciesLat4 <- rowVars(as.matrix(rstSpecieG4_2), na.rm = T)
  ##Geting y-axis values to plot
  stepsize = (rstSpecieG1@extent@ymax - rstSpecieG1@extent@ymin) / rstSpecieG1@nrows
  yvals = seq(rstSpecieG1@extent@ymax - stepsize / 2, rstSpecieG1@extent@ymin, -stepsize)
  yvalsRev <- rev(yvals)
  seqSpecies <- seq(1:384)
  ## 
  df_latitudeMean <- data.frame(seqSpecies, sumPixelUsedLat1, medianSpeciesLat1, meanSpeciesLat1, varSpeciesLat1, sdSpeciesLat1,
                                sumPixelUsedLat2, medianSpeciesLat2, meanSpeciesLat2, varSpeciesLat2, sdSpeciesLat2,
                                sumPixelUsedLat3, medianSpeciesLat3, meanSpeciesLat3, varSpeciesLat3, sdSpeciesLat3,
                                sumPixelUsedLat4,medianSpeciesLat4, meanSpeciesLat4, varSpeciesLat4, sdSpeciesLat4)
  df_latitudeMean[[paste0(fun,"SpeciesLat1Rel")]] = df_latitudeMean[[paste0(fun,"SpeciesLat1")]]/max(df_latitudeMean[[paste0(fun,"SpeciesLat1")]], na.rm = T)
  df_latitudeMean[[paste0(fun,"SpeciesLat2Rel")]] = df_latitudeMean[[paste0(fun,"SpeciesLat2")]]/max(df_latitudeMean[[paste0(fun,"SpeciesLat2")]], na.rm = T)
  df_latitudeMean[[paste0(fun,"SpeciesLat3Rel")]] = df_latitudeMean[[paste0(fun,"SpeciesLat3")]]/max(df_latitudeMean[[paste0(fun,"SpeciesLat3")]], na.rm = T)
  df_latitudeMean[[paste0(fun,"SpeciesLat4Rel")]] = df_latitudeMean[[paste0(fun,"SpeciesLat4")]]/max(df_latitudeMean[[paste0(fun,"SpeciesLat4")]], na.rm = T)
  
  title2 <- gsub("_", " ", title)
  ggplot(df_latitudeMean) +
    geom_line(aes(x=(!!sym(paste0(fun, "SpeciesLat1Rel"))), y=yvalsRev, color = names(rstSpecieG1)), orientation = "y") +
    geom_line(aes(x=(!!sym(paste0(fun, "SpeciesLat2Rel"))), y=yvalsRev, color = names(rstSpecieG2)), orientation = "y", show.legend = F) +
    geom_line(aes(x=(!!sym(paste0(fun, "SpeciesLat3Rel"))), y=yvalsRev, color = names(rstSpecieG3)), orientation = "y", show.legend = F) +
    geom_line(aes(x=(!!sym(paste0(fun, "SpeciesLat4Rel"))), y=yvalsRev, color = names(rstSpecieG4)), orientation = "y", show.legend = F) +
    
    scale_y_continuous(n.breaks = 5, labels = c("90°N","45°N","0","45°S","90°S"))+
    labs(x=paste0(stringr::str_to_title(fun), " relative species density - ", title2),
         y="Latitude")+
    theme(legend.title =element_blank(),
          legend.position = "top",
          legend.text = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.title.x = element_text(size=15, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.title.y = element_text(size=15, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.text.x = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.text.y = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color="gray", size = 1),
          axis.line.y = element_line(color="gray", size = 1)) # get rid of minor grid
  #scale_color_manual(values = colors) + 
  #coord_fixed(ratio = 1/ratioP)
  ggsave(
    paste0(folderResults,"/",fun,"_species_density_ALl_Relative.png"),
    #ggplot_alternative(),
    # width = 20,
    # height = 10,
    dpi = 300
  )
}

myGraphLongitudeAllRelative <- function(rstSpecie1,rstSpecie2,rstSpecie3,rstSpecie4, rstMask, crs_to = "+proj=longlat +datum=WGS84 +no_defs" , 
                                       title = "",formula = "", folderResults, fun = "mean"){
  library(raster)
  library(ggplot2)
  library(ggspatial)
  library(dplyr)
  library(scales)
  library(matrixStats)
  #Projecting raster
  #Projecting raster to equalarea (Goodie homolisine)
  rstSpecieG1 <-projectRaster(rstSpecie1, crs = crs_to, method = 'ngb')
  rstSpecieG2 <-projectRaster(rstSpecie2, crs = crs_to, method = 'ngb')
  rstSpecieG3 <-projectRaster(rstSpecie3, crs = crs_to, method = 'ngb')
  rstSpecieG4 <-projectRaster(rstSpecie4, crs = crs_to, method = 'ngb')
  rstMaskG <-projectRaster(rstMask, crs = crs_to)
  # Get a raster only values > 0
  rstSpecieG1_2 <- rstSpecieG1
  rstSpecieG1_2[is.na(rstMaskG)] <- 0
  rstSpecieG1_2[rstSpecieG1_2 == 0] <- NA
  rstSpecieG2_2 <- rstSpecieG2
  rstSpecieG2_2[is.na(rstMaskG)] <- 0
  rstSpecieG2_2[rstSpecieG2_2 == 0] <- NA
  rstSpecieG3_2 <- rstSpecieG3
  rstSpecieG3_2[is.na(rstMaskG)] <- 0
  rstSpecieG3_2[rstSpecieG3_2 == 0] <- NA
  rstSpecieG4_2 <- rstSpecieG4
  rstSpecieG4_2[is.na(rstMaskG)] <- 0
  rstSpecieG4_2[rstSpecieG4_2 == 0] <- NA
  ##
  rstSpecieG1_3 <- rstSpecieG1_2
  rstSpecieG1_3[rstSpecieG1_3 > 0] <- 1
  rstSpecieG2_3 <- rstSpecieG2_2
  rstSpecieG2_3[rstSpecieG2_3 > 0] <- 1
  rstSpecieG3_3 <- rstSpecieG3_2
  rstSpecieG3_3[rstSpecieG3_3 > 0] <- 1
  rstSpecieG4_3 <- rstSpecieG4_2
  rstSpecieG4_3[rstSpecieG4_3 > 0] <- 1
  ## Getting the mean species distribution by latitude specie 1
  sumSpeciesLat1 <- colSums(as.matrix(rstSpecieG1_2), na.rm = T)
  sumPixelUsedLat1 <- colSums(as.matrix(rstSpecieG1_3), na.rm = T)
  meanSpeciesLat1 <- colMeans(as.matrix(rstSpecieG2), na.rm = T)
  medianSpeciesLat1 <- colMedians(as.matrix(rstSpecieG1_2), na.rm = T)
  varSpeciesLat1 <- colVars(as.matrix(rstSpecieG1_2), na.rm = T)
  sdSpeciesLat1 <- sqrt(varSpeciesLat1)
  dtSpeciesLat1 <- colVars(as.matrix(rstSpecieG1_2), na.rm = T)
  ## Getting the mean species distribution by latitude specie 2
  sumSpeciesLat2 <- colSums(as.matrix(rstSpecieG2_2), na.rm = T)
  sumPixelUsedLat2 <- colSums(as.matrix(rstSpecieG2_3), na.rm = T)
  meanSpeciesLat2 <- colMeans(as.matrix(rstSpecieG2_2), na.rm = T)
  medianSpeciesLat2 <- colMedians(as.matrix(rstSpecieG2_2), na.rm = T)
  varSpeciesLat2 <- colVars(as.matrix(rstSpecieG2_2), na.rm = T)
  sdSpeciesLat2 <- sqrt(varSpeciesLat2)
  dtSpeciesLat2 <- colVars(as.matrix(rstSpecieG2_2), na.rm = T)
  ## Getting the mean species distribution by latitude specie 3
  sumSpeciesLat3 <- colSums(as.matrix(rstSpecieG3_2), na.rm = T)
  sumPixelUsedLat3 <- colSums(as.matrix(rstSpecieG3_3), na.rm = T)
  meanSpeciesLat3 <- colMeans(as.matrix(rstSpecieG3_2), na.rm = T)
  medianSpeciesLat3 <- colMedians(as.matrix(rstSpecieG3_2), na.rm = T)
  varSpeciesLat3 <- colVars(as.matrix(rstSpecieG3_2), na.rm = T)
  sdSpeciesLat3 <- sqrt(varSpeciesLat3)
  dtSpeciesLat3 <- colVars(as.matrix(rstSpecieG3_2), na.rm = T)
  ## Getting the mean species distribution by latitude
  sumSpeciesLat4 <- colSums(as.matrix(rstSpecieG4_2), na.rm = T)
  sumPixelUsedLat4 <- colSums(as.matrix(rstSpecieG4_3), na.rm = T)
  meanSpeciesLat4 <- colMeans(as.matrix(rstSpecieG4_2), na.rm = T)
  medianSpeciesLat4 <- colMedians(as.matrix(rstSpecieG4_2), na.rm = T)
  varSpeciesLat4 <- colVars(as.matrix(rstSpecieG4_2), na.rm = T)
  sdSpeciesLat4 <- sqrt(varSpeciesLat4)
  dtSpeciesLat4 <- colVars(as.matrix(rstSpecieG4_2), na.rm = T)
  ##Geting y-axis values to plot
  stepsize = (rstSpecieG1@extent@xmax - rstSpecieG1@extent@xmin) / rstSpecieG1@ncols
  yvals = seq(rstSpecieG1@extent@xmax - stepsize / 2, rstSpecieG1@extent@xmin, -stepsize)
  yvalsRev <- rev(yvals)
  seqSpecies <- seq(1:874)
  ## 
  df_latitudeMean <- data.frame(seqSpecies, sumPixelUsedLat1, medianSpeciesLat1, meanSpeciesLat1, varSpeciesLat1, sdSpeciesLat1,
                                sumPixelUsedLat2, medianSpeciesLat2, meanSpeciesLat2, varSpeciesLat2, sdSpeciesLat2,
                                sumPixelUsedLat3, medianSpeciesLat3, meanSpeciesLat3, varSpeciesLat3, sdSpeciesLat3,
                                sumPixelUsedLat4,medianSpeciesLat4, meanSpeciesLat4, varSpeciesLat4, sdSpeciesLat4)
  df_latitudeMean[[paste0(fun,"SpeciesLat1Rel")]] = df_latitudeMean[[paste0(fun,"SpeciesLat1")]]/max(df_latitudeMean[[paste0(fun,"SpeciesLat1")]], na.rm = T)
  df_latitudeMean[[paste0(fun,"SpeciesLat2Rel")]] = df_latitudeMean[[paste0(fun,"SpeciesLat2")]]/max(df_latitudeMean[[paste0(fun,"SpeciesLat2")]], na.rm = T)
  df_latitudeMean[[paste0(fun,"SpeciesLat3Rel")]] = df_latitudeMean[[paste0(fun,"SpeciesLat3")]]/max(df_latitudeMean[[paste0(fun,"SpeciesLat3")]], na.rm = T)
  df_latitudeMean[[paste0(fun,"SpeciesLat4Rel")]] = df_latitudeMean[[paste0(fun,"SpeciesLat4")]]/max(df_latitudeMean[[paste0(fun,"SpeciesLat4")]], na.rm = T)
  
  title2 <- gsub("_", " ", title)
  ggplot(df_latitudeMean) +
    geom_line(aes(y=(!!sym(paste0(fun, "SpeciesLat1Rel"))), x=yvalsRev, color = names(rstSpecieG1))) +
    geom_line(aes(y=(!!sym(paste0(fun, "SpeciesLat2Rel"))), x=yvalsRev, color = names(rstSpecieG2)), show.legend = F) +
    geom_line(aes(y=(!!sym(paste0(fun, "SpeciesLat3Rel"))), x=yvalsRev, color = names(rstSpecieG3)), show.legend = F) +
    geom_line(aes(y=(!!sym(paste0(fun, "SpeciesLat4Rel"))), x=yvalsRev, color = names(rstSpecieG4)), show.legend = F) +
    
    #scale_y_continuous(n.breaks = 5, labels = c("90°N","45°N","0","45°S","90°S"))+
    labs(y=paste0(stringr::str_to_title(fun), " relative species density - ", title2),
         x="Longitude")+
    theme(legend.title =element_blank(),
          legend.position = "top",
          legend.text = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.title.x = element_text(size=15, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.title.y = element_text(size=15, 
                                      family="American Typewriter",
                                      hjust=0.5),
          axis.text.x = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          axis.text.y = element_text(size=15, 
                                     family="American Typewriter",
                                     hjust=0.5),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color="gray", size = 1),
          axis.line.y = element_line(color="gray", size = 1)) # get rid of minor grid
  #scale_color_manual(values = colors) + 
  #coord_fixed(ratio = 1/ratioP)
  ggsave(
    paste0(folderResults,"/",fun,"_species_density_ALl_Relative_Longitude.png"),
    #ggplot_alternative(),
    width = 30,
    height = 10,
    dpi = 300
  )
}

myGraph2 <- function(df_Species, title, groupSpecie = "orderName", folderResults, hasAnnotate = 0){
  nTotal <- nrow(df_Species)
  colors <- c("LC" = "#38c457", "VU" = "#fbe946", "DD" = "#dfd9d3", "EX" = "#37292f",
              "CR" = "#d8001d", "EN" = "#ff683f", "NT" = "#bee447","EW" = "#471c36")
  df2 <- df_Species %>% 
    dplyr::mutate(rlCodes = factor(rlCodes, 
                                   levels = c("EX","EW", "LC", "NT","DD", "VU", "EN", "CR"))) %>%
    group_by((!!sym(groupSpecie)), rlCodes) %>%
    summarise(n=n())%>%
    mutate(percent = (n / sum(n)), cumsum = cumsum(percent), 
           label=ifelse(rlCodes=="DD",  paste0(sum(n),"", " (",round((sum(n)/nTotal)*100, 2)," %)"), ""))
  df3 <- df2 %>% group_by((!!sym(groupSpecie))) %>%
    mutate(threatN = ifelse(rlCodes %in% c("CR", "EN", "VU"), n, 0),
              excludeN = ifelse(rlCodes %in% c("EX","EW","DD"), n, 0)) %>%
    summarize(total = sum(n),
           threat = sum(threatN),
           exclude =sum(excludeN),
           perBest = round(threat/(total - exclude),4),
           rlCodes = "DD")
  df4 <- df2 %>% left_join(df3)
  df4 <- df4 %>% 
    dplyr::mutate(rlCodes = factor(rlCodes, 
                                   levels = c("EX", "EW", "LC", "NT", "DD", "VU", "EN", "CR")),
                  label2 = ifelse(rlCodes=="DD",  paste0(format(total, big.mark = ","),"", " (",round(perBest*100, 1)," %)"), ""))
  subClasses <- length(unique(df_Species[[groupSpecie]]))
  if(subClasses < 6){
    rastiP <- 5
  }else if (subClasses <12){
    rastiP <- 10
  }else{
    rastiP <- 20
  }
  if(subClasses > 30)
    angle = 90
  else
    angle = 45
  title2 <- gsub("_", " ", title)

  p <- ggplot(df4,aes(x=(!!sym(groupSpecie)), y=percent, fill=rlCodes)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colors)+
    geom_bar(position = 'fill', stat="identity", color = "Black") +
    geom_crossbar(aes(y = perBest, ymin=perBest,ymax=perBest), 
                  color="blue", show.legend = F, width = 0.4)+ 
    labs(fill="Category", y = "Percentage")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=18, 
                                      family="American Typewriter"),
          plot.title = element_text(size=18, 
                                      face="bold", 
                                      family="American Typewriter",
                                      hjust=0.5,
                                      lineheight=1.2),
          legend.title = element_text(size=18, 
                                    face="bold", 
                                    family="American Typewriter",
                                    hjust=0.5,
                                    lineheight=1.2),
          axis.text.x=element_text(size=18, 
                                   family="American Typewriter",
                                   angle=45,
                                   hjust=0.5,
                                   vjust = 0.5),
          axis.text.y = element_text(size=18, 
                                   family="American Typewriter",
                                   hjust=1),
          legend.text = element_text(size=14, 
                                     family="American Typewriter",
                                     hjust=1))+
    
    coord_fixed(ratio=rastiP)
  if(hasAnnotate == 1){
    p <- p + geom_text(aes(y=1, label=label2), size = 5, vjust=-1)
  }
  p
  ggsave(
    paste0(folderResults,"/barProportion_",title,".png"),
    #ggplot_alternative(),
    width = 16,
    height = 10,
    dpi = 300
  )
}
myGraph2_1 <- function(df_Species, title, groupSpecie = "orderName", folderResults, hasAnnotate = 0){
  nTotal <- nrow(df_Species)
  colors <- c("LC" = "#38c457", "VU" = "#fbe946", "DD" = "#dfd9d3", "EX" = "#37292f",
              "CR" = "#d8001d", "EN" = "#ff683f", "NT" = "#bee447","EW" = "#471c36")
  df2 <- df_Species %>% 
    dplyr::mutate(rlCodes = factor(rlCodes, 
                                   levels = c("LC", "NT","DD", "VU", "EN", "CR", "EW"))) %>%
    group_by((!!sym(groupSpecie)), rlCodes) %>%
    summarise(n=n())%>%
    mutate(percent = (n / sum(n)), cumsum = cumsum(percent), 
           label=ifelse(rlCodes=="DD",  paste0(sum(n),"", " (",round((sum(n)/nTotal)*100, 2)," %)"), ""))
  df3 <- df2 %>% group_by((!!sym(groupSpecie))) %>%
    mutate(threatN = ifelse(rlCodes %in% c("CR", "EN", "VU"), n, 0),
           excludeN = ifelse(rlCodes %in% c("EW","DD"), n, 0)) %>%
    summarize(total = sum(n),
              threat = sum(threatN),
              exclude =sum(excludeN),
              perBest = round(threat/(total - exclude),4),
              rlCodes = "DD")
  df4 <- df2 %>% left_join(df3)
  df4 <- df4 %>% 
    dplyr::mutate(rlCodes = factor(rlCodes, 
                                   levels = c("LC", "NT", "DD", "VU", "EN", "CR", "EW")),
                  label2 = ifelse(rlCodes=="DD",  paste0(format(total, big.mark = ","),"", " (",round(perBest*100, 1)," %)"), ""))
  subClasses <- length(unique(df_Species[[groupSpecie]]))
  if(subClasses < 6){
    rastiP <- 5
  }else if (subClasses <12){
    rastiP <- 10
  }else{
    rastiP <- 20
  }
  if(subClasses > 30)
    angle = 90
  else
    angle = 45
  title2 <- gsub("_", " ", title)
  
  p <- ggplot(df4,aes(x=(!!sym(groupSpecie)), y=percent, fill=rlCodes)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colors)+
    geom_bar(position = 'fill', stat="identity", color = "Black") +
    geom_crossbar(aes(y = perBest, ymin=perBest,ymax=perBest), 
                  color="blue", show.legend = F, width = 0.4)+ 
    labs(fill="Category", y = "Percentage")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=18, 
                                      family="American Typewriter"),
          plot.title = element_text(size=18, 
                                    face="bold", 
                                    family="American Typewriter",
                                    hjust=0.5,
                                    lineheight=1.2),
          legend.title = element_text(size=18, 
                                      face="bold", 
                                      family="American Typewriter",
                                      hjust=0.5,
                                      lineheight=1.2),
          axis.text.x=element_text(size=18, 
                                   family="American Typewriter",
                                   angle=45,
                                   hjust=0.5,
                                   vjust = 0.5),
          axis.text.y = element_text(size=18, 
                                     family="American Typewriter",
                                     hjust=1),
          legend.text = element_text(size=14, 
                                     family="American Typewriter",
                                     hjust=1))+
    
    coord_fixed(ratio=rastiP)
  if(hasAnnotate == 1){
    p <- p + geom_text(aes(y=1, label=label2), size = 5, vjust=-1)
  }
  p
  ggsave(
    paste0(folderResults,"/barProportion_",title,"v2.png"),
    #ggplot_alternative(),
    width = 16,
    height = 10,
    dpi = 300
  )
}

myGraph2_2 <- function(df_Species, title, groupSpecie = "orderName", folderResults, hasAnnotate = 0, dpi = 400){
  nTotal <- nrow(df_Species)
  colors <- c("LC" = "#38c457", "VU" = "#fbe946", "DD" = "#dfd9d3", "EX" = "#37292f",
              "CR" = "#d8001d", "EN" = "#ff683f", "NT" = "#bee447","EW" = "#471c36")
  df2 <- df_Species %>% 
    dplyr::mutate(rlCodes = factor(rlCodes, 
                                   levels = c("EW", "EX", "LC", "NT","DD", "VU", "EN", "CR"))) %>%
    group_by((!!sym(groupSpecie)), rlCodes) %>%
    summarise(n=n())%>%
    mutate(percent = (n / sum(n)), cumsum = cumsum(percent), 
           label=ifelse(rlCodes=="DD",  paste0(sum(n),"", " (",round((sum(n)/nTotal)*100, 2)," %)"), ""))
  df3 <- df2 %>% group_by((!!sym(groupSpecie))) %>%
    mutate(threatN = ifelse(rlCodes %in% c("CR", "EN", "VU"), n, 0),
           excludeN = ifelse(rlCodes %in% c("EX", "EW","DD"), n, 0)) %>%
    summarize(total = sum(n),
              threat = sum(threatN),
              exclude =sum(excludeN),
              perBest = round(threat/(total - exclude),4),
              rlCodes = "DD")
  df4 <- df2 %>% left_join(df3)
  df4 <- df4 %>% 
    dplyr::mutate(rlCodes = factor(rlCodes, 
                                   levels = c("EW", "EX", "LC", "NT","DD", "VU", "EN", "CR")),
                  label2 = ifelse(rlCodes=="DD",  paste0(format(total, big.mark = ","),"", " (",round(perBest*100, 1)," %)"), ""))
  subClasses <- length(unique(df_Species[[groupSpecie]]))
  if(subClasses < 6){
    rastiP <- 5
  }else if (subClasses <12){
    rastiP <- 10
  }else{
    rastiP <- 20
  }
  if(subClasses > 30)
    angle = 90
  else
    angle = 45
  title2 <- gsub("_", " ", title)
  
  p <- ggplot(df4,aes(x=(!!sym(groupSpecie)), y=percent, fill=rlCodes)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colors)+
    geom_bar(position = 'fill', stat="identity", color = "Black") +
    geom_crossbar(aes(y = perBest, ymin=perBest,ymax=perBest), 
                  color="blue", show.legend = F, width = 0.4)+ 
    labs(fill="Category", y = "Percentage")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=18, 
                                      family="American Typewriter"),
          plot.title = element_text(size=18, 
                                    face="bold", 
                                    family="American Typewriter",
                                    hjust=0.5,
                                    lineheight=1.2),
          legend.title = element_text(size=18, 
                                      face="bold", 
                                      family="American Typewriter",
                                      hjust=0.5,
                                      lineheight=1.2),
          axis.text.x=element_text(size=18, 
                                   family="American Typewriter",
                                   angle=45,
                                   hjust=0.5,
                                   vjust = 0.5),
          axis.text.y = element_text(size=18, 
                                     family="American Typewriter",
                                     hjust=1),
          legend.text = element_text(size=14, 
                                     family="American Typewriter",
                                     hjust=1))+
    
    coord_fixed(ratio=rastiP)
  if(hasAnnotate == 1){
    p <- p + geom_text(aes(y=1, label=label2), size = 5, vjust=-1)
  }
  p
  ggsave(
    paste0(folderResults,"/barProportion_",title,"v2_",dpi,".png"),
    #ggplot_alternative(),
    width = 16,
    height = 10,
    dpi = dpi
  )
}
