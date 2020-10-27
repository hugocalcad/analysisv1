# # Packages
# install.packages("raster")
# install.packages("classInt")
# install.packages("raster")
# install.packages("sp")
# install.packages("tidyverse")
# install.packages("cowplot")

#Load packages
library(classInt)
library(sp)
library(tidyverse)
library(raster)
library(classInt)
library(cowplot)

# Based on: https://gist.github.com/scbrown86/2779137a9378df7b60afd23e0c45c188

# The function that produces the colour matrix
colmat <- function(nquantiles = 3, upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft= "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,
                   saveLeg = TRUE) {
  # The colours can be changed by changing the HEX codes for:
  # upperleft, upperright, bottomleft, bottomright
  # From http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  # upperleft = "#64ACBE", upperright = "#574249", bottomleft = "#E8E8E8", bottomright = "#C85A5A",
  # upperleft = "#BE64AC", upperright = "#3B4994", bottomleft = "#E8E8E8", bottomright = "#5AC8C8",
  # upperleft = "#73AE80", upperright = "#2A5A5B", bottomleft = "#E8E8E8", bottomright = "#6C83B5", 
  # upperleft = "#9972AF", upperright = "#804D36", bottomleft = "#E8E8E8", bottomright = "#C8B35A",
  # upperleft = "#DA8DC8", upperright = "#697AA2", bottomleft = "#E8E8E8", bottomright = "#73BCA0",
  # Similar to Teuling, Stockli, Seneviratnea (2011)
  # upperleft = "#F7900A", upperright = "#993A65", bottomleft = "#44B360", bottomright = "#3A88B5",
  # Viridis style
  # upperleft = "#FEF287", upperright = "#21908D", bottomleft = "#E8F4F3", bottomright = "#9874A1",
  # Similar to Fjeldsa, Bowie, Rahbek 2012
  # upperleft = "#34C21B", upperright = "#FFFFFF", bottomleft = "#595757",  bottomright = "#A874B8",
  # Default from original source
  # upperleft = "#0096EB", upperright = "#820050", bottomleft= "#BEBEBE", bottomright = "#FFE60F",
  require(classInt)
  my.data <- seq(0, 1, .01)
  # Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
  my.class <- classInt::classIntervals(my.data,
                                       n = nquantiles,
                                       style = "quantile"
  )
  my.pal.1 <- findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- findColours(my.class, my.col)
  }
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>%
    mutate("Y" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>% 
    gather(data = ., key = X, value = HEXCode, na.rm = FALSE, -Y) %>%
    mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    dplyr::select(-c(4)) %>%
    mutate("X" = rep(seq(from = 1, to = nquantiles, by = 1), each = nquantiles),
           "Y" = rep(seq(from = 1, to = nquantiles, by = 1), times = nquantiles)) %>%
    mutate("UID" = row_number())
  # Use plotLeg if you want a preview of the legend
  if (plotLeg) {
    p <- ggplot(col.matrix.plot, aes(X, Y, fill = HEXCode)) +
      geom_raster() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(aspect.ratio = 1,
            axis.title = element_text(size = 8, colour = "black",hjust = 0.5, 
                                      vjust = 1),
            axis.title.y = element_text(angle = 90, hjust = 0.5)) +
      xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ylab(bquote(.(ylab) ~  symbol("\256")))
    print(p)
    assign(
      x = "BivLegend",
      value = p,
      pos = .GlobalEnv
    )
  }
  # Use saveLeg if you want to save a copy of the legend
  if (saveLeg) {
    ggsave(filename = "bivLegend.pdf", plot = p, device = "pdf",
           path = "./", width = 4, height = 4, units = "in",
           dpi = 300)
  }
  seqs <- seq(0, 100, (100 / nquantiles))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
}



# Function to assign colour-codes to raster data
# As before, by default assign tercile breaks
bivariate.map <- function(rasterx, rastery, colormatrix = col.matrix,
                          nquantiles = 10) {
  
  quanmean <- getValues(rasterx)
  temp <- data.frame(quanmean, quantile = rep(NA, length(quanmean)))
  brks <- with(temp, quantile(temp, na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles))))
  while (any(duplicated(brks))) {
    brks <- ifelse(duplicated(brks), 
                   brks + (0.01 * min(brks[brks > 0])),
                   brks)
  }
  r1 <- within(temp, quantile <- cut(quanmean, breaks = brks,
                                     labels = 2:length(brks),
                                     include.lowest = TRUE))
  quantr <- data.frame(r1[,2])
  
  quanvar <- getValues(rastery)
  temp <- data.frame(quanvar, quantile = rep(NA, length(quanvar)))
  brks <- with(temp, quantile(temp,na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles))))
  while (any(duplicated(brks))) {
    brks <- ifelse(duplicated(brks),
                   brks + (0.01 * min(brks[brks > 0])),
                   brks)
  }
  r2 <- within(temp, quantile <- cut(quanvar, breaks = brks,
                                     labels = 2:length(brks),
                                     include.lowest = TRUE))
  quantr2 <- data.frame(r2[, 2])
  
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  col.matrix2 <- colormatrix
  cn <- unique(colormatrix)
  
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <- 1,
           col.matrix2[i] <- which(col.matrix2[i] == cn)[1])
  }
  
  cols <- numeric(length(quantr[, 1]))
  for (i in 1:length(quantr[, 1])) {
    a <- as.numeric.factor(quantr[i, 1])
    b <- as.numeric.factor(quantr2[i, 1])
    cols[i] <- as.numeric(col.matrix2[b, a])
  }
  r <- rasterx
  r[1:length(r)] <- cols
  return(r)
}


# Retrieve some raster data from worldclim
className <- "reptiles"
classNameBS <- list("amphibians" = "AMPHIBIA","birds" = "AVES", "mammals" ="MAMMALIA", "reptiles" = "REPTILIA")
className2 <- stringr::str_to_title(className)
fileList <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_",className,"_062020.csv")
folderResults <- paste0("/media/victor/DATA1/GRA2020/SEND_DATA02/RESULTS/",className2,"/")
rstSpecie <- raster(paste0(folderResults, 'sumTotal',className2,'.tif'))
title <- paste0(className2) 
folderGraph <- "graphics07"
crs_to <-  '+proj=igh'

rst_EN <- raster(paste0(folderResults, 'sum_EN.tif'))
rst_VU <- raster(paste0(folderResults, 'sum_VU.tif'))
rst_CR <- raster(paste0(folderResults, 'sum_CR.tif'))
rst_DD <- raster(paste0(folderResults, 'sum_DD.tif'))
rstRichnesThreath <- rst_EN + rst_VU + rst_CR

rstSpecie[is.na(rstMask)] <- NA
rstRichnesThreath[is.na(rstMask)] <- NA
rstSpecie[rstSpecie == 0] <- NA
rstRichnesThreath[rstSpecie == 0] <- NA

# Define the number of breaks
nBreaks <- 6

# Create the colour matrix
col.matrix <- colmat(nquantiles = nBreaks, xlab = "Richness", ylab = "Threatened", 
                     ## non default colours
                     upperleft = "#a6611a", upperright = "#dfc27d", 
                     bottomleft = "#80cdc1", bottomright = "#018571",
                     saveLeg = FALSE, plotLeg = TRUE)

# create the bivariate raster
bivmapT1 <- bivariate.map(rasterx = rstSpecie, rastery = rstRichnesThreath,
                        colormatrix = col.matrix, nquantiles = nBreaks)

## Error in the above function: Error in cut.default(quanvar, breaks = brks, labels = 2:length(brks),  : 'breaks' are not unique

rstMask[!is.na(rstMask)] <- 1
bivmapT1P <- projectRaster(bivmapT1, crs = crs_to, method = "ngb")
rstMaskP <- projectRaster(rstMask, crs = crs_to, method = "ngb")
tif <- st_as_stars(rstMaskP)
sf=st_as_sf(tif)
sf_MaskP <- st_combine(sf)

map <- ggplot() + 
  layer_spatial(sf_MaskP, col = "gray")+
  layer_spatial(bivmapT1P, aes(fill = stat(band1)))+
  scale_fill_gradientn(colours = col.matrix, na.value = "transparent", ) +
  theme(text = element_text(size = 10, colour = "black")) +
  #borders(colour = "black", size = 0.5) +
  #coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude") +
  labs(title=paste0(title," - Richness vs Threatened"), subtitle = "",
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
map

# Overlay the legend on the map
## use draw_plot(..., x, y) to control position.
fig <- ggdraw(map) + 
  draw_plot(BivLegend +
              theme(plot.background = element_rect(fill = "white", colour = NA)),
            width = 0.15, height = 0.15, x = 0.0, y = 0.05)
dir.create(paste0(folderResults, folderGraph), showWarnings = FALSE)
ggsave(
  paste0(folderResults, folderGraph,"/twoDimesions_",title,".png"),
  #ggplot_alternative(),
  width = 10,
  height = 5,
  dpi = 400
)
