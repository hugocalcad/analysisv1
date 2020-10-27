# The function that produces the colour matrix
colmat <- function(nquantiles = 3, upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,
                   saveLeg = TRUE) {
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  require(magrittr)
  require(classInt)
  # The colours can be changed by changing the HEX codes for:
  # upperleft, upperright, bottomleft, bottomright
  # From http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  # upperleft = "#64ACBE", upperright = "#574249", bottomleft = "#E8E8E8", bottomright = "#C85A5A",
  # upperleft = "#BE64AC", upperright = "#3B4994", bottomleft = "#E8E8E8", bottomright = "#5AC8C8",
  # upperleft = "#73AE80", upperright = "#2A5A5B", bottomleft = "#E8E8E8", bottomright = "#6C83B5", 
  # upperleft = "#9972AF", upperright = "#804D36", bottomleft = "#E8E8E8", bottomright = "#C8B35A",
  # upperleft = "#DA8DC8", upperright = "#697AA2", bottomleft = "#E8E8E8", bottomright = "#73BCA0",
  # Similar to Teuling, Stockli, Seneviratnea (2011) [https://doi.org/10.1002/joc.2153]
  # upperleft = "#F7900A", upperright = "#993A65", bottomleft = "#44B360", bottomright = "#3A88B5",
  # Viridis style
  # upperleft = "#FEF287", upperright = "#21908D", bottomleft = "#E8F4F3", bottomright = "#9874A1",
  # Similar to Fjeldsa, Bowie, Rahbek 2012
  # upperleft = "#34C21B", upperright = "#FFFFFF", bottomleft = "#595757",  bottomright = "#A874B8",
  # Default from original source
  # upperleft = "#0096EB", upperright = "#820050", bottomleft= "#BEBEBE", bottomright = "#FFE60F",
  my.data <- seq(0, 1, .01)
  # Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
  my.class <- classInt::classIntervals(my.data,
                                       n = nquantiles,
                                       style = "quantile" )
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
    pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>% 
    mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(Y = rev(.$Y)) %>% 
    dplyr::select(-c(4)) %>%
    mutate("Y" = rep(seq(from = 1, to = nquantiles, by = 1), each = nquantiles),
           "X" = rep(seq(from = 1, to = nquantiles, by = 1), times = nquantiles)) %>%
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
                          nquantiles = 3, export.colour.matrix = TRUE,
                          outname = paste0("colMatrix_rasValues", names(rasterx))) {
  # export.colour.matrix will export a data.frame of rastervalues and RGB codes 
  # to the global environment outname defines the name of the data.frame
  quanmean <- getValues(rasterx)
  temp <- data.frame(quanmean, quantile = rep(NA, length(quanmean)))
  brks <- with(temp, unique(quantile(temp,
                              na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles)))
  ))
  ## Add (very) small amount of noise to all but the first break
  ## https://stackoverflow.com/a/19846365/1710632
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r1 <- within(temp, quantile <- cut(quanmean,
                                     breaks = brks,
                                     labels = 2:length(brks),
                                     include.lowest = TRUE
  ))
  quantr <- data.frame(r1[, 2])
  quanvar <- getValues(rastery)
  temp <- data.frame(quanvar, quantile = rep(NA, length(quanvar)))
  brks <- with(temp, unique(quantile(temp,
                              na.rm = TRUE,
                              probs = c(seq(0, 1, 1 / nquantiles)))
  ))
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r2 <- within(temp, quantile <- cut(quanvar,
                                     breaks = brks,
                                     labels = 2:length(brks),
                                     include.lowest = TRUE
  ))
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <- colormatrix
  cn <- unique(colormatrix)
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <- 1, col.matrix2[i] <- which(
             col.matrix2[i] == cn
           )[1]
    )
  }
  # Export the colour.matrix to data.frame() in the global env
  # Can then save with write.table() and use in ArcMap/QGIS
  # Need to save the output raster as integer data-type
  if (export.colour.matrix) {
    # create a dataframe of colours corresponding to raster values
    exportCols <- as.data.frame(cbind(
      as.vector(col.matrix2), as.vector(colormatrix),
      t(col2rgb(as.vector(colormatrix)))
    ))
    # rename columns of data.frame()
    colnames(exportCols)[1:2] <- c("rasValue", "HEX")
    # Export to the global environment
    assign(
      x = outname,
      value = exportCols,
      pos = .GlobalEnv
    )
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

#### TESTS ####
example <- TRUE
if (example) {
  library(tidyverse)
  library(raster)
  library(classInt)
  library(ggspatial)
  library(cowplot)
  
  # Retrieve some raster data from worldclim
  # and clip to SE Asia and Northern Australia
  fileList <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_reptiles_062020.csv"
  folderResults <- "/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/Reptiles/"
  rstSpecie <- raster(paste0(folderResults, 'sumTotalReptiles.tif'))
  rstMask <- raster('/media/victor/DATA1/GRA2020/SEND_DATA/mask_50.tif')
  
  rst_EN <- raster(paste0(folderResults, 'sum_EN.tif'))
  rst_VU <- raster(paste0(folderResults, 'sum_VU.tif'))
  rst_CR <- raster(paste0(folderResults, 'sum_CR.tif'))
  rst_DD <- raster(paste0(folderResults, 'sum_DD.tif'))
  rstRichnesThreath <- rst_EN + rst_VU + rst_CR
  
  rstSpecie[rstSpecie == 0] <- NA
  rstRichnesThreath[rstSpecie == 0] <- NA

  # Define the number of breaks
  nBreaks <- 10
  
  # Create the colour matrix
  col.matrix <- colmat(nquantiles = nBreaks, xlab = "Richness", ylab = "Trheat", 
                       ## non default colours
                       upperleft = "#F7900A", upperright = "#993A65", 
                       bottomleft = "#44B360", bottomright = "#3A88B5",
                       saveLeg = FALSE, plotLeg = TRUE)
  
  # create the bivariate raster
  bivmap <- bivariate.map(rasterx = rstSpecie, rastery = rstRichnesThreath,
                          export.colour.matrix = TRUE, outname = "bivMapCols",
                          colormatrix = col.matrix, nquantiles = nBreaks)
  # spplot(bivmap)
  
  # Make the map using ggplot
  map <- ggplot() + 
    layer_spatial(bivmap, aes(fill = stat(band1)))+
    scale_fill_gradientn(colours = col.matrix, na.value = "transparent") +
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
    labs(title="Birds - Richness vs threat", subtitle = "formula",
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
              width = 0.15, height = 0.15, x = 0.0, y = 0.1)
  fig
  
  # Save
  # ggsave(plot = fig,
  #        filename = "BivaraitePlot_ggsave.pdf",
  #        device = "pdf", path = "./",
  #        dpi = 320)
  
  ##
  library(ggspatial)
  
    
}
