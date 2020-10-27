library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(tidyverse)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
fileList01 <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_amphibians_062020.csv"
fileList02 <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_birds_062020.csv"
fileList03 <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_mammals_062020.csv"
fileList04 <- "/media/victor/DATA1/GRA2020/SEND_DATA/Geographic_spatial_data_terrestrial_tetrapods_062020_50/List_reptiles_062020.csv"
folderResults <- "/media/victor/DATA1/GRA2020/SEND_DATA/RESULTS/"
folderGraph <- "graphicsGeneral"
title <- "Sum_circular_plot"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Open the function plots
source("../fun/richnessMap.R")
##
listNames <- c("kingdomName", "phylumName", "className", "orderName",
               "familyName", "genusName", "speciesName",
               "scientificName", "redlistCategory", "rasterFile")
df_Species01 <- read.csv(fileList01, stringsAsFactors = F)
df_Species02 <- read.csv(fileList02, stringsAsFactors = F)
df_Species03 <- read.csv(fileList03, stringsAsFactors = F)
df_Species04 <- read.csv(fileList04, stringsAsFactors = F)
names(df_Species02) <- listNames
# preparing data
df_Species <-rbind(df_Species01, df_Species02, df_Species03, df_Species04)

df2 <- df_Species %>% group_by(className, orderName) %>%
  summarise(n=n()) %>%arrange(desc(n)) 

df2$className <- factor(df2$className, levels= c("AVES", "AMPHIBIA", "MAMMALIA", "REPTILIA"))
df2$orderName <- as.factor(df2$orderName)
df2 <- as_data_frame(df2)
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(df2$className), ncol(df2)) )
colnames(to_add) <- colnames(df2)
to_add$className <- rep(levels(df2$className), each=empty_bar)
df2<- rbind(df2, to_add)
df2 <- df2 %>% arrange(factor(className))
df2$id <- seq(1, nrow(df2))

# Get the name and the y position of each label
label_data <- df2
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- df2 %>% 
  group_by(className) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
df2$nlog = log10(df2$n)
label_data$nlog = log10(label_data$n)
# Make the plot
p <- ggplot(df2, aes(x=as.factor(id), y=n, fill=className)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=nlog, fill=className), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 4, xend = start, yend = 4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 3, xend = start, yend = 3), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 2, xend = start, yend = 2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(df2$id),4), y = c(1, 2, 3, 4), label = c("10", "100", "1000", "10000") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=nlog, fill=className), stat="identity", alpha=0.5) +
  #scale_y_log10(limits = c(-1000, 1000))+
  ylim(-3,4) +
  theme_minimal() +
  theme(
    #legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm") 
  ) +
  coord_polar() + 
  labs(fill = "Class")+
  geom_text(data=label_data, aes(x=id, y=nlog+0.1, label=orderName, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  #+
  #geom_text(data=base_data, aes(x = title, y = -1, label=className), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)

p
ggsave(
  paste0(folderResults, folderGraph,"/",title,".png"),
  #ggplot_alternative(),
  width = 12,
  height = 10,
  dpi = 300
)
