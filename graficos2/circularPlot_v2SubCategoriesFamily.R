library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(raster)
library(dplyr)
library(tidyverse)
# Open Data and set variables to change (Amphibians, Birds, Mammals, Reptiles)
fileList01 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_amphibians_082020.csv"
fileList02 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_birds_082020.csv"
fileList03 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_mammals_082020.csv"
fileList04 <- "/media/victor/DATA/GRA2020/SEND_DATA02/Geographic_spatial_data_terrestrial_tetrapods_082020_50/List_reptiles_082020.csv"
folderResults <- "/media/victor/DATA/GRA2020/SEND_DATA02/RESULTS/"
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
df_Species$redlistCategory[which(df_Species$redlistCategory == "")] <- "Least Concern" 
rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
            "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
            "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
            "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")
df_Species$rlCodes<-df_Species$redlistCategory
for(i in 1:length(rlCodes)){
  df_Species$rlCodes[which(df_Species$rlCodes == names(rlCodes[i]))] = rlCodes[i]
}

df2 <- df_Species %>% 
  filter(className == "REPTILIA" ) %>%
  dplyr::mutate(rlCodes = factor(rlCodes, 
                                 levels = c("DD", "LC", "NT", "VU", "EN", "CR", "EW", "EX"))) %>%
  group_by(orderName, familyName, rlCodes) %>%
  summarise(n=n())%>%
  mutate(percent = (n / sum(n)))

df2 <- df2 %>% complete(familyName, rlCodes = unique(df2$rlCodes), fill = list(n= 0, percent = 0))
df3 <- df2 %>% 
  filter(rlCodes %in% c("VU", "EN", "CR")) %>%
  group_by(orderName, familyName) %>%
  summarise(total = sum(percent))

df4 <- df2 %>% left_join(df3)

df4 <- df4 %>% arrange(orderName, total)


df4$orderName <- factor(df4$orderName, levels= c("CROCODYLIA", "RHYNCHOCEPHALIA", "SAURIA", "SERPENTES", "TESTUDINES"))

df4 <- as_data_frame(df4)
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
nObsType <- nlevels(as.factor(df4$rlCodes))
#to_add <- data.frame( matrix(NA, empty_bar*nlevels(df2$className), ncol(df2)) )
to_add <- data.frame( matrix(NA, empty_bar*nlevels(df4$orderName)*nObsType, ncol(df4)) )
colnames(to_add) <- colnames(df4)
to_add$orderName <- rep(levels(df4$orderName),each=empty_bar*nObsType)
df4<- rbind(df4, to_add)
df4 <- df4 %>% arrange(orderName, total)
df4$id <- rep( seq(1, nrow(df4)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- df4 %>% group_by(id, familyName) %>% summarize(tot=sum(percent))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- df4 %>% 
  group_by(orderName) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
# df4$nlog = log10(df4$n)
# label_data$nlog = log10(label_data$tot)

colors <- c("LC" = "#38c457", "VU" = "#fbe946", "DD" = "#dfd9d3", "EX" = "#37292f",
            "CR" = "#d8001d", "EN" = "#ff683f", "NT" = "#bee447","EW" = "#471c36")
classes <- c("A" = "CROCODYLIA", "B" = "RHYNCHOCEPHALIA", "C" = "SAURIA", "D" = "SERPENTES", "E" = "TESTUDINES")
base_data$class <- as.character(base_data$orderName)

for(i in 1:length(classes)){
  base_data$class[which(base_data$class == classes[i])] = names(classes[i])
}

base_data
# Make the plot
p <- ggplot(df4) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=percent, fill=rlCodes), stat="identity", alpha=0.5) +
  scale_fill_manual(values = colors)+
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.75, xend = start, yend = 0.75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.50, xend = start, yend = 0.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0.25, xend = start, yend = 0.25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(df4$id),4), y = c(0.25, 0.5, 0.75, 1), label = c("25", "50", "75", "100") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  #geom_bar(aes(x=as.factor(id), y=nlog, fill=className), stat="identity", alpha=0.5) +
  #scale_y_log10(limits = c(-1000, 1000))+
  ylim(-0.55,1.25) +
  theme_minimal() +
  theme(
    #legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm") 
  ) +
  coord_polar() + 
  labs(fill = "Category")+
  geom_text(data=label_data, aes(x=id, y=tot+0.01, label=familyName, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -0.05, xend = end+1, yend = -0.05), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -0.1, label=class), hjust=c(0.5,0.5,0.5,0.5,0.5), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)+
  scale_discrete_identity(
    aesthetics = "label",
    name = "Order",
    breaks = base_data$class,
    labels = base_data$orderName,
    guide = "legend"
  )

p
ggsave(
  paste0(folderResults, folderGraph,"/circulaAllPercentageFamilyByClass.png"),
  #ggplot_alternative(),
  width = 12,
  height = 10,
  dpi = 600
)
