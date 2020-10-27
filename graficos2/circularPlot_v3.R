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
saveName <- "Threat_circular_plot_bestEstimate"
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
# preparing datas
df_Species <-rbind(df_Species01, df_Species02, df_Species03, df_Species04)
rlCodes <-c("Least Concern" = "LC", "Vulnerable" = "VU", "Data Deficient" = "DD", "Extinct" = "EX",
            "Critically Endangered" = "CR", "Endangered" = "EN", "Near Threatened" = "NT","Extinct in the Wild" = "EW",
            "Lower Risk/least concern" = "LC", "Critically Endangered (Possibly Extinct)" = "CR",
            "Lower Risk/near threatened" = "NT", "Lower Risk/conservation dependent" = "LC")
df_Species$rlCodes<-df_Species$redlistCategory
for(i in 1:length(rlCodes)){
  df_Species$rlCodes[which(df_Species$rlCodes == names(rlCodes[i]))] = rlCodes[i]
}
df_Species_threat <- df_Species %>% filter(rlCodes %in% c("CR","EN","VU"))
df_Species_exclude <- df_Species %>% filter(rlCodes %in% c("EW","EX"))

df_total <- df_Species %>% group_by(className, orderName) %>%
  summarise(total=n()) %>%arrange(total) 
df_threat <- df_Species_threat %>% group_by(className, orderName) %>%
  summarise(threat=n()) %>%arrange(threat) 
df_exclude <- df_Species_exclude %>% group_by(className, orderName) %>%
  summarise(exclude=n()) %>%arrange(exclude) 

df_join <- df_total %>% 
  left_join(df_threat) %>%
  left_join(df_exclude)
df_join$threat[is.na(df_join$threat)] <- 0
df_join$exclude[is.na(df_join$exclude)] <- 0
#df_join$per <- round((df_join$threat/df_join$total)*100,2)
df_join$per <- ifelse(df_join$total==df_join$exclude, 0, round((df_join$threat/(df_join$total-df_join$exclude))*100,2))
df2 <- df_join %>% arrange(desc(per))

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
df2$nlog = log10(df2$per)
label_data$nlog = log10(label_data$per)
# Make the plot
ggplot(df2, aes(x=as.factor(id), y=per, fill=className)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=per, fill=className), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(df2$id),4), y = c(25, 50, 75, 100), label = c("25", "50", "75", "100") , color="grey", size=4 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=per, fill=className), stat="identity", alpha=0.5) +
  #scale_y_log10(limits = c(-1000, 1000))+
  ylim(-30,103) +
  theme_minimal() +
  theme(
    #legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm") 
  ) +
  coord_polar() + 
  labs(fill = "Class", y= "%")+
  geom_text(data=label_data, aes(x=id, y=per+0.2, label=orderName, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2, angle= label_data$angle, inherit.aes = FALSE ) +
  labs(title = "Best Estimate")+

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  #+
#geom_text(data=base_data, aes(x = title, y = -1, label=className), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)

ggsave(
  paste0(folderResults, folderGraph,"/",saveName,".png"),
  #ggplot_alternative(),
  width = 12,
  height = 10,
  dpi = 300
)
