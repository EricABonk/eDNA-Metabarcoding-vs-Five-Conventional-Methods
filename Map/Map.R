# Map
library(readxl)
library(tidyverse)
x2<-read_excel("Map/Detour Samples Tracking.xlsx")

# Assuming "sample" is the name of the column you want to modify
columnName <- "Sample"
x2[, columnName][x2[, columnName] == "08-07-23-SC-REF"] <- "08-07-23 - SunC-REF"
x2[, columnName][x2[, columnName] == "09-07-23-SC-REF"] <- "09-07-23 - SunC-REF"
x2[, columnName][x2[, columnName] == "08-07-23-SC-NF"] <- "08-07-23 - SunC-EXP"
x2[, columnName][x2[, columnName] == "10-07-23-KC-EXP"] <- "10-07-23 - KC-EXP"
x2[, columnName][x2[, columnName] == "11-07-23-KC-REF"] <- "11-07-23 - KC-REF"
x2[, columnName][x2[, columnName] == "12-07-23-SC-FF"] <- "12-07-23 - SunC-FF"
x2[, columnName][x2[, columnName] == "09-07-23-SC-NF"] <- "09-07-23 - SunC-EXP"
x2[, columnName][x2[, columnName] == "11-07-23-FISH-POND"] <- "12-07-23 - RJ-POND"
x2[, columnName][x2[, columnName] == "12-07-23-HC-REF"] <- "12-07-23 - HC-REF"
x2[, columnName][x2[, columnName] == "13-07-23-HC-REF"] <- "13-07-23 - HC-REF"
eDNA<-read_excel("Data/eDNA.xlsx")

x2<-subset(x2, select=c(Sample,Lat,Long, Rep))

z<-unique(eDNA$Sample)
z<-as.data.frame(z)
x2 <- x2 %>% filter(Sample %in% z$z)

x2 <- x2 %>% distinct(Sample, .keep_all = TRUE)



library(RgoogleMaps)  
library(ggmap)
httr::set_config(httr::config(http_version = 0))
base = get_map(location=c(-79.9,49.9,-79.5,50.1),zoom=15, maptype="stamen_terrain") # Get base map
map1 = ggmap(base)
map1
?get_map()
x2$Lat<-as.numeric(x2$Lat)
x2$Long<-as.numeric(x2$Long)
citation("ggplot2")
# Make final map
map2<-map1 +  geom_jitter(data = x2, aes(y = Lat, x = Long, fill = Sample),color="black",shape=21, width = 0.003, height = 0.003) + 
  labs(y="Latitude (\u00B0)", x="Longitude (\u00B0)") +
  
  theme_bw() + theme(legend.position="right", axis.text = element_text(size = rel(0.75)), legend.key = element_rect(colour = "white"), axis.text.x = element_text(angle=45, vjust=0.5))+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(face = "bold"))+
  guides(colour = guide_legend(title = "Location",ncol = 1))+
  theme(legend.title = element_text(size=7))+
  theme(axis.text.x = element_text(size=8,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(face="bold"))
map2
# Zoom out map 


basex = get_map(location=c(-100,35,-65,60),zoom=7, maptype="stamen_terrain") # Get base map
map1 = ggmap(basex)
map1

x2$Lat<-as.numeric(x2$Lat)
x2$Long<-as.numeric(x2$Long)
citation("ggplot2")
# Make final map
map3<-map1 +  geom_jitter(data = x2, aes(y = Lat, x = Long, fill = Sample),color="black",shape=21, width = 0.003, height = 0.003) + 
  labs(y="Latitude (\u00B0)", x="Longitude (\u00B0)") +
  
  theme_bw() + theme(legend.position="right", axis.text = element_text(size = rel(0.75)), legend.key = element_rect(colour = "white"), axis.text.x = element_text(angle=45, vjust=0.5))+
  theme(legend.text =element_text(size = 6,face = "bold"))+
  theme(legend.key.size = unit(.2, 'cm')) + 
  theme(legend.title = element_text(face = "bold"))+
  guides(colour = guide_legend(title = "Location",ncol = 1))+
  theme(legend.title = element_text(size=7))+
  theme(axis.text.x = element_text(size=8,face="bold"))+
  theme(axis.title = element_text(size = 8,face="bold"))+
  theme(axis.text.y = element_text(face="bold"))
map3
# API Key 
register_stadiamaps("7553dafa-48e9-487d-967a-24bd2688b865", write = FALSE)
