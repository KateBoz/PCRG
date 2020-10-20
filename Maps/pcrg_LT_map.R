################################################
#  Creating a Larval Light Trap interactive Map
#
# Created by: Katelyn Bosley
# Date: 10/20/2020
################################################

#install packages
load.libraries<-function(){
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(sp)
  library(leaflet)
  library(wesanderson)
  library(car)
  library(formattable)
  library(htmltools)
  library(webshot)
  library(zoo)
  library(openxlsx)
  library(mgcv)
  library(stargazer)
  library(mapview)
  library(GGally)
}

load.libraries()

#webshot::install_phantomjs()


#read in the data
dat<-read.csv("2019StationMetadata.csv")
#look at the data
head(dat)


#clean the data
names(dat)
sapply(dat,class)


dat$Latitude.fac<-as.factor(dat$Latitude)
dat$Longitude.fac<-as.factor(dat$Longitude)



####################################################################
# Create the map
####################################################################

#save site names and locations for map and future
p<-dplyr::distinct(dat,Site_Code,Site_Name,Latitude,Longitude)
site.table = formattable(p) 

pal <- wes_palette('BottleRocket2', length(unique(p$Site.Name)), type = c("continuous"))
#pal2 <- wes_palette('BottleRocket2', nrow(mean.mega), type = c("continuous"))

m <- leaflet() %>% addProviderTiles("Esri.WorldTopoMap")
#m <- leaflet() %>% addProviderTiles("Esri.WorldPhysical")

#m %>% setView(-122.4333, 47.63333, zoom = 8) %>% addCircleMarkers(p$Longitude,p$Latitude,label = as.character(p$Site.Name),color = pal,radius = 4,labelOptions = labelOptions(noHide = T))

#sites.map<-m %>% setView(-122.4333, 47.63333, zoom = 8) %>% addCircleMarkers(p$Longitude,p$Latitude,label = as.character(p$Site.Name),color = pal,radius = 4)

sites.map<-m %>% setView(-122.4333, 47.63333, zoom = 8) %>% addCircleMarkers(p$Longitude,p$Latitude,label = as.character(p$Site_Name),color = "red",radius = 5)



#mega.map<-m %>% setView(-122.4333, 47.63333, zoom = 8) %>% addCircleMarkers(mean.mega$Longitude,mean.mega$Latitude,label = as.character(mean.mega$Station),color = "red",radius = mean.mega$mean_density*8)



mapshot(sites.map, url = paste0(getwd(), "/station_map.html"),
        file = paste0(getwd(), "/LT_location_map.png"))

#mapshot(mega.map, url = paste0(getwd(), "/megalopae_map.html"),
#        file = paste0(getwd(), "/megalopae_map.png"))


