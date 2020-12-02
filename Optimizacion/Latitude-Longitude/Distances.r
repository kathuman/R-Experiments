rm(list=ls())
setwd("C:/Users/dasep/Dropbox/DTU PhD/Preliminary research/R Programs/Optimizacion/Latitude-Longitude")
sample <- read.table("Rinput.txt")

install.packages("ggmap")
install.packages("plyr")
library(ggmap)
library(plyr)

sample$locMall <- "(36.84895, -76.288018)"

google_results <- rbind.fill(apply(subset(sample, select=c("V1", "locMall")), 1, function(x) mapdist(x[1], x[2], mode="driving")))

google_results

install.packages("RgoogleMaps")
library("RgoogleMaps")
library(help="RgoogleMaps")

# Addresses in Puebla
Start <- as.numeric(getGeoCode("Av 12 Oriente 415, Puebla, Mexico"))
Finish <- as.numeric(getGeoCode("18 Oriente 609, Cholula de Rivadavia, Pue., Mexico"))

# Addresses in Santiago de Chile
Start <- as.numeric(getGeoCode("Av Colon 5185, Las Condes, Santiago, Chile"))
Finish <- as.numeric(getGeoCode("Av Pocuro 2785, Provdencia, Santiago, Chile"))

geoStart <- getGeoCode("Av Colon 5185, Las Condes, Santiago, Chile")
geoFinish <- getGeoCode("Av Pocuro 2785, Provdencia, Santiago, Chile")

# Address in Washington, DC
Start <- as.numeric(getGeoCode("The White House"))
Finish <- as.numeric(getGeoCode("Washington Monument"))

View(Start)
View(geoFinish)

# getGeoCode is a function that returns (lat,long)
# mapdist requires start and end coordinates in (long,lat)
# The values must then first be reversed. We therefore create a Start2 and Finish2 coordinates
Start2 <- Start
Finish2 <- Finish

Start2[1] <- Start[2]
Start2[2] <- Start[1]

Finish2[1] <- Finish[2]
Finish2[2] <- Finish[1]

Distance <- mapdist(Start2, Finish2, mode="driving")
View(Distance)

# Restricted number of geosearches, 2500 per session
geocodeQueryCheck()

#Obtain the map from Google Maps
mapImageData1 <- get_map(location = c(lon = Start2[1], lat = Start2[2]), color = "color", source = "google", maptype = "terrain", zoom = 13)
mapImageData1 <- get_map(location = "Santiago de Chile", color = "color", source = "google", maptype = "terrain")
# Plot the Map
ggmap(mapImageData1, extent = "device", ylab = "Latitude", xlab = "Longitude")

# Identify the route between two points
route <- route(Start2, Finish2, mode="driving")
View(route)

legs_df <- route(Start2, Finish2, alternatives=TRUE, mode="driving")
View(legs_df)

df <- legs_df[legs_df$route=="A",7:8]

names(df) <- c("lon", "lat")

df <- c(df,geoFinish)

View(df)

map <- get_googlemap("Av Colon 5185, Las COndes, Chile", markers = df, path = df, scale = 2, zoom = 14)

ggmap(map, extent = "device")
# qmap("Av Colon 5185, Las Condes, Santiago, Chile", zoom = 15, maptype = "hybrid", base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) + geom_leg(aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route), alpha = 3/4, size = 2, data = legs_df) + labs(x = "Longitude", y = "Latitude", colour = "Route") + facet_wrap(~ route, ncol = 3) + theme(legend.position = "top")

# 2500 route quieries per day
routeQueryCheck()
