# Use of Leaflet package for maps visualizations
# http://www.computerworld.com/article/2894448/business-intelligence/useful-new-r-packages-for-data-visualization-and-analysis.html

# Installation of lealfet fromGithub requries the devtools package
# install.packages("devtools)
require(devtools)

devtools::install_github("rstudio/leaflet")

require(leaflet)

#create a basic map object and titles
mymap <- leaflet()
mymap <- addTiles(mymap)

# view the emptyu map object by typing its name

mymap

#set the map where you want it to be centered and its zoomlevel
mymap <- setView(mymap, -84.3847, 33.7613, zoom = 17)
mymap

# add a popup
addPopups(mymap, -84.3847, 33.7616, 'Data journalists at work, <b>NICAR 2015</b>')

# a way to concatenate all these instructions is throught the operator %>%
mymap <- leaflet() %>% 
  addTiles() %>%
  setView(-84.3847, 33.7613, zoom = 17) %>%
  addPopups(-84.3847, 33.7616, 'Data journalists at work, <b>NICAR 2015</b>')

# see the results
mymap

###############################################################
# Something a little more interesting would be, for instance, to plot all the starbucks in a specific area
# this data can be accessed from platforms such as OpenData
# https://opendata.socrata.com/Business/All-Starbucks-Locations-in-the-US-Map/ddym-zvjk

# first download the data for all Starbucks in the US
download.file("https://opendata.socrata.com/api/views/ddym-zvjk/rows.csv?accessType=DOWNLOAD", destfile="starbucks.csv", method="curl")

# analize and visualize the data
starbucks <- read.csv("https://opendata.socrata.com/api/views/ddym-zvjk/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)
str(starbucks)
atlanta <- subset(starbucks, City == "Atlanta" & State == "GA")
leaflet() %>% addTiles() %>% setView(-84.3847, 33.7613, zoom = 13) %>%
  addMarkers(data = atlanta, lat = ~ Latitude, lng = ~ Longitude,popup = atlanta$Name) %>%
  addPopups(-84.3847, 33.7616, 'Data journalists at work, <b>NICAR 2015</b>')

#############################################################
# a third exercise will be to identofy how many Starbucks there are per inhabitant, per state

statepops <- read.csv("acs2013_1yr_statepop.csv", stringsAsFactors = FALSE)
# A little glimpse at the dplyr library; lots more on that soon
library(dplyr)
