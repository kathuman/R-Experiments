rm(list=ls())

install.packages("rvest")
library(rvest)
install.packages("dplyr")
library(dplyr)
install.packages("httr")
library(httr) # >0.5
install.packages("tidyr")
library(tidyr)
install.packages("gpclib")
library(gpclib)
install.packages("rgeos")
library(rgeos)
install.packages("sp")
library(sp)
install.packages("maptools")
library(maptools)
install.packages("rgdal")
library(rgdal) # needs gdal > 1.11.0
library(ggplot2)
library(reshape2)
install.packages("gridExtra")
library(gridExtra)

# extract data from rvest-ed <div>'s and clean it up a bit
# pass in the rvested HTML object and the CSS selector to extract, also 
# indicating whether we want a number or character vector returned

extractAndCleanup <- function(data, selector, make_numeric=FALSE) {
    x <- data %>% html_nodes(selector) %>% html_text()
    x <- gsub("^[[:punct:][:space:]]*|[[:punct:][:space:]]*$", "", x)
    if (make_numeric) x <- as.numeric(gsub("[,[:space:]]*", "", x))
    x
}

bbc_vote <- html("http://www.bbc.com/news/events/scotland-decides/results")

secede <- data.frame(
    council=bbc_vote %>% extractAndCleanup(".body-row__cell--council"),
    electorate=bbc_vote %>% extractAndCleanup(".body-row__cell--electorate", TRUE),
    yes=bbc_vote %>% extractAndCleanup(".body-row__cell--yes", TRUE),
    no=bbc_vote %>% extractAndCleanup(".body-row__cell--no", TRUE),
    stringsAsFactors=FALSE)

secede <- secede %>% mutate(gone=yes>no,
                            color=ifelse(gone, "#0065BD", "#CF142B77"))

GET("http://static.bbci.co.uk/news/1.46.4-1166/js/data/maps/l/scotland-elections.js",
    write_disk("data/scotland.json"), progress())
tmp <- readLines("data/scotland.json")
writeLines(tmp[2], "data/scotland.json")

map = readOGR("data/scotland.json", "scotland-elections")