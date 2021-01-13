rm(list=ls())
# From http://datascienceplus.com/goodreads-webscraping-and-text-analysis-with-r-part-1/
# 1.- Webscrapping
library(data.table)   # Required for rbindlist
library(dplyr)        # Required to use the pipes %>% and some table manipulation commands
install.packages(c("RSelenium"))
library(magrittr)     # Required to use the pipes %>%
library(rvest)        # Required for read_html
library(RSelenium)    # Required for webscraping with javascript

url <- "https://www.goodreads.com/book/show/18619684-the-time-traveler-s-wife#other_reviews"
book.title <- "The time traveler's wife"
output.filename <- "GR_TimeTravelersWife.csv"

# start the RSelenium server
checkForServer()
startServer()
remDr <- remoteDriver(browserName = "firefox", port = 4444) # instantiate remote driver to connect to Selenium Server
remDr$open() # open web browser
remDr$navigate(url) 

# 3.- Machine Learning
install.packages(c("data.table","caret", "RtextTools","xgboost","ROCR"))

library(data.table)
library(dplyr)
library(caret)
library(RTextTools)
library(xgboost)
library(ROCR)