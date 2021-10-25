rm(list=ls())
library(ggplot2)
?read.delim 
??base::delim
help.search("delimited") 
RSiteSearch("parsing text") 

ufo<-read.delim("data/ufo/ufo_awesome.tsv", sep="\t", stringsAsFactors=FALSE,header=FALSE, na.strings="")
head(ufo)

names(ufo)<-c("DateOccurred","DateReported","Location","ShortDescription","Duration","LongDescription")

ufo$DateOccurred<-as.Date(ufo$DateOccurred, format="%Y%m%d")

ufo$DateOccurred<-as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported<-as.Date(ufo$DateReported, format="%Y%m%d")


get.location<-function(l) {
  split.location<-tryCatch(strsplit(l,",")[[1]], error= function(e) return(c(NA, NA)))
  clean.location<-gsub("^ ","",split.location)
  if (length(clean.location)>2) {
    return(c(NA,NA))
  }
  else {
    return(clean.location)
  }
}

city.state<-lapply(ufo$Location, get.location)
head(city.state)

barplot(ufo$Location)
as.factor(ufo$Location)
