# R Statistics Essential Training
# Ex04_01
# Examining outliers

setwd("~/R/Lynda R classes/Ex_Files_RStats_EssT/Exercise Files/Ch04/04_01")

#Categoricval data
#Outlier is < 10%
#Worldide shipments of Smartphones OS
#in millions for 2013 1Q
OS <-read.csv("OS.csv", header=TRUE)
View(OS)
OS

#Outlier has a proportion of < .1
#either combine them into an "other" category or delete

OS.hi <- subset(OS, Proportion>0.1)
OS.hi

#Quantitative data
require("datasets")
?rivers
data(rivers)
hist(rivers)
boxplot(rivers, horizontal=TRUE)
boxplot.stats(rivers)
rivers.low <- rivers[rivers<1210] #remove outliers
boxplot(rivers.low, horizontal=TRUE)
boxplot.stats(rivers.low)
