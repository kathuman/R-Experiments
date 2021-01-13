# R Statistics Essential Training
# Ex02_02
# Creating pie charts for categorical variables

#Load datasets package
require("datasets")

rm(list=ls()) #delete all previously stored data
data(chickwts)

#create a table with the frequencies
feeds <- table(chickwts$feed)
feeds

pie(feeds)
?pie