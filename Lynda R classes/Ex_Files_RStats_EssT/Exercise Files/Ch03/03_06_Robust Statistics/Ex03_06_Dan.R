# R Statistics Essential Training
# Ex03_06
# Robust statistics for univariate analyses

# More information on Robust Analysis
browseURL("http://j.mp/12YPV5L")
#or see the CRAN Task view on robust statistics at 
browseURL("http://cran.r-project.org/web/views/Robust.html")

# Load Data
?state.area
area <- state.area
area
hist(area)
boxplot(area)
boxplot.stats(area)
summary(area)

#Robust methods for describing the center
mean(area)  #not robust
median(area)
mean(area, trim=0.05)  #trimming 5% fromeach end (10% of the total)
mean(area, trim=0.1)
mean(area, trim=0.2)
mean(area, trim=0.5) #counterintuitive but it gives the median

#Robust methods for describing the variation
sd(area) #not robust
mad(area) #median absolute deviation
IQR(area) #Interquartile range, is an ordinal measure of variation,i.e. values have to be ordered first
fivenum(area) #Tukey's hinges (similar to quartiles)

rm(list=ls()) #clean up

