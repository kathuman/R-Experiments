# R Statistics Essential Training
# Ex08_02
# Creating scatterplots for grouped data

#Load Data
?iris
data(iris)
iris[1:5,]

# Load "car" package
install.packages("car") #package installed Companion to Applied Regression
library(car) #Load it to the workspace

search() #Check car is available and loaded

#Now we make a single scatterplot with groups marked
#Function can be called "scatterplot" or "sp"
sp(Sepal.Width ~ Sepal.Length | Species,
   data = iris,
   xlab = "Sepal Width",
   ylab = "Sepal Length",
   main = "Iris Data",
   labels = row.names(iris))

#Clean up
detach("package:car", unload = TRUE)
rm(list = ls())


