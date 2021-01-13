#EX01_05
#Using R's built in datasets
?datasets
library(help="datasets")
data()

#Information about a specific dataset
?airmiles

#To load a dataset into the workspace
data(airmiles)  #listed as "ts" for "time series"

#to see the contents of the data set
#it needs not be loaded to accomplish this
airmiles

#To see its structure
?str
str(airmiles)

#Now for a dataset that has rows and columns
?anscombe
data(anscombe)

#to see its structure
str(anscombe)
anscombe

rm(list=ls())  #clean up