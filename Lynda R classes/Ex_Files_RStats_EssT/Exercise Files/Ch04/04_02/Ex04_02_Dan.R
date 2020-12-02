# R Statistics Essential Training
# Ex04_02
# Transforming variables

require(datasets)
?islands
islands
hist(islands)
boxplot(islands)

# z-scores
islands.z <- scale(islands)  #M=0, SD=1
islands.z
hist(islands.z, breaks=16)
boxplot(islands.z)
mean(islands.z)
sd(islands.z)
attr(islands.z, "scaled:center") #shows the original mean
attr(islands.z, "scaled:scale") #shows the original SD

islands.z <- as.numeric(islands.z) #converts the matrix back to numeric
islands.z  #identifiers have been lost

#Logarithmic transformration
islands.ln <- log(islands)
islands.log10 <- log10(islands)
hist(islands.ln)
boxplot(islands.ln)
#if you have 0 in your dataset, add one first and then evaluate the ln
#if you have negative values, you may square, but recenter.

#ranking
# transofrmation that maintains the order
islands.rank1 <- rank(islands)
hist(islands.rank1)
boxplot(islands.rank1)

islands.rank2 <- rank(islands, ties.method="random")
hist(islands.rank2)
boxplot(islands.rank2)

#Dichotomizing
#splitting the data in high and lowdata. has to be used wisely and purposefully
#We willsplit at 1000
continent <- ifelse(islands>1000,1,0)
continent

rm(list=ls()) #Clean up
