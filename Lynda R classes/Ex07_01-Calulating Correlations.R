# R Statistics Essential Training
# Ex07_01
# Calculating correlations

data(swiss)

#Correlation matrix for data frame
cor(swiss)
round(cor(swiss),2) #rounded to 2 decimal places

# Can test one pair of variables at a time
# Gives r, hypoothesis test and  confidence interval
cor.test(swiss$Fertility, swiss$Education)

#Install "Hmisc" package to get p-values for matrix
install.packages("Hmisc")
require("Hmisc")

help(package = "Hmisc")

#Need to coerce from data frame to matrix
#to get correlation matrix and p-values
rcorr(as.matrix(swiss))