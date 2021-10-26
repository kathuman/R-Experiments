require(qcc)
# generate simulated data
diameters <- as.data.frame(replicate(4, rnorm(10,mean=1.31,sd=0.05)))
View(diameters)

# qcc function to create an R chart
q <- qcc(diameters, type="R", nsigmas=3)

# qcc to create a Xbar chart
q <- qcc(diameters, type="xbar", nsigmas=3)

# Process Capability
process.capability(q, spec.limits=c(1.31,1.32))


require(TSstudio)
data("Coffee_Prices")

ts_info(Coffee_Prices)
#>  The Coffee_Prices series is a mts object with 2 variables and 701 observations
#>  Frequency: 12 
#>  Start time: 1960 1 
#>  End time: 2018 5