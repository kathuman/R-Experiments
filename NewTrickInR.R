## Some New Tricks in R
##
## 1.Use the data Editor
## 2.Dealing with Missing data
## 3.Define number of digits in result
## 4.Loop Examples
## 5.Include labels in Scatterplot Graphs
## 6.Draw Normal Distributions
## 7.Draw Sine and Cosine Functions
## 8.3D Plot of Half of a Torus
## 9.Using quantmod package - Downloading Financial Data from Internet
## 10.Use of TTR Package - Graph Financial Data
## 11.Waterfall Graphs
## 12.Advanced graphics
## 13.Forecasts
## 14.Uniform Distribution Generator
## 15. Googlevis
## 17. Quandl Application (Access to Databases Online)
## 18. Waterfall charts
## 19. Maps Plots

## 1. Using the Data Editor - Example
MinWage <- data.frame(Year = numeric(), Value = numeric())
MinWage <- edit(MinWage) #Alternate Method
data.entry(MinWage) #Alternate method

## 2.Dealing with Missing data
x <- c(1,2,3,4,5,6,NA,8,9,10)
mean(x)
mean(x, na.rm = TRUE)

## 3.Define the nuber of digits for the results
getOption("digits")
pi
options(digits = 4)

## 4.Loop Examples
i <− c(1:5)
for (n in i) print(n * 10) ## Multiply each component of i by 10

## 5.Include label"s in Scatterpoint data plot
## Requires the "calibrate" library
library("calibrate")
x <- rnorm(50)
y <- rnorm(50)
plot(x,y,asp=1)
textxy(x,y,1:50,m=c(mean(x),mean(y)))

## 6.Draw Normal Distributions
xaxis <− seq(0, 40, .5)
y1 <− dnorm(xaxis, 20, 6)
y2 <− dnorm(xaxis, 20, 3)
plot(xaxis, y2, type = "l", main = "Comparing Two Normal Distributions")
points(xaxis, y1, type = "l", col = "red")

## 7.Draw Sine and Cosine Functions
x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n",
        main = expression(paste(plain(sin) * phi, "  and  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi, "cos" * phi), # only 1st is taken
        xlab = expression(paste("Phase Angle ", phi)),
        col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
     labels = expression(-pi, -pi/2, 0, pi/2, pi))


abline(h = 0, v = pi/2 * c(-1,1), lty = 2, lwd = .1, col = "gray70")



## 8.3D Plot of Half of a Torus
install.packages("plot3D") ##Istall Necessary Package
library("plot3D")
par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 1))
R <- 3
r <- 2
x <- seq(0, 2*pi,length.out=50)
y <- seq(0, pi,length.out=50)
M <- mesh(x, y)

alpha <- M$x
beta <- M$y

## 9.Using quantmod package - Downloading Financial Data from Internet
install.packages("quantmod") ##Install Necessary Package
library("quantmod")
getSymbols("YHOO",src="google") # from google finance
getSymbols("GOOG",src="yahoo") # from yahoo finance
getSymbols("DEXJPUS",src="FRED")  # FX rates from FRED

## Graph Financial Data with quantmod package
getSymbols("AAPL",src="yahoo") 
barChart(AAPL) 
candleChart(AAPL,multi.col=TRUE,theme="white") 

getSymbols("XPT/USD",src="oanda") 
chartSeries(XPTUSD,name="Platinum (.oz) in $USD") 
chartSeries(to.weekly(XPTUSD),up.col='white',dn.col='blue') 

## 10.Use of TTR Package - Graph Financial Data
require(TTR)
getSymbols("AAPL") 
chartSeries(AAPL)
addMACD()
addBBands() 

## 11.Waterfall Graphs
install.packages("waterfall")
library("library")
data(jaquith)
waterfallchart(jaquith$factor~jaquith$score,jaquith)

library(waterfall)
data(rasiel)
b=rasiel
waterfallchart(b[,1]~b[,2],b,col=terrain.colors(3))
#Note we use square brackets to refer to the first and second columns

## 12.Advanced graphics
install.packages("playwith")
library(playwith)
library(zoo)
playwith(xyplot(sunspots ~ yearmon(time(sunspots)), xlim = c(1900, 1930), type="l"), time.mode = TRUE)

## 13.Forecasts
install.packages("forecast")
library(forecast)
data(AirPassengers)
str(AirPassengers)
tsdisplay(AirPassengers)
seasonplot(AirPassengers)
stl(AirPassengers,"periodic")
plot(stl(AirPassengers,"periodic"),main="STL Function")
#Divide time series in Components - Very Powerful
model1=ets(AirPassengers)
plot(model1)

model2=auto.arima(AirPassengers)
par(mfrow=c(2,1))
forecast(model1,10)
plot(forecast(model1,10))
forecast(model2,10)
plot(forecast(model2,10))
accuracy(model1)
accuracy(model2)
par(mfrow=c(2,1))
#Graph the relative error measurements
barplot(accuracy(model1),main="model1")
barplot(accuracy(model2),main="model2")

##The followign is another way of separating the observed time series into trend, seasonal and random
data(AirPassengers)
f <- decompose(AirPassengers)
f$figure
plot(f$figure, type="b", xaxt="n", xlab="")
monthNames <- months(ISOdate(2011,1:12,1)) #Get the Month's names
axis(1, at=1:12, labels=monthNames, las=2)
plot(f)

fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(AirPassengers, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4),lty=c(1,1,2)) #this will draw the upper and lower bound for the forecast


##The following finds the optimal alignment between two time series
install.packages("dtw")
library(dtw)
idx <- seq(0, 2*pi, len=100)
a <- sin(idx) + runif(100)/10
b <- cos(idx)
align <- dtw(a, b, step=asymmetricP1, keep=T)
dtwPlotTwoWay(align)

## 14.Uniform Distribution Generator
runif(100, min=2, max=5)
hist(runif(1000, min=2, max=5))

Nsim=10^4 #number of random variables
U=runif(Nsim)
X=-log(U) #transforms of uniforms
Y=rexp(Nsim) #exponentials from R
par(mfrow=c(1,2)) #plots
hist(X,freq=F,main="Exp from Uniform")
hist(Y,freq=F,main="Exp from R")

## 15. Googlevis
library(XML)
url <- "http://www.gdacs.org/Cyclones/report.aspx?eventid=41058&episodeid=28&eventtype=TC"
dat <- readHTMLTable(readLines(url), which=5)
dat$latlon <- dat[,8]
levels(dat$latlon) <- sapply(
  strsplit(levels(dat[,8]), ",\n "),
  function(x) paste(x[2], x[1], sep=":")
)
dat$Category <- factor(dat$Category, levels=levels(dat$Category)[c(6,7,1:5)],
                       ordered=TRUE)
dat$cat <- as.numeric(dat$Category)
dat$Gust_kmh <- dat[,6]
levels(dat$Gust_kmh) <- sapply(strsplit(levels(dat[,6]), "km"),
                               function(x) gsub(" ", "",x[1]))
dat$Gust_kmh <- as.numeric(as.character(dat$Gust_kmh))

install.packages("googleVis")
library(googleVis)
M <- gvisGeoChart(dat, "latlon", sizevar="cat",
                  colorvar="Gust_kmh",
                  options=list(region='035',
                               backgroundColor="lightblue",
                               datalessRegionColor="grey"))
plot(M)

#########################
#Googlevis Example2
# MotionChart
library(WDI)
indnams <- c("fertility.rate", "life.expectancy",
             "population", "GDP.per.capita.Current.USD",
             "15.to.25.yr.female.literacy")
inds <- c('SP.DYN.TFRT.IN','SP.DYN.LE00.IN',
          'SP.POP.TOTL','NY.GDP.PCAP.CD',
          'SE.ADT.1524.LT.FE.ZS')
wdiData <- WDI(country="all", indicator=inds,
               start=1960, end=format(Sys.Date(), "%Y"),
               extra=TRUE)
colnum <- match(inds, names(wdiData))

names(wdiData)[colnum] <- indnams
## Create a motion chart
library(googleVis)
WorldBank <- droplevels(subset(wdiData,
                               !region %in% "Aggregates"))
M <- gvisMotionChart(WorldBank,
                     idvar="country", timevar="year",
                     xvar="life.expectancy",
                     yvar="fertility.rate",
                     colorvar="region",
                     sizevar="population",
                     options=list(width=550, height=500))
## Display the chart in the browser
plot(M)

#####################
# save a numeric vector containing 48 monthly observations
# from Jan 2009 to Dec 2012 as a time series object
myts <- ts(AirPassengers, start=c(2009, 1), end=c(2012, 12),frequency=12)

# subset the time series (June 2012 to December 2012)
myts2 <- window(myts, start=c(2012, 6), end=c(2012, 12))

# plot series
plot(myts)

# Seasonal decompostion
fit <- stl(myts, s.window="period")
plot(fit)

# additional plots
monthplot(myts)
library(forecast)
seasonplot(myts)

# simple exponential - models level
fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(myts, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(myts)

# predictive accuracy
library(forecast)
accuracy(fit)

# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3)) 

##16. WDI Information - World Development Indicators
install.packages("WDI")
require("WDI")
info <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD",start = 2005, end = 2011, extra = FALSE, cache = NULL)
## This functions retrieves information from the WDI
WDIsearch() #this function gives you the different available indicators
countryID <- c("Argentina","Chile")
subset(info,country==countryID)
plot(subset(info,country==countryID)$year,subset(info,country==countryID)$NY.GDP.PCAP.CD, type="l")

##################################
## 17. Quandl Application
install.packages("Quandl")
library(Quandl)

## The Databases have to be Accessed by fist athenticating a Token
Quandl.auth("EuemUhtMyVkz2Xv67dec")  			# Authenticate your token

## Access to a Database
mydata <- Quandl("FRED/GDP")

## Access to timeseries
mytimeseries <- Quandl("NSE/OIL", type="ts")

## You can get multiple datasets in one call by passing an array of Quandl codes
mydata = Quandl(c("NSE/OIL.4","WIKI/AAPL.1"))

## Data Manipulation at the time of download
mydata = Quandl("NSE/OIL", start_date="yyyy-mm-dd", end_date="yyyy-mm-dd") #Date restriction
mydata = Quandl("NSE/OIL", collapse="annual") #Timelapse restriction ("weekly"|"monthly"|"quarterly"|"annual")
mydata = Quandl("NSE/OIL", transformation="rdiff") #Transformations ("diff"|"rdiff"|"normalize"|"cumulative")

#Search Quandl Databases from within R console
Quandl.search(query = "crude oil", page = 2, source = "DOE", silent = TRUE)
searchResults <- Quandl.search(query = "debt chile", silent = TRUE)

ChileData <- Quandl("WORLDBANK/CHL_DT_DOD_DECT_CD_FD")
plot(ChileData, type="l")

################################
## DataMarket Application
rm(list=ls())
install.packages("rdatamarket")
library(rdatamarket)
library(Quandl)
ausgdp <- as.ts(dmseries("http://data.is/1jDQwpr")[,1])
ausgdp2 <- ts(rev(Quandl("FRED/AUSRGDPC", type="ts")), end=2011)
plot(ets(ausgdp))
tsdisplay(ausgdp)
    
## more sources http://www.r-bloggers.com/financial-data-accessible-from-r-part-iii/
## library(Quantl)
## library(Quantmod)
## library(TFX)
## library(Rbbg)
## library(IBrokers)
## library(rdatastream)
## library(pwt)
## library(flmport)
## library(Thinknum)
install.packages("Thinknum")

library("Thinknum")
goog <- Thinknum("genderstats_sh_dyn_nmrt@chl")
goog2 <- Thinknum("genderstats_sh_dyn_nmrt@chn")
plot(goog2, type="l", xlab="year", ylim=c(0,23), col="red")
lines(goog, type="l")
legend(1,2, c("China","Chile"),pch=1:2)

################################################
## 18. Waterfall charts

library(latticeExtra)
library(waterfall)
data(rasiel) # Example data of the waterfall package
rasiel
asTheEconomist(waterfallchart(value~label, data=rasiel,groups=subtotal, main="P&L"))

asTheEconomist(
  waterfallchart(value~label, data=rasiel,
                 groups=subtotal, main="P&L",
                 panel=function(x, y, ...) {
                   panel.waterfallchart(x, y, ...);
                   ltext(x=seq(1,7,1),y=c(75,75,10,1,20,15,14),
                         labels=c("+150","-170","-20","+10","+18","-2","+6"),
                         srt=90,font=2,cex=1.5)
                 }
  )
)

#####################################
## 19. Maps Plots
install.packages(c("WDI", "dplyr"))
install.packages("devtools")
require(devtools)
install_github("ramnathv/rCharts@dev")
install_github("ramnathv/rCharts")
install_github("ramnathv/rMaps")
library(WDI)
library(rMaps)
library(dplyr)
install.packages("countrycode")
library(countrycode)
# Get CO2 emission data from World bank
# Data source : http://data.worldbank.org/indicator/EN.ATM.CO2E.KT/
df <- WDI(country=c("all"), 
          indicator="EN.ATM.CO2E.KT", 
          start=2004, end=2013)
# Data manipulation By dplyr
data <- df %.% 
  na.omit() %.%
  #Add iso3c format country code 
  mutate(iso3c=countrycode(iso2c, "iso2c", "iso3c")) %.% 
  group_by(iso3c) %.%
  #Get the most recent CO2 emission data
  summarize(value=EN.ATM.CO2E.KT[which.max(year)])
# Visualize it by rMaps
i1 <- ichoropleth(value~iso3c, data, map="world")
i1$show("iframesrc", cdn = TRUE) # for blog post
i1
#... or you can direct plot by just evaluating "i1" on R console.

library(rMaps)
install.packages("magrittr")
library(magrittr)
DMdata <- read.csv("http://dl.dropboxusercontent.com/u/956851/INCIDENCE_STATE_ESTIMATES.csv", 
                   as.is = TRUE, skip = 1)
DMdata <- DMdata[, c("State", grep("Age.adjusted.Rate.per", colnames(DMdata), 
                                   value = TRUE))]
colnames(DMdata) <- c("State", paste(sep = "", 1996:2010, "percent"))
DMdata <- reshape2::melt(DMdata, id.var = "State")
DMdata$variable <- as.character(DMdata$variable) %>% gsub(pattern = "percent", 
                                                          replacement = "") %>% as.numeric()
colnames(DMdata) <- c("State", "Year", "Incidence")
DMdata$Incidence <- as.numeric(DMdata$Incidence)
DMdata <- subset(DMdata, complete.cases(DMdata))
DMdata$State <- state.abb[match(as.character(DMdata$State), state.name)]
i1 <- ichoropleth(Incidence ~ State, data = DMdata, animate = "Year", play = TRUE)
i1$show("iframesrc", cdn = TRUE)
i1

