# Process description in https://ntguardian.wordpress.com/2017/03/27/introduction-stock-market-data-r-1/

# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

start <- as.Date("2017-01-01")

#end <- as.Date(Sys.Date())
end <- as.Date("2019-12-11")

# Let's get Novo Nordisk stock data; Novo's ticker symbol is NVO. We use the
# quantmod function getSymbols, and pass a string as a first argument to
# identify the desired ticker symbol, pass 'yahoo' to src for Yahoo!
# Finance, and from and to specify date ranges

# The default behavior for getSymbols is to load data directly into the
# global environment, with the object being named after the loaded ticker
# symbol. This feature may become deprecated in the future, but we exploit
# it now.
# Let's get data for Maresk (MAERSK-B.CO) and Carlsberg (CABGY)

getSymbols("NVO", src = "yahoo", from = start, to = end)
getSymbols(c("MAERSK-B.CO", "CABGY"), src = "yahoo", from = start, to= end)


##  As of 0.4-0, 'getSymbols' uses env=parent.frame() and
##  auto.assign=TRUE by default.
## 
##  This  behavior  will be  phased out in 0.5-0  when the call  will
##  default to use auto.assign=FALSE. getOption("getSymbols.env") and 
##  getOptions("getSymbols.auto.assign") are now checked for alternate defaults
## 
##  This message is shown once per session and may be disabled by setting 
##  options("getSymbols.warning4.0"=FALSE). See ?getSymbols for more details.

class(NVO)
head(NVO)

# Plotting the stock data

plot(NVO[, "NVO.Close"], main = "NOVO Nordisk B stock price evolution")

#CandelChart for NVO

candleChart(NVO, up.col = "black", dn.col = "red", theme = "white")

head(CABGY)
head(`MAERSK-B.CO`)

str(`MAERSK-B.CO`)
str(CABGY)
str(NVO)

# Create an xts object (xts is loaded with quantmod) that contains closing
# prices for NVO and CABGY
stocks <- as.xts(data.frame(NVO = NVO[, "NVO.Close"], CABGY = CABGY[, "CABGY.Close"], `MAERSK-B.CO` = `MAERSK-B.CO`[, "MAERSK-B.CO.Close"]))
head(stocks)

# Create a plot showing all series as lines; must use as.zoo to use the zoo
# method for plot, which allows for multiple series to be plotted on same
# plot
plot(as.zoo(stocks), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("right", c("NVO", "CABGY","MAERSK-B.CO"), lty = 1:3, cex = 0.5)

### We would much rather plot the stock's returns
# Get me my beloved pipe operator!
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}

stock_return = apply(stocks, 1, function(x) {x / stocks[1,]}) %>% 
  t %>% as.xts

head(stock_return)

plot(as.zoo(stock_return), screens = 1, lty = 1:3, xlab = "Date", ylab = "Return")
legend("topleft", c("NOVO(NVO)", "CARLSBERG(CABGY)"), lty = 1:3, cex = 0.5)


# Moving average plots

candleChart(NVO, up.col = "black", dn.col = "red", theme = "white", main="NOVO NORDISK (NVO)")
addSMA(n = 20)

# The subset argument allows specifying the date range to view in the chart.
# This uses xts style subsetting. Here, I'm using the idiom
# 'YYYY-MM-DD/YYYY-MM-DD', where the date on the left-hand side of the / is
# the start date, and the date on the right-hand side is the end date. If
# either is left blank, either the earliest date or latest date in the
# series is used (as appropriate). This method can be used for any xts
# object, say, AAPL
candleChart(NVO, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
addSMA(n = 20)

candleChart(NVO, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
addSMA(n = c(20, 50, 200))
