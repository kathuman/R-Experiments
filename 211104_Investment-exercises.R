#########################################################
#### Use Quantmod


library(quantmod)
library(xts)
library(ggplot2)
library(gridExtra) # grid.arrange

graphics.off()
rm(list=ls())

# Data Range
sdate <- as.Date("2018-07-01")
edate <- as.Date("2021-11-03")

# Samsung Electronics (005930), Naver (035420)
tsla_stock=getSymbols('TSLA',from=sdate,to=edate,auto.assign = F)
aapl_stock=getSymbols('AAPL',from=sdate,to=edate,auto.assign = F)

# Typically use previous value for NA
no.na <- which(is.na(tsla_stock[,6]))      # no for NA
tsla_stock[no.na,6] <- tsla_stock[no.na-1,6]

no.na <- which(is.na(aapl_stock[,6]))
aapl_stock[no.na,6] <- aapl_stock[no.na-1,6] 

# Only stock price
tsla_price <- tsla_stock[,6]
aapl_price <- aapl_stock[,6]

# log return using adjusted stock price
tsla_rtn <- diff(log(tsla_price),1)
aapl_rtn <- diff(log(aapl_price),1)

# draw graph
x11(width=5.5, height=6)
plot1 <- ggplot(tsla_price, aes(x = index(tsla_price), y = tsla_price)) +
  geom_line(color="blue", size=1.2) + 
  ggtitle("TSLA stock price") + xlab("Date") + ylab("Price(￦)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%y-%m", date_breaks = "3 months")

plot2 <- ggplot(tsla_rtn, aes(x = index(tsla_rtn), y = tsla_rtn)) +
  geom_line(color = "red", size=1.2) + 
  ggtitle("TSLA stock return") + xlab("Date") + ylab("Return(%)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%y-%m", date_breaks = "3 months")

grid.arrange(plot1, plot2, ncol=1, nrow = 2)

x11(width=5.5, height=6)
plot1 <- ggplot(aapl_price, aes(x = index(aapl_price), y = aapl_price)) +
  geom_line(color = "blue", size=1.2) + 
  ggtitle("AAPL stock price") + xlab("Date") + ylab("Price(￦)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%y-%m", date_breaks = "3 months")

plot2 <- ggplot(aapl_rtn, aes(x = index(aapl_rtn), y = aapl_rtn)) +
  geom_line(color = "red", size=1.2) + 
  ggtitle("AAPL stock return") + xlab("Date") + ylab("Return(%)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%y-%m", date_breaks = "3 months")

grid.arrange(plot1, plot2, ncol=1, nrow = 2)

############################################################
## https://www.r-bloggers.com/2017/04/an-introduction-to-stock-market-data-analysis-with-r-part-2/

if (!require("TTR")) {
  install.packages("TTR")
  library(TTR)
}
if (!require("quantstrat")) {
  install.packages("quantstrat", repos="http://R-Forge.R-project.org")
  library(quantstrat)
}
if (!require("IKTrading")) {
  if (!require("devtools")) {
    install.packages("devtools")
  }
  library(devtools)
  install_github("IKTrading", username = "IlyaKipnis")
  library(IKTrading)
}
library(quantmod)
library(quantstrat)

start <- as.Date("2020-10-30")
end <- as.Date("2021-10-30")

getSymbols("AAPL", src="yahoo", from = start, to = end)
class(AAPL)
head(AAPL)

candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")

AAPL_sma_10 <- SMA(
  Cl(AAPL),  # The closing price of AAPL, obtained by quantmod's Cl() function
  n = 10     # The number of days in the moving average window
)
AAPL_sma_50 <- SMA(
  Cl(AAPL),
  n = 50
)
AAPL_sma_200 <- SMA(
  Cl(AAPL),
  n = 200
)
zoomChart("2016")  # Zoom into the year 2016 in the chart
addTA(AAPL_sma_10, on = 1, col = "red")  # on = 1 plots the SMA with price
addTA(AAPL_sma_50, on = 1, col = "blue")
addTA(AAPL_sma_200, on = 1, col = "green")


############################
candleChart(adjustOHLC(AAPL), up.col = "black", dn.col = "red", theme = "white")
addLines(h = 0, col = "black", on = 3)
addSMA(n = c(20, 50), on = 1, col = c("red", "blue"))
