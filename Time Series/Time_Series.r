rm(list=ls())
install.packages(c("quantmod","caTools","PerformanceAnalytics","TTR"))
require(quantmod)
require(caTools)
require(PerformanceAnalytics)
require(TTR)
getSymbols(c("LQD", "DBC", "VTI", "ICF", "SHY"), from="1990-01-01")

prices <- cbind(Ad(LQD), Ad(DBC), Ad(VTI), Ad(ICF), Ad(SHY))
View(prices)
prices <- prices[!is.na(prices[,2]),]
View(prices)
returns <- Return.calculate(prices)
View(returns)
cashPrices <- prices[, 5]
View(cashPrices)
assetPrices <- prices[, -5]
View(assetPrices)

require(caTools)
pctChannelPosition <- function(prices, dayLookback = 60, lowerPct = .25, upperPct = .75) {
  leadingNAs <- matrix(nrow=dayLookback-1, ncol=ncol(prices), NA)
  
  upperChannels <- runquantile(prices, k=dayLookback, probs=upperPct, endrule="trim")
  upperQ <- xts(rbind(leadingNAs, upperChannels), order.by=index(prices))
  
  lowerChannels <- runquantile(prices, k=dayLookback, probs=lowerPct, endrule="trim")
  lowerQ <- xts(rbind(leadingNAs, lowerChannels), order.by=index(prices))
  
  positions <- xts(matrix(nrow=nrow(prices), ncol=ncol(prices), NA), order.by=index(prices))
  positions[prices > upperQ & lag(prices) < upperQ] <- 1 #cross up
  positions[prices < lowerQ & lag(prices) > lowerQ] <- -1 #cross down
  positions <- na.locf(positions)
  positions[is.na(positions)] <- 0
  
  colnames(positions) <- colnames(prices)
  return(positions)
}

#find our positions, add them up
d60 <- pctChannelPosition(assetPrices)
d120 <- pctChannelPosition(assetPrices, dayLookback = 120)
d180 <- pctChannelPosition(assetPrices, dayLookback = 180)
d252 <- pctChannelPosition(assetPrices, dayLookback = 252)
compositePosition <- (d60 + d120 + d180 + d252)/4

compositeMonths <- compositePosition[endpoints(compositePosition, on="months"),]

returns <- Return.calculate(prices)
monthlySD20 <- xts(sapply(returns[,-5], runSD, n=20), order.by=index(prices))[index(compositeMonths),]
weight <- compositeMonths*1/monthlySD20
weight <- abs(weight)/rowSums(abs(weight))
weight[compositeMonths < 0 | is.na(weight)] <- 0
weight$CASH <- 1-rowSums(weight)

#not actually equal weight--more like composite weight, going with 
#Michael Kapler's terminology here
ewWeight <- abs(compositeMonths)/rowSums(abs(compositeMonths))
ewWeight[compositeMonths < 0 | is.na(ewWeight)] <- 0
ewWeight$CASH <- 1-rowSums(ewWeight)

rpRets <- Return.portfolio(R = returns, weights = weight)
ewRets <- Return.portfolio(R = returns, weights = ewWeight)

both <- cbind(rpRets, ewRets)
colnames(both) <- c("RiskParity", "Equal Weight")
charts.PerformanceSummary(both)
rbind(table.AnnualizedReturns(both), maxDrawdown(both))
apply.yearly(both, Return.cumulative)