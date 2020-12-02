URL <- "http://ichart.finance.yahoo.com/table.csv?s=SPY"
dat <- read.csv(URL)
dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
ts(dat$High)
plot(dat$High,type = "l")

install.packages("forecast")
library(forecast)
model1=ets(dat$High)
plot(model1)

tsdisplay(dat$High)
