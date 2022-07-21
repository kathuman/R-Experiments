library(quantmod)
from <- c("CAD", "JPY", "USD")
to <- c("USD", "USD", "EUR")
getQuote(paste0(from, to, "=X"))

library(priceR)
dataAUD <-historical_exchange_rates("USD", to = "AUD", start_date = "2018-01-01", end_date = "2020-06-30")
plot(dataAUD)

