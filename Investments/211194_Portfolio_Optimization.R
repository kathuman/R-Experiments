## Load the required libraries
# For optimization non-linear solver
library(ROI)
library(ROI.plugin.alabama)
# Finance analytics
library(tidyquant)
# Visualization
library(plotly)
# Core
library(tidyverse)

## get the data
assets <- c("CVS", "MCK", "CAH", "WBA", "JNJ")
sort(assets)
stock_prices_tbl <- tq_get(assets, from = "2015-01-01", to = "2020-09-27")
stock_returns_tbl <- stock_prices_tbl %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, mutate_fun = periodReturn, period = "yearly", col_rename = "returns")