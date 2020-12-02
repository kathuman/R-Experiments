## Plotly example
## Look at Plot.ly

install.packages("devtools")
library(devtools)
install_github("plotly", "ropensci")
library(plotly)

## Loading required package: RCurl
## Loading required package: bitops
## Loading required package: RJSONIO
## Loading required package: ggplot2

library(ggplot2)

## Example 1 - Plotting CO2 Data
py <- plotly(username="kathuman", key="zk39f3ki9h")
a <- qplot(conc, uptake, data = CO2, colour = Type) + scale_colour_discrete(name = "")
py$ggplotly(a)

## Example 2 - Scatterplots
# Generate data
data <- data.frame(x=rep(1:10, times=5), group = rep(1:5, each=10))
data$lt <- c("even", "odd")[(data$group%%2+1)] # linetype
data$group <- as.factor(data$group)
data$y <- rnorm(length(data$x), data$x, .5) + rep(rnorm(5, 0, 2), each=10)
d <- ggplot() + geom_line(data=data, aes(x=x, y=y, colour=group, group=group, linetype=group)) + ggtitle("geom_line + scale_linetype automatic")
py$ggplotly(d)