rm(list=ls())
# Small example of how to use causal inference
# Reference: http://google.github.io/CausalImpact/CausalImpact.html


# First we install teh CausalImpact library
install.packages("devtools")
library(devtools)
devtools::install_github("google/CausalImpact")

library(CausalImpact)

# We now create an example dataset
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)

dim(data)
View(data)
head(data)

# We can visualize the data
matplot(data, type = "l")

# we then identofy the two periods, pre and post intervention
# the model will be trined in the pre intervention period and tested after that
pre.period <- c(1, 70)
post.period <- c(71, 100)

# we then perform the Impact analysis
impact <- CausalImpact(data, pre.period, post.period)
# and we plot the results
plot(impact)

# the resulting plots are 3
#   1. Prediction for the post treatment period (in blue) and the comparison with the actual values
#   2. Point by point difference for the actual versus the predicted
#   3. Cumulative effect

