# https://www.r-bloggers.com/2021/04/deep-neural-network-in-r/

library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

# install.packages(c("keras","mlbench","neuralnet"))

data("BostonHousing")
data <- BostonHousing
str(data)

# Convert factor variables into numeric variables
data %<>% mutate_if(is.factor, as.numeric)

# Neural network optimization
n <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
               data = data,
               hidden = c(12,7),
               linear.output = F,
               lifesign = 'full',
               rep=1)

plot(n,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

data <- as.matrix(data)
dimnames(data) <- NULL

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
training <- data[ind==1,1:13]
test <- data[ind==2, 1:13]
trainingtarget <- data[ind==1, 14]
testtarget <- data[ind==2, 14]
str(trainingtarget)

str(testtarget)

# Scaling
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

#Model creation
model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(13)) %>%
  layer_dense(units = 1)
