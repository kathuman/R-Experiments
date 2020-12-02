rm(list=ls())
# install.packages('neuralnet')
library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library

#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(500, min=0, max=100))
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Train the neural network
#Going to have 10 hidden perceptrons in 1 layer
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=c(4,4), threshold=0.01)
print(net.sqrt)

#Plot the neural network
plot(net.sqrt)

#Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers
net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)

##########################
library("neuralnet")
XOR <- c(0,1,1,0)
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR)
print(net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=2, rep=5))
plot(net.xor, rep="best")

##########################
# Example2 - Trinity College, Ireland
setwd("~/R/Neural Networks in R")
rm(list=ls())
require(nnet)
cheese <- read.csv("cheese.csv")
names(cheese)
summary(cheese)
par(mfrow=c(2, 2))
View("ST4003-Lab5-Introduction_to_Neural_Networks.pdf")

fitnn1 = nnet(taste ~ Acetic + H2S + Lactic, cheese, size=0,skip=TRUE, linout=TRUE)
summary(fitnn1)

lm(taste ~ Acetic + H2S + Lactic, cheese)

#We now compute the AIC
SSE1 = sum(fitnn1$residuals^2)
AIC1 = 2*5 + 30*log(SSE1/30)
AIC1

#lets do a second model without acetic acid
fitnn2 = nnet(taste ~ H2S + Lactic, cheese, size=0, skip=TRUE,linout=TRUE)
summary(fitnn2)
SSE2 = sum(fitnn2$residuals^2)
AIC2 = 2*4 + 30*log(SSE2/30)
AIC2

AIC2 < AIC1

#Now we use one hidden layer
fitnn3 = nnet(taste ~ Acetic + H2S + Lactic, cheese, size=1,linout=TRUE)

cheese2 = scale(cheese)
fitnn3 = nnet(taste ~ Acetic + H2S + Lactic, cheese2, size=1,linout=TRUE)
summary(fitnn3)

