## These Examples 
## 1. plot a time series and decompose this time series into seasonal and non seasonal components.
## 2. plot error measurements betwen two models
## 3. plot a time series with a forcast upper and lower bounds

## 1. The following is another way of separating the observed time series into trend, seasonal and random
install.packages("forecast")
library(forecast)
data(AirPassengers)
str(AirPassengers)
tsdisplay(AirPassengers)
seasonplot(AirPassengers)
stl(AirPassengers,"periodic")
plot(stl(AirPassengers,"periodic"),main="STL Function")
#Divide time series in Components - Very Powerful
model1=ets(AirPassengers)
plot(model1)

## The following is another way of separating the observed 
## time series into trend, seasonal and random
data(AirPassengers)
f <- decompose(AirPassengers)
plot(f$figure, type="b", xaxt="n", xlab="")
monthNames <- months(ISOdate(2011,1:12,1)) #Get the month
axis(1, at=1:12, labels=monthNames, las=2)
plot(f)

## 2. plot error measurements betwen two models
model2=auto.arima(AirPassengers)
par(mfrow=c(2,1))
forecast(model1,10)
plot(forecast(model1,10))
forecast(model2,10)
plot(forecast(model2,10))
accuracy(model1)
accuracy(model2)
par(mfrow=c(2,1))
#Graph the relative error measurements
barplot(accuracy(model1),main="model1")
barplot(accuracy(model2),main="model2")


## 3. this will draw the upper and lower bound for the forecast
fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(AirPassengers, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4),lty=c(1,1,2))
