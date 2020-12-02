rm(list=ls())
xbar <- 159
n <- 1047
ns <- 100
M = matrix(rbinom(n*ns,size=1,prob=xbar/n),nrow=n)

fIC = function(x) mean(x) + c(-1,1)*1.96*sqrt(mean(x)*(1-mean(x)))/sqrt(n)
#Function to calculate the interval of Confidence
IC = t(apply(M,2,fIC)) 
#Matrix of Values for the Interval of Confidence for each of the 100 measures
MN = apply(M,2,mean)
#list of 100 values each being the mean 

k = (xbar/n<IC[,1])|(xbar/n>IC[,2]) 
# logicasl set of values for the different measures in case these fall within the Confidence Interval
plot(MN,1:ns,xlim=range(IC),axes=FALSE,xlab="",ylab="",pch=19,cex=.7, col=c("blue","red")[1+k])
# expression c("blue","red")[1+k] determines de color of the plot depending on the value of
# k which was a logical expression, but which turn to a number 1 or 2 when adding k+1
axis(1)
segments(IC[,1],1:ns,IC[,2],1:ns,col=c("blue","red")[1+k])
abline(v=xbar/n)

#Now for the Bayesian Confidence Interval
u=seq(.1,.2,length=501)
v=dbeta(u,1+xbar,1+n-xbar)
plot(u,v,axes=FALSE,type="l")
I=u<qbeta(.025,1+xbar,1+n-xbar)
polygon(c(u[I],rev(u[I])),c(v[I],rep(0,sum(I))),col="red",density=30,border=NA)
I=u>qbeta(.975,1+xbar,1+n-xbar)
polygon(c(u[I],rev(u[I])),c(v[I],rep(0,sum(I))),col="red",density=30,border=NA)
axis(1)

pk <- rbeta(ns,1+xbar,1+n-xbar)
par(new=TRUE)
hist(pk,prob=TRUE,col="light green",border="white",axes=FALSE,main="",xlab="",ylab="",lwd=3,xlim=c(.12,.18))

M=matrix(rbinom(n*ns,size=1,prob=rep(pk,each=n)),nrow=n)
MN=apply(M,2,mean)

abline(v=qbeta(c(.025,.975),1+xbar,1+n-xbar),col="red",lty=2)
points(pk,seq(1,40,length=ns),pch=19,cex=.7)
k=(MN<qbeta(.025,1+xbar,1+n-xbar))|(MN>qbeta(.975,1+xbar,1+n-xbar))
points(MN,seq(1,40,length=ns),pch=19,cex=.7,col=c("blue","red")[1+k])
segments(MN,seq(1,40,length=ns),pk,seq(1,40,length=ns),col="grey")

