install.packages("clusterGeneration")
require(clusterGeneration)

set.seed(2)
num.vars<-8
num.obs<-1000

#arbitrary correlation matrix and random variables
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
parms<-runif(num.vars,-10,10)

#response variable as linear combination of random variables and random error term
y<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)

require(nnet)
rand.vars<-data.frame(rand.vars)
y<-data.frame((y-min(y))/(max(y)-min(y)))
names(y)<-'y'
mod1<-nnet(rand.vars,y,size=10,linout=T)

#import function from Github
require(RCurl)

root.url<-'https://gist.github.com/fawda123'
raw.fun<-paste(
    root.url,
    '5086859/raw/17fd6d2adec4dbcf5ce750cbd1f3e0f4be9d8b19/nnet_plot_fun.r',sep='/')
script<-getURL(raw.fun, ssl.verifypeer = FALSE)
eval(parse(text = script))
rm('script','raw.fun')

par(mar=numeric(4),mfrow=c(1,2),family='serif')
plot(mod1,nid=F)
plot(mod1)
