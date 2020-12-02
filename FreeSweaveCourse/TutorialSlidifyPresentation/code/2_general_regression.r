

knitrSet('gen')




source("lspline.R")




source("rcscomp.R")




source("rcsex.R")




source("regass.R")



x <- c(0,1,3,5,6)
y <- c(4,3,5.5,5,2)
plot(x, y, type="l", xlab=expression(X), ylab=expression(f(X)), axes=FALSE)
axis(1)
axis(2, labels=FALSE)



require(Hmisc)
x <- rcspline.eval(seq(0,1,.01),
                   knots=seq(.05,.95,length=5), inclx=T)
xm <- x
xm[xm > .0106] <- NA
matplot(x[,1], xm, type="l", ylim=c(0,.01),
        xlab=expression(X), ylab='', lty=1)
matplot(x[,1], x,  type="l",
        xlab=expression(X), ylab='', lty=1)



x <- seq(0, 1, length=300)
for(nk in 3:6) {
  set.seed(nk)
  knots <- seq(.05, .95, length=nk)
  xx <- rcspline.eval(x, knots=knots, inclx=T)
  for(i in 1:(nk-1))
    xx[,i]<-(xx[,i] - min(xx[,i])) /
      (max(xx[,i]) - min(xx[,i]))
  for(i in 1:20) {
    beta  <- 2*runif(nk-1) - 1
    xbeta <- xx%*%beta+2*runif(1) - 1
    xbeta <- (xbeta - min(xbeta)) /
             (max(xbeta) - min(xbeta))
    if(i==1) {
      plot(x, xbeta, type="l", lty=1,
           xlab=expression(X), ylab='', bty="l")
      title(sub=paste(nk,"knots"), adj=0, cex=.75)
      for(j in 1:nk)
        arrows(knots[j], .04, knots[j], -.03,
               angle=20, length=.07, lwd=1.5)
    }
    else lines(x, xbeta, col=i)
  }
}



plot(0:1, 0:1, xlab=expression(X[2]), ylab=expression(C(Y)),
     axes=FALSE,  type='n')
axis(1, at=0:1, labels=rep('',2))
axis(2, at=0:1, labels=rep('',2))
lines(c(.05, .8), c(.05, .5))
lines(c(.05, .8), c(.30, .75))
text(.9, .5, expression(X[1]==0), adj=.5)
text(.9, .75,expression(X[1]==1), adj=.5)


