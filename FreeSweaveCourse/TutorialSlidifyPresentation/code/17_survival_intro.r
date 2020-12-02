

knitrSet('survintro')



tt <- c(seq(.0001,.002,by=.001),seq(.002,.02,by=.001),
        seq(.02,1,by=.01))
# Note: bb,dd not stated in usual Weibull form
aa <- .5
bb <- -.5
cc <- 10
dd <- 4
cumhaz <- (aa/(bb+1))*tt^(bb + 1) + (cc/(dd+1))*tt^(dd + 1)
survival <- exp(-cumhaz)
hazard <- ifelse(tt>.001, aa*tt^bb + cc*tt^dd, NA)
plot(tt, survival, type="l", xlab=expression(t), ylab=expression(S(t)))


plot(tt, cumhaz, type="l", xlab=expression(t),
     ylab=expression(Lambda(t)))


plot(tt, hazard, type="l", xlab=expression(t), ylab=expression(lambda(t)))



tt <- seq(1e-5, 1.2, length=100)
plot(tt, rep(0,100), ylim=c(0,7), type="n", xlab=expression(t), ylab="")
a <- 1
i <- 0
for(b in c(.5,1,2,4)) {
  i <- i + 1
  lines(tt, a*b*tt^(b-1), lty=i)
}
legend(.4, 6, c(".5","1","2","4"), lty=1:4, cex=.9, bty="n")



require(rms)
tt <- c(1,3,3,6,8,9,10)
stat <- c(1,1,1,0,0,1,0)
S <- Surv(tt, stat)
survplot(survfit(S ~ 1), conf="bands",n.risk=TRUE,xlab=expression(t))
survplot(survfit(S ~ 1, type="fleming-harrington", conf.int=FALSE),
         add=TRUE, lty=3)



## require(rms)
## units(y) <- "Month"
## # Default is "Day" - used for axis labels, etc.
## survfit(Surv(y, event) ~ svar1 + svar2 + ... , data, subset,
##         type=c("kaplan-meier", "fleming-harrington"),
##         error=c("greenwood", "tsiatis"), se.fit=TRUE,
##         conf.int=.95,
##         conf.type=c("log","log-log","plain","none"), ...)



## f <- survfit(...)
## print(f)    # print brief summary of f
## summary(f, times, censored=FALSE)



## f <- survfit(Surv(futime, event) ~ sex)
## summary(f, seq(30, 180, by=30))



## units(y) <- "Year"
## f <- survfit(Surv(y, stat) ~ treatment)
## survplot(f, ylab="Fraction Pain-Free")



## cph(Survobject ~ predictor)


