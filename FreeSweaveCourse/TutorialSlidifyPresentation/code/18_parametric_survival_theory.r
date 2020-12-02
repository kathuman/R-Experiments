

knitrSet('parsurv')



plot(0, 0, type="n", xlab="Survival for Control Subject",
     ylab="Improvement in Survival",
     xlim=c(0,1), ylim=c(0,.7))
i <- 0
hr <- seq(.1, .9, by=.1)
for(h in hr) {
  i <- i + 1
  p <- seq(.0001, .9999, length=200)
  p2 <- p^h
  d <- p2 - p
  lines(p, d, lty=i)
  maxd <- max(d)
  smax <- p[d==maxd]
  text(smax,maxd+.02, format(h), cex=.6)
}



require(rms)
haz <- survreg.auxinfo$t$hazard
times <- c(seq(0, .25, length=100), seq(.26, 2, length=150))
high <- c(6, 1.5, 1.5, 1.75)
low  <- c(0, 0, 0, .25)
dfs  <- c(1, 2, 3, 5, 7, 15, 500)
cols <- rep(1, 7)
ltys <- 1:7
i <- 0
for(scale in c(.25, .6, 1, 2)) {
  i <- i + 1
  plot(0, 0, xlim=c(0,2), ylim=c(low[i], high[i]),
       xlab=expression(t), ylab="Hazard Function", type="n")
  col <- 1.09
  j <- 0
  for(df in dfs) {
    j <- j+1
    ## Divide by t to get hazard for log t distribution
    lines(times,
          haz(log(times), 0, c(log(scale), df))/times,
          col=cols[j], lty=ltys[j])
    if(i==1) text(1.7, .23 + haz(log(1.7), 0,
         c(log(scale),df))/1.7, format(df))
  }
  title(paste("Scale:", format(scale)))
}   # Figure (*\ref{fig:parsurv-t-hazard}*)



getHdata(kprats)
kprats$group <- factor(kprats$group, 0:1, c('Group 1', 'Group 2'))
dd <- datadist(kprats); options(datadist="dd")

S <- with(kprats, Surv(t, death))
f <- survfit(S ~ group, type="fleming", data=kprats)
survplot(f, n.risk=TRUE, conf='none',   # Figure (*\ref{fig:parsurv-kprats-check}*)
         label.curves=list(keys='lines'), levels.only=TRUE)
title(sub="Nonparametric estimates", adj=0, cex=.7)

# Check fits of Weibull, log-logistic, log-normal
xl <- c(4.8, 5.9)
survplot(f, loglog=TRUE, logt=TRUE, conf="none", xlim=xl,
         label.curves=list(keys='lines'), levels.only=TRUE)
title(sub="Weibull (extreme value)", adj=0, cex=.7)
survplot(f, fun=function(y)log(y/(1-y)), ylab="logit S(t)",
         logt=TRUE, conf="none", xlim=xl,
         label.curves=list(keys='lines'), levels.only=TRUE)
title(sub="Log-logistic", adj=0, cex=.7)
survplot(f, fun=qnorm, ylab="Inverse Normal S(t)",
         logt=TRUE, conf="none",
         xlim=xl,cex.label=.7,
         label.curves=list(keys='lines'), levels.only=TRUE)
title(sub="Log-normal", adj=0, cex=.7)



fw <- psm(S ~ group, data=kprats, dist='weibull')
fl <- psm(S ~ group, data=kprats, dist='loglogistic',
          y=TRUE)
fn <- psm(S ~ group, data=kprats, dist='lognormal')
latex(fw, fi='')
latex(fl, fi='')
latex(fn, fi='')



survplot(f, conf.int=FALSE,   # Figure (*\ref{fig:parsurv-kprats-psm-np}*)
         levels.only=TRUE, label.curves=list(keys='lines'))
survplot(fl, add=TRUE, label.curves=FALSE, conf.int=FALSE)



latex(pphsm(fw), fi='', digits=4)



r <- resid(fl, 'cens')
survplot(survfit(r ~ group, data=kprats),
         conf='none', xlab='Residual',
         label.curves=list(keys='lines'), levels.only=TRUE)
survplot(survfit(r ~ 1), conf='none', add=TRUE, col='red')
lines(r, lwd=1, col='blue')   # Figure (*\ref{fig:parsurv-kprats-resid-np}*)



## psm(Surv(d.time, event) ~ 1)



## units(d.time) <- "Year"
## f <- psm(Surv(d.time,cdeath) ~ lsp(age,65)*sex)
## # default is Weibull
## anova(f)
## summary(f)          # summarize effects with delta log T
## latex(f)            # typeset math. form of fitted model
## survest(f, times=1) # 1y survival est. for all subjects
## survest(f, expand.grid(sex="female", age=30:80), times=1:2)
## # 1y, 2y survival estimates vs. age, for females
## survest(f, data.frame(sex="female",age=50))
## # survival curve for an individual subject
## survplot(f, sex=NA, age=50, n.risk=T)
## # survival curves for each sex, adjusting age to 50
## f.ph <- pphsm(f)    # convert from AFT to PH
## summary(f.ph)       # summarize with hazard ratios
##                     # instead of changes in log(T)



med   <- Quantile(fl)
meant <- Mean(fl)
haz   <- Hazard(fl)
surv  <- Survival(fl)
latex(surv, file='', type='Sinput')


# Plot estimated hazard functions and add median
# survival times to graph
survplot(fl, group, what="hazard")   # Figure (*\ref{fig:parsurv-kprats-hazard}*)
# Compute median survival time
m <- med(lp=predict(fl,
           data.frame(group=levels(kprats$group))))
m
med(lp=range(fl$linear.predictors))
m <- format(m, digits=3)
text(68, .02, paste("Group 1 median: ", m[1],"\n",
                    "Group 2 median: ", m[2], sep=""))
# Compute survival probability at 210 days
xbeta <- predict(fl,
                 data.frame(group=c("Group 1","Group 2")))
surv(210, xbeta)


