

knitrSet('tordinal')



require(Hmisc)
getHdata(support)
sfdm <- as.integer(support$sfdm2) - 1
sf <- function(y)
  c('Y>=1'=qlogis(mean(y >= 1)), 'Y>=2'=qlogis(mean(y >= 2)),
    'Y>=3'=qlogis(mean(y >= 3)))
s <- summary(sfdm ~ adlsc + sex + age + meanbp, fun=sf,
             data=support)
plot(s, which=1:3, pch=1:3, xlab='logit', vnames='names',
     main='', width.factor=1.5)   # Figure (*\ref{fig:tordinal-po-assumpts-support}*)



getHdata(diabetes)
a <- Ecdf(~ log(glyhb), group=frame, fun=qnorm, xlab='log(HbA1c)',
          label.curves=FALSE, data=diabetes,
          ylab=expression(paste(Phi^-1, (F[n](x)))))  # Figure (*\ref{fig:tordinal-glyhb}*)
b <- Ecdf(~ log(glyhb), group=frame, fun=qlogis, xlab='log(HbA1c)',
          label.curves=list(keys='lines'), data=diabetes,
          ylab=expression(logit(F[n](x))))
print(a, more=TRUE, split=c(1,1,2,1))
print(b, split=c(2,1,2,1))



## y ~ cohort + X1 + X2 + X3 + ...



## y ~ cohort*(X1 + X2) + X3



## u <- cr.setup(Y)         # Y=original ordinal response
## attach(mydata[u$subs,])  # mydata is the original dataset
##                          # mydata[i,] subscripts input data,
##                          # using duplicate values of i for
##                          # repeats
## y      <- u$y            # constructed binary responses
## cohort <- u$cohort       # cohort or risk set categories
## f <- lrm(y ~ cohort*age + sex)


