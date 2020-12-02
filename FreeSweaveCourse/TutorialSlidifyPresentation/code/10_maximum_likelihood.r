

knitrSet('mle')
options(width=70)



p <- seq(.01, .99, by=.001)
loglike <- function(p, s, n) s*log(p)+(n-s)*log(1-p)
score <- function(p, s, n) s/p-(n-s)/(1-p)
info <- function(p, s, n) n/p/(1-p)
s <- c(50, 60, 12)
n <- c(100, 100, 20)
lt <- c(1, 3, 4)
cl <- 0.7
plot(1:2, 1:2, xlim=c(0,1), ylim=c(-150,-10), xlab="P",
     ylab="Log Likelihood", type="n")
labl <- paste("s=", s, "  n=", n, sep="")
legend(.125, -110, labl, lty=lt, bty="n", cex=cl)
q <- par()$usr
for(i in 1:3) {
  lines(p, loglike(p, s[i], n[i]), lty=lt[i])
  lines(c(.5, .5), c(q[3], loglike(.5, s[i], n[i])), lty=2)
  lines(c(q[1], .5), c(loglike(.5, s[i], n[i]), loglike(.5, s[i], n[i])), lty=2)
  pe <- s[i]/n[i]
  lines(c(pe, pe), c(q[3], loglike(pe, s[i], n[i])), lty=2)
  lines(c(q[1], pe), c(loglike(pe, s[i], n[i]), loglike(pe, s[i], n[i])), lty=2)
}



plot(0, 0, xlim=c(0,1), ylim=c(-500,600), xlab="P", ylab="Score", type="n")
legend(.05, -225, labl, lty=lt, bty="n", cex=cl)
for(i in 1:3) {
  lines(p, score(p, s[i], n[i]), lty=lt[i])
  q <- par()$usr
  lines(c(.5, .5), c(q[3], score(.5, s[i], n[i])), lty=2)
  lines(c(q[1], .5), c(score(.5, s[i], n[i]), score(.5, s[i], n[i])), lty=2)
  pe <- s[i]/n[i]
  lines(c(pe, pe), c(q[3], score(pe, s[i], n[i])), lty=2)
  lines(c(q[1], pe), c(score(pe, s[i], n[i]), score(pe, s[i], n[i])), lty=2)
}



plot(0, 0, xlim=c(0,1), ylim=c(80,1200), xlab="P", ylab="Information", type="n")
w <- c(1,3)
legend(.4, 1000, paste("n=", n[w], sep=""), lty=lt[w], bty="n", cex=cl)
for(i in w) lines(p, info(p, s[i], n[i]), lty=lt[i])



require(rms)
n <- 200
set.seed(15)
x1 <- rnorm(n)
logit <- x1/2
y <- ifelse(runif(n) <= plogis(logit), 1, 0)
dd <- datadist(x1);  options(datadist='dd')
f <- lrm(y ~ pol(x1,2), x=TRUE, y=TRUE)
print(f, latex=TRUE)
latex(anova(f), file='', table.env=FALSE)



# Get 2-row design matrix for obtaining predicted values
# for x = 1 and 5
X <- predict(f, data.frame(x1=c(1,5)), type='x')
Xdif <- X[2,,drop=FALSE] - X[1,,drop=FALSE]
Xdif
b <- bootcov(f, B=1000)
boot.log.odds.ratio <- b$boot.Coef %*% t(Xdif)
sd(boot.log.odds.ratio)
# This is the same as from summary(b, x=c(1,5)) as summary uses
# the bootstrap covariance matrix
summary(b, x1=c(1,5))[1, 'S.E.']
# Compare this s.d. with one from information matrix
summary(f, x1=c(1,5))[1,'S.E.']
# Compute percentiles of bootstrap odds ratio
exp(quantile(boot.log.odds.ratio, c(.025, .975)))
# Automatic:
summary(b, x1=c(1,5))[' Odds Ratio',]
print(contrast(b, list(x1=5), list(x1=1)), fun=exp)
# Figure (*\ref{fig:mle-bootor}*)
hist(boot.log.odds.ratio, nclass=100, xlab='log(OR)', main='')



x1s <- seq(0, 5, length=100)
pwald     <- Predict(f, x1=x1s)
psand     <- Predict(robcov(f), x1=x1s)
pbootcov  <- Predict(b, x1=x1s, usebootcoef=FALSE)
pbootnp   <- Predict(b, x1=x1s)
pbootbca  <- Predict(b, x1=x1s, boot.type='bca')
pbootbas  <- Predict(b, x1=x1s, boot.type='basic')
psimult   <- Predict(b, x1=x1s, conf.type='simultaneous')
pl <- function(w, col, lty=1) {
  llines(w$x1, w$lower, col=col, lty=lty)
  llines(w$x1, w$upper, col=col, lty=lty) }
plot(pbootnp, ylim=c(-1,9), addpanel=function(...) {
  pl(pwald,    'black')
  pl(psand,    'purple')
  pl(pbootcov, 'blue', lty=2)
  pl(pbootbca, 'green')
  pl(pbootbas, 'yellow')
  pl(psimult,  'red')
   })   # Figure (*\ref{fig:mle-bootvary}*)



set.seed(191)
x1 <- rnorm(100)
y  <- x1 + rnorm(100)
pens <- df <- aic <- c(0,.07,.5,2,6,15,60)
all <- nl <- list()

for(penalize in 1:2) {
  for(i in 1:length(pens)) {
    f <- ols(y ~ rcs(x1,5), penalty=
             list(simple=if(penalize==1)pens[i] else 0, 
                  nonlinear=pens[i]))
    df[i] <-  f$stats['d.f.']
    aic[i] <- AIC(f)
    nam <- paste(if(penalize == 1) 'all' else 'nl',
                 ' penalty:', pens[i], sep='')
    nam <- as.character(pens[i])
    p <- Predict(f, x1=seq(-2.5, 2.5, length=100), conf.int=FALSE)
    if(penalize == 1) all[[nam]] <- p else nl[[nam]] <- p
  }
  print(rbind(df=df, aic=aic))
}
all <- do.call('rbind', all); all$type <- 'Penalize All'
nl  <- do.call('rbind', nl) ; nl$type  <- 'Penalize Nonlinear'
both <- rbind.data.frame(all, nl)
require(lattice)
xyplot(yhat ~ x1 | type, groups=.set., data=both, type='l',
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(a=0, b=1, col=gray(.7))})
# Figure (*\ref{fig:mle-pmle}*)


