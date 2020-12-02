

knitrSet('multivar')



## f <- lrm(y ~ sex + race + rcs(age,5) + rcs(weight,5) +
##          rcs(height,5) + rcs(blood.pressure,5))
## plot(anova(f))




source("shrink-groupmeans.R")




source("physiol-transcan.R")



require(Hmisc)
getHdata(support)   # Get data frame from web site
heart.rate     <- support$hrt
blood.pressure <- support$meanbp
blood.pressure[400:401]
blood.pressure[400:401] <- NA  # Create two missings
d <- data.frame(heart.rate, blood.pressure)
par(pch=46)
w <- transcan(~ heart.rate + blood.pressure,
              transformed=TRUE, imputed=TRUE, show.na=TRUE, data=d)
w$imputed$blood.pressure
plot(heart.rate, blood.pressure)
t <- w$transformed
plot(t[,'heart.rate'], t[,'blood.pressure'],
     xlab='Transformed hr', ylab='Transformed bp')
spe <- round(c(spearman(heart.rate, blood.pressure),
               spearman(t[,'heart.rate'],
                        t[,'blood.pressure'])), 2)



set.seed(123)
n <- 50
y <- runif(20*n)
group <- rep(1:20,each=n)
ybar <- tapply(y, group, mean)
ybar <- sort(ybar)
plot(1:20, ybar, type='n', axes=FALSE, ylim=c(.3,.7),
     xlab='Group', ylab='Group Mean')
lines(1:20, ybar)
points(1:20, ybar, pch=20, cex=.5)
axis(2)
axis(1, at=1:20, labels=FALSE)
for(j in 1:20) axis(1, at=j, labels=names(ybar)[j])
abline(h=.5, col=gray(.85))


