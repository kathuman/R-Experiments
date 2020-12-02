

knitrSet('nh')



require(rms)
getHdata(nhgh)
w <- subset(nhgh, age >= 21 & dx==0 & tx==0, select=-c(dx,tx))
latex(describe(w), file='')
dd <- datadist(w); options(datadist='dd')



i <- with(w, age >= 45 & age <= 55)
u <- ecdf(w$gh[i])
# Compute normal inverse CDF from central age interval
z <- function(x) {
  x <- pmin(pmax(u(x), .001), .999)
  qnorm(x)
}
# Stratify normal inverse CDF of z(gh) by quartiles of age
# with age < 80.  Plot inverse normal trans. of ECDF so can
# just check for linearity
Ecdf(~ z(gh), groups=cut2(age, g=4), data=subset(w, age < 80), 
     fun=qnorm,   # Figure (*\ref{fig:nh-ecdf}*)
     xlab='Empirical Z-Transformation of Glycohemoglobin',
     ylab='Normal Inverse ECDF',
     label.curve=list(keys='lines'))



f <- lrm(gh ~ rcs(age,5), data=w, x=TRUE)
aget <- predict(f, type='terms')
cuts <- seq(4.5, 8, by=.1)
cof <- numeric(length(cuts))
for(k in 1:length(cuts)) {
  cof[k] <- coef(lrm(gh >= cuts[k] ~ aget, data=w))[2]
}
plot(cuts, cof, xlab='Glycohemoglobin Cutoff',   # Figure (*\ref{fig:nh-trypo}*)
     ylab='Coefficient of Transformed age', type='l')



f <- Rq(gh ~ rcs(age,5) + log(ht) + log(wt), data=w)
print(f, latex=TRUE)
g <- Rq(gh ~ rcs(age,5) + rcs(log(ht),4) + rcs(log(wt),4),
        data=w)
lan <- function(a) latex(a, table.env=FALSE, file='')
lan(anova(g, ht, wt))



v <- varclus(~ wt + ht + bmi + leg + arml + armc + waist +
             tri + sub + age + sex + re, data=w)
plot(v)  # Figure (*\ref{fig:nh-redun}*)
# Omit wt so it won't be removed before bmi
redun(~ ht + bmi + leg + arml + armc + waist + tri + sub,
      data=w, r2=.75)



f <- Rq(ht ~ rcs(age,4)*sex, data=w)
plot(Predict(f, age, sex))   # Figure (*\ref{fig:nh-htchange}*)



s <- spearman2(gh ~ age + sex + re + wt + leg + arml + armc +
               waist + tri + sub, data=w, p=2)
plot(s)   # Figure (*\ref{fig:nh-allocadf}*)



f <- Rq(gh ~ rcs(age,5) + sex + re + rcs(wt,3) + rcs(leg,3) +
        arml + rcs(armc,3) + rcs(waist,4) + tri + rcs(sub,3),
        data=w, x=TRUE, y=TRUE)
lstats <- function(f) {
  s <- f$stats
  rho <- spearman(fitted(f), w$gh)
  cat('\n',
      '$n=', s['n'],', p=',s['p'],', g=',round(s['g'],3),
      ', \\textrm{mean}|Y-\\hat{Y}|=',round(s['mad'],3),
      ', \\rho=',round(rho,3), '$\n\n', sep='')
}
lstats(f)



print(fastbw(f, rule='p'), estimates=FALSE)



lan(anova(f, armc, tri, sub))
g <- Rq(gh ~ rcs(age,5) + re + rcs(leg,3) + rcs(armc,3) +
        tri + rcs(sub,3), data=w)
lstats(g)
# Compare against a model that substitutes waist for
# armc, tri, sub
h <- Rq(gh ~ rcs(age,5) + sex + re + rcs(leg,3) +
        rcs(waist,4), data=w)
lstats(h)



set.seed(13)  # so can reproduce results 
v <- validate(f, B=200, bw=TRUE, estimates=FALSE, rule='p')



latex(v, B=30, file='')
# shows variables selected for first 30 boots.  MAD =
# mean absolute difference between predicted and observed gh



a <- aregImpute(~ gh + wt + ht + bmi + leg + arml + armc +
                waist + tri + sub,
                data=w, n.impute=5, pr=FALSE)
f <- fit.mult.impute(gh ~ rcs(age,5) + sex + re + rcs(leg,3) +
                     rcs(waist,4), Rq, a, data=w, pr=FALSE)
print(f, latex=TRUE)
lan(anova(f))
plot(Predict(f))   # Figure (*\ref{fig:nh-peffects}*)



g <- Rq(gh ~ rcs(age,5) + sex + re + rcs(leg,3) +
        rcs(waist,4), data=w, x=TRUE, y=TRUE)
gb <- bootcov(g, B=500)   # Stores all bootstrap betas in gb



bootcl  <- Predict(gb, age)
multimp <- Predict(f, age)
plot(Predict(g, age), addpanel=function(...) {   # Figure (*\ref{fig:nh-peffects2}*)
  with(bootcl, {llines(age, lower, col='blue')
                llines(age, upper, col='blue')})
  with(multimp, {llines(age, lower, col='red')
                 llines(age, upper, col='red')
                 llines(age, yhat, col='red')})},
     col.fill=gray(.9))



f <- Newlevels(f, list(re=abbreviate(levels(w$re))))
nom <- nomogram(f)   # Figure (*\ref{fig:nh-nomogram}*)
plot(nom, lplabel='Median Glycohemoglobin', lmgp=.2)



f <- fit.mult.impute(gh ~ rcs(age,5) + sex + re + rcs(leg,3)+
                     rcs(waist,4), ols, a, data=w, pr=FALSE)
f <- Newlevels(f, list(re=abbreviate(levels(w$re))))
nom <- nomogram(f)   # Figure (*\ref{fig:nh-nomogram-ols}*)
plot(nom, lplabel='Mean Glycohemoglobin', lmgp=.2)


