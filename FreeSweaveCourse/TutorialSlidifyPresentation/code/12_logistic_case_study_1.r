

knitrSet('lrcase1')



require(rms)
getHdata(prostate)
prostate <-
  within(prostate, {
    levels(ekg)[levels(ekg) %in% c('old MI','recent MI')] <- 'MI'
    ekg.norm <- 1*(ekg %in% c('normal','benign'))
    levels(ekg) <- abbreviate(levels(ekg))
    pfn <- as.numeric(pf)
    levels(pf)  <- levels(pf)[c(1,2,3,3)]
    cvd <- status %in% c("dead - heart or vascular",
                         "dead - cerebrovascular")
    rxn = as.numeric(rx) })
# Use transcan to compute optimal pre-transformations
ptrans <-   # See Figure (*\ref{fig:prostate-transcan}*)
  transcan(~ sz + sg + ap + sbp + dbp + 
           age + wt + hg + ekg + pf + bm + hx + dtime + rx,
           imputed=TRUE, transformed=TRUE,
           data=prostate, pl=FALSE, pr=FALSE)
# Use transcan single imputations
imp <- impute(ptrans, data=prostate, list.out=TRUE)
NAvars <- all.vars(~ sz + sg + age + wt + ekg)
for(x in NAvars) prostate[[x]] <- imp[[x]]
subset <- prostate$status %in% c("dead - heart or vascular",
    "dead - cerebrovascular","dead - prostatic ca")
trans <- ptrans$transformed[subset,]
psub  <- prostate[subset,]



# Function to compute the first k PCs
ipc <- function(x, k=1, ...) princomp(x, ..., cor=TRUE)$scores[,1:k]
# Compute the first 8 PCs on raw variables then on transformed ones
pc8 <- ipc(~ sz + sg + log(ap) + sbp + dbp + age +
           wt + hg + ekg.norm + pfn + bm + hx + rxn + dtime,
           data=psub, k=8)
f8   <- lrm(cvd ~ pc8, data=psub)
pc8t <- ipc(trans, k=8)
f8t  <- lrm(cvd ~ pc8t, data=psub)
# Fit binary logistic model on original variables
f <- lrm(cvd ~ sz + sg + log(ap) + sbp + dbp + age +
         wt + hg + ekg + pf + bm + hx + rx + dtime, data=psub)
# Expand continuous variables using splines
g <- lrm(cvd ~ rcs(sz,4) + rcs(sg,4) + rcs(log(ap),4) + rcs(sbp,4) +
         rcs(dbp,4) + rcs(age,4) + rcs(wt,4) + rcs(hg,4) + ekg +
         pf + bm + hx + rx + rcs(dtime,4), data=psub)
# Fit binary logistic model on individual transformed variables
h <- lrm(cvd ~ trans, data=psub)



c(f8=AIC(f8), f8t=AIC(f8t), f=AIC(f), g=AIC(g), h=AIC(h))



print(f, latex=TRUE)
an <- anova(f)
latex(an, file='', table.env=FALSE)
plot(an)   # Figure (*\ref{fig:lrcase1-full}*)
s <- f$stats
gamma.hat <- (s['Model L.R.'] - s['d.f.'])/s['Model L.R.']



dd <- datadist(psub); options(datadist='dd')
plot(Predict(f))   # Figure (*\ref{fig:lrcase1-fullpeffects}*)



plot(summary(f), log=TRUE)   # Figure (*\ref{fig:lrcase1-fullor}*)



fastbw(f)



fred <- lrm(cvd ~ sz + sg + hx, data=psub)
latex(fred, file='')
nom <- nomogram(fred, fun=plogis,
                funlabel="Probability", 
                fun.at=c(.01,.05,.1,.25,.5,.75,.9,.95,.99))
plot(nom, xfrac=.45)   # Figure (*\ref{fig:lrcase1-nom}*)



f <- update(f, x=TRUE, y=TRUE)
v <- validate(f, B=200, bw=TRUE)



latex(v, B=20, digits=3)



cal <- calibrate(f, B=200, bw=TRUE)
plot(cal)   # Figure (*\ref{fig:lrcase1-cal}*)



vfull <- validate(f, B=200)
latex(vfull, digits=3)



v5 <- validate(f, bw=TRUE, sls=0.5, type='individual', B=200)


latex(v5, digits=3, B=0)



lp <- predict(f)   # Compute linear predictor from full model
# Insert sigma=1 as otherwise sigma=0 will cause problems
a <- ols(lp ~ sz + sg + log(ap) + sbp + dbp + age + wt +
         hg + ekg + pf + bm + hx + rx + dtime, sigma=1, data=psub)
# Specify a ridiculous stopping criterion to remove all variables
s <- fastbw(a, aics=10000)
betas <- s$Coefficients   # matrix, rows=iterations
X     <- cbind(1, f$x)    # design matrix
# Compute the series of approximations to lp
ap <- X %*% t(betas)
# For each approximation compute approximation R^2 and ratio of
# likelihood ratio chi-square for approximate model to that of
# original model
m <- ncol(ap) - 1   # all but intercept-only model
r2 <- frac <- numeric(m)
fullchisq <- f$stats['Model L.R.']
for(i in 1:m) {
  lpa <- ap[,i]
  r2[i] <- cor(lpa, lp)^2
  fapprox <- lrm(cvd ~ lpa, data=psub)
  frac[i] <- fapprox$stats['Model L.R.'] / fullchisq
  }   # Figure (*\ref{fig:lrcase1-approxr2}*):
plot(r2, frac, type='b',
     xlab=expression(paste('Approximation ', R^2)),
     ylab=expression(paste('Fraction of Full Model LR ',
         chi^2, ' Preserved')))
abline(h=.95, col=gray(.83)); abline(v=.95, col=gray(.83))
abline(a=0, b=1, col=gray(.83))



fapprox <- ols(lp ~ sz + sg + log(ap) + age + ekg + pf + hx + rx,
               data=psub)
fapprox$stats['R2']   # as a check
latex(fapprox, file='')
nom <- nomogram(fapprox, ap=c(.1, .5, 1, 5, 10, 20, 30, 40),
                fun=plogis, funlabel="Probability", 
                lp.at=(-5):4,
                fun.lp.at=qlogis(c(.01,.05,.25,.5,.75,.95,.99)))
plot(nom, xfrac=.45)   # Figure (*\ref{fig:lrcase1-nomapprox}*)


