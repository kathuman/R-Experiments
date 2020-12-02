

knitrSet('longit')
source('~/R/Hmisc/R/sas.get.s')  # upData(pr=FALSE)



require(rms)
getHdata(cdystonia)  
attach(cdystonia)

# Construct unique subject ID
uid <- factor(paste(site, id))

# What is the frequency of patterns of subjects' time points?
table(tapply(week, uid,
           function(w) paste(sort(unique(w)), collapse=' ')))

# Plot raw data, superposing subjects
xYplot(twstrs ~ week | site*treat, groups=uid, 
       type='b', label.curves=FALSE)  # Fig. (*\ref{fig:longit-spaghetti}*)



# Show quartiles - Fig. (*\ref{fig:longit-quartiles}*)
xYplot(twstrs ~ week | treat, method='quantile', nx=0)



detach(cdystonia)
baseline <- subset(data.frame(cdystonia,uid),
                   week == 0, -week)
baseline <- upData(baseline, rename=c(twstrs='twstrs0'),
                   pr=FALSE)
followup <- subset(data.frame(cdystonia,uid), week > 0,
                   c(uid,week,twstrs))
rm(uid)
both     <- merge(baseline, followup, by='uid')

dd       <- datadist(both)
attach(both)
dd <- datadist(dd, twstrs0, week);  options(datadist='dd')



require(nlme)
cp <- list(corCAR1,corExp,corCompSymm,corLin,corGaus,corSpher)
z  <- vector('list',length(cp))
for(k in 1:length(cp)) {
  z[[k]] <- gls(twstrs ~ treat*rcs(week,4) + rcs(twstrs0,4) +
                rcs(age,4)*sex,
                correlation=cp[[k]](form = ~week | uid))
}
anova(z[[1]],z[[2]],z[[3]],z[[4]],z[[5]],z[[6]])



a <- Gls(twstrs ~ treat*rcs(week,4) + rcs(twstrs0,4) +
         rcs(age,4)*sex,
          correlation=corCAR1(form=~week | uid))
print(a, latex=TRUE)



v <- Variogram(a, form=~ week | uid)
plot(v)  # Figure (*\ref{fig:longit-variogram}*)



p1 <- xYplot(resid(a) ~ fitted(a) | treat,
             abline=list(h=0,lty=2))
p2 <- xYplot(resid(a) ~ twstrs0, abline=list(h=0,lty=2))
p3 <- xYplot(resid(a) ~ week, method=smean.sdl, nx=0,
             abline=list(h=0,lty=2), ylim=c(-20,20))
p4 <- qqnorm(a, ~(resid(., type='n')))  
print(p1, more=TRUE, split=c(1,1,2,2)) # Figure (*\ref{fig:longit-resid}*)
print(p2, more=TRUE, split=c(2,1,2,2))
print(p3, more=TRUE, split=c(1,2,2,2))
print(p4, more=FALSE,split=c(2,2,2,2))



an <- anova(a)   # Table (*\ref{longit-anova}*)
latex(an, file='', label='longit-anova')
plot(an)         # Figure (*\ref{fig:longit-anova}*)



# Figure (*\ref{fig:longit-pleffects}*)
plot(Predict(a, week, treat, conf.int=FALSE))
plot(Predict(a, twstrs0))
plot(Predict(a, age, sex))



# Show effects (differences in means) for week 8
latex(summary(a), size='small', file='', table.env=FALSE)
# To get results for week 8 for a different reference group
# for treatment, use e.g. summary(a, week=4, treat='Placebo')



# Compare low dose with placebo, separately at each time
k1 <- contrast(a, list(week=c(2,4,8,12,16), treat='5000U'),
                  list(week=c(2,4,8,12,16), treat='Placebo'))
options(width=80)
k1
# Compare high dose with placebo
k2 <- contrast(a, list(week=c(2,4,8,12,16), treat='10000U'),
                  list(week=c(2,4,8,12,16), treat='Placebo'))
k2



p1 <- xYplot(Cbind(Contrast, Lower, Upper) ~ week, data=k1,
             ylab='Low Dose - Placebo', type='b',
             abline=list(h=0, lty=2), ylim=c(-15,10))

p2 <- xYplot(Cbind(Contrast, Lower, Upper) ~ week, data=k2,
             ylab='High Dose - Placebo', type='b',
             abline=list(h=0, lty=2), ylim=c(-15,10))

print(p1, more=T, split=c(1,1,2,1))   # Figure (*\ref{fig:longit-contrasts}*)
print(p2, more=F, split=c(2,1,2,1))



# Figure (*\ref{fig:longit-nomogram}*)
plot(nomogram(a), cex.axis=.55, cex.var=.8, lmgp=.25)


