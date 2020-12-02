

knitrSet('ordinal')



require(rms)
getHdata(ari)  # defines ari, Sc, Y, Y.death
vclust <-
  varclus(~ illd + hlt + slpm + slpl + wake + convul + hfa + hfb +
            hfe  + hap + hcl  + hcm  + hcs  + hdi    + fde + chi +
            twb  + ldy + apn  + lcw  + nfl  + str    + gru + coh +
            ccy  + jau + omph + csd  + csa  + aro    + qcr + con +
            att  + mvm + afe  + absu + stu  + deh    + dcp + crs +
            abb  + abk + whz  + hdb  + smi2 + abd    + conj+ oto +
            puskin, data=ari)
plot(vclust)  # Figure (*\ref{fig:ordinal-varclus}*)



Sc <- transform(Sc,
                ausc = 1 * (ausc == 3),
                bul.conv = 1 * (bul.conv == 'TRUE'),
                abdominal = 1 * (abdominal == 'TRUE'))
plot.xmean.ordinaly(Y ~ age + abs(temp-37) + abs(rr-60) +
                    abs(hrat-125) + waz + bul.conv + drowsy +
                    agitated + reffort + ausc + feeding +
                    abdominal, data=Sc, cr=TRUE,
                    subn=FALSE, cex.points=.65)  # Figure (*\ref{fig:ordinal-ordinality}*)



Sc$ageg <- cut2(Sc$age, c(7, 60))



vsign.trans <- transcan(~ temp + hrat + rr, data=Sc,
                        imputed=TRUE, pl=FALSE)
Sc <- transform(Sc,
                temp = impute(vsign.trans, temp),
                hrat = impute(vsign.trans, hrat),
                rr   = impute(vsign.trans, rr))



f1 <- lrm(Y ~ ageg*(rcs(temp,5)+rcs(rr,5)+rcs(hrat,4)) + rcs(waz,4) +
          bul.conv + drowsy + agitated + reffort + ausc + feeding +
          abdominal, data=Sc, x=TRUE, y=TRUE)
# x=TRUE, y=TRUE used by resid() below
print(f1, latex=TRUE, coefs=5)



latex(anova(f1), file='', label='ordinal-anova.f1',
      caption='Wald statistics from the proportional odds model',
      size='smaller')   # Table (*\ref{ordinal-anova.f1}*)



resid(f1, 'score.binary', pl=TRUE, which=c(17,18,20,21)) # Figure (*\ref{fig:ordinal-score-binary}*)



f2 <- lrm(Y ~ age + temp + rr + hrat + waz + 
          bul.conv + drowsy + agitated + reffort + ausc +
          feeding + abdominal, data=Sc, x=TRUE, y=TRUE)
resid(f2, 'partial', pl=TRUE, label.curves=FALSE) # Figure (*\ref{fig:ordinal-partial}*)



cr0 <- lrm(Y==0 ~ age + temp + rr + hrat + waz + 
           bul.conv + drowsy + agitated + reffort + ausc +
           feeding + abdominal, data=Sc, x=TRUE, y=TRUE)
# Use the update function to save repeating model right-
# hand side.  An indicator variable for Y=1 is the
# response variable below
cr1 <- update(cr0, Y==1 ~ ., subset=Y>=1)
plot.lrm.partial(cr0, cr1, center=TRUE)  # Figure (*\ref{fig:ordinal-cratios}*)



u <- cr.setup(Y)
Sc.expanded <- Sc[u$subs, ]
y      <- u$y
cohort <- u$cohort



## # Another version of the function below
## perf <- function(fit) {
##   Sc$cohort <- 'all'
##   pred.y0 <- predict(fit, Sc, type='fitted')
##   pred.ygt0 <- 1 - pred.y0
##   s <- somers2(pred.ygt0, Y > 0)
##   list(fractions=round(c(mean(pred.ygt0 < .05),
##          mean(pred.ygt0 > .25),
##          mean(pred.ygt0 > .5)), 2),
##        somers=s)
## }



full <-
  lrm(y ~ cohort*(ageg*(rcs(temp,5) + rcs(rr,5)) + rcs(waz,4) +
      bul.conv + drowsy + agitated + reffort + ausc + feeding +
      abdominal + hydration + hxprob + pustular + crying +
      fever.ill + stop.breath + labor), 
      data=Sc.expanded, x=TRUE, y=TRUE)
# x=TRUE, y=TRUE are for pentrace, validate, calibrate below
perf <- function(fit) {  # model performance for Y=0
  pr <- predict(fit, type='fitted')[cohort == 'all']
  s <- round(somers2(pr, y[cohort == 'all']), 3)
  pr <- 1 - pr   # Predict Prob[Y > 0] instead of Prob[Y = 0]
  f <- round(c(mean(pr < .05), mean(pr > .25),
               mean(pr > .5)), 2)
  f <- paste(f[1], ', ', f[2], ', and ', f[3], '.', sep='')
  list(somers=s, fractions=f)
}  
perf.unpen <- perf(full)
print(full, latex=TRUE, coefs=5)
latex(anova(full, cohort), file='', label='ordinal-anova.cohort',
      caption='Wald statistics for \\co{cohort} in the CR model',
      size='smaller[2]')   # Table (*\ref{ordinal-anova.cohort}*)
an <- anova(full, india=FALSE, indnl=FALSE)
latex(an, file='', label='ordinal-anova.full',
      caption='Wald statistics for the continuation ratio model.
       Interactions with \\co{cohort} assess non-proportional hazards',
      caption.lot='Wald statistics for $Y$ in the
                   continuation ratio model',
      size='smaller[2]')   # Table (*\ref{ordinal-anova.full}*)



anova(full)



d <- options(digits=4)
pentrace(full,
         list(simple=c(0,.025,.05,.075,.1), 
              interaction=c(0,10,50,100,125,150)))
options(d)



full.pen <-
  update(full, 
         penalty=list(simple=.05, interaction=125))
print(full.pen, latex=TRUE, coefs=FALSE)



effective.df(full.pen)
## Compute discrimination for Y=0 vs. Y>0
perf.pen <- perf(full.pen)
# Exclude interactions and cohort effects from plot
plot(anova(full.pen), cex.labels=0.75, rm.ia=TRUE,
     rm.other='cohort  (Factor+Higher Order Factors)')
# Figure (*\ref{fig:ordinal-full-pen-anova}*)



# Workaround because \Sexpr below bombed
cat(perf.pen$fractions, '\n', file='fractions.tex')



latex(full.pen, which=1:21, file='')



## # To compare with how this was done in the first edition; didn't make a
## # difference; second plot is lower than original; TODO figure out ref.zero changes
## p <- Predict(full.pen, temp, ageg, cohort='all', ref.zero=TRUE, conf.int=FALSE)
## p1 <- plot(p, adj.subtitle=FALSE, ylim=c(-2.5, 1))
## p <- Predict(full.pen, temp, ageg, cohort='Y>=1', ref.zero=TRUE, conf.int=FALSE)
## p2 <- plot(p, adj.subtitle=FALSE, ylim=c(-2.5, 1))
## print(p1, split=c(1, 1, 1, 2), more=TRUE)
## print(p2, split=c(1, 2, 1, 2), more=FALSE)



yl <- c(-3, 1)    # put all plots on common y-axis scale

# Plot predictors that interact with another predictor
# Vary ageg over all age groups, then vary temp over its
# default range (10th smallest to 10th largest values in
# data).  Make a separate plot for each 'cohort'
# ref.zero centers effects using median x

dd <- datadist(Sc.expanded); dd <- datadist(dd, cohort)
options(datadist='dd')

p1 <- Predict(full.pen, temp, ageg, cohort,
             ref.zero=TRUE, conf.int=FALSE)
p2 <- Predict(full.pen, rr,   ageg, cohort,
              ref.zero=TRUE, conf.int=FALSE)
p <- rbind(temp=p1, rr=p2)   # Figure (*\ref{fig:ordinal-shapes-age}*):
plot(p, cond='cohort', groups='ageg', varypred=TRUE,
     adj.subtitle=FALSE, ylim=yl, label.curves=FALSE)
Key(.27, .2, other=list(cex=.6))



# For each predictor that only interacts with cohort, show
# the differing effects of the predictor for predicting
# Pr(Y=0) and Pr(Y=1 given Y exceeds 0) on the same graph

dd$limits['Adjust to','cohort'] <- 'Y>=1'
v <- Cs(waz, bul.conv, drowsy, agitated, reffort, ausc,
        feeding, abdominal, hydration, hxprob, pustular,
        crying)
yeq1 <- Predict(full.pen, name=v, ref.zero=TRUE)
yl <- c(-1.5, 1.5)
# Figure (*\ref{fig:ordinal-shapes-yeq1}*)
plot(yeq1, ylim=yl, cex.axis=.7)



dd$limits['Adjust to','cohort'] <- 'all'  # original default
all <- Predict(full.pen, name=v, ref.zero=TRUE)
plot(all, ylim=yl, cex.axis=.7)   # Figure (*\ref{fig:ordinal-shapes-all}*)



plogit  <- predict(full.pen)
f <- ols(plogit ~ ageg*(rcs(temp,5) + rcs(rr,5)) + 
         rcs(waz,4) + bul.conv + drowsy + agitated +
         reffort + ausc + feeding + abdominal + hydration +
         hxprob + pustular + crying + fever.ill +
         stop.breath + labor,
         subset=cohort=='all', data=Sc.expanded, sigma=1)

# Do fast backward stepdown
w <- options(width=120)
fastbw(f, aics=1e10)
options(w)
# 1e10 causes all variables to eventually be
# deleted so can see most important ones in order

# Fit an approximation to the full penalized model using
# most important variables
full.approx <-
  ols(plogit ~ rcs(temp,5) + ageg*rcs(rr,5) +
      rcs(waz,4) + bul.conv + drowsy + reffort + 
      ausc + feeding,
      subset=cohort=='all', data=Sc.expanded)
p <- predict(full.approx)
abserr <- mean(abs(p - plogit[cohort == 'all']))
Dxy <- somers2(p, y[cohort == 'all'])['Dxy']



f <- full.approx
f$coefficients      <- -f$coefficients
f$linear.predictors <- -f$linear.predictors

n <- nomogram(f, 
              temp=32:41, rr=seq(20,120,by=10),
              waz=seq(-1.5,2,by=.5),
              fun=plogis, funlabel='Pr(Y>0)',
              fun.at=c(.02,.05,seq(.1,.9,by=.1),.95,.98))
# Print n to see point tables
plot(n, lmgp=.2, cex.axis=.6)  # Figure (*\ref{fig:ordinal-nomogram-approx}*)
newsubject <-
  data.frame(ageg='[ 0, 7)', rr=30, temp=39, waz=0, drowsy=5,
             reffort=2, bul.conv=0, ausc=0, feeding=0)
xb <- predict(f, newsubject)



set.seed(1)   # so can reproduce results
v <- validate(full.pen, B=200, cluster=u$subs,
              subset=cohort=='all')
latex(v, file='', digits=2, size='smaller')
v <- round(v, 3)



cal <- calibrate(full.pen, B=200, cluster=u$subs, 
                 subset=cohort=='all')
err <- plot(cal)  # Figure (*\ref{fig:ordinal-calibrate}*)



## sfdm <- as.integer(sfdm2) - 1



## sf <- function(y)
##   c('Y>=1'=qlogis(mean(y >= 1)),
##     'Y>=2'=qlogis(mean(y >= 2)),
##     'Y>=3'=qlogis(mean(y >= 3)),
##     'Y>=4'=qlogis(mean(y >= 4)))



## for(i in 1:4)
##   plsmo(age, sfdm >= i, add=i>1,
##         ylim=c(.2,.8), ylab='Proportion Y>=j')
## for(i in 1:4)
##   plsmo(age, sfdm >= i, add=i>1, fun=qlogis,
##         ylim=qlogis(c(.2,.8)), ylab='logit')


