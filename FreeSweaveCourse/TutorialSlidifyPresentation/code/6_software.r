

knitrSet()



## response ~ terms



## y ~ age + sex            # age + sex main effects
## y ~ age + sex + age:sex  # add second-order interaction
## y ~ age*sex              # second-order interaction +
##                          # all main effects
## y ~ (age + sex + pressure)^2
##                  # age+sex+pressure+age:sex+age:pressure...
## y ~ (age + sex + pressure)^2 - sex:pressure
##                        # all main effects and all 2nd order
##                        # interactions except sex:pressure
## y ~ (age + race)*sex     # age+race+sex+age:sex+race:sex
## y ~ treatment*(age*race + age*sex)
##                          # no interact. with race,sex
## sqrt(y) ~ sex*sqrt(age) + race
## # functions, with dummy variables generated if
## # race is an R factor (classification) variable
## y ~ sex + poly(age,2)# poly makes orthogonal polynomials
## race.sex <- interaction(race,sex)
## y ~ age + race.sex   # if desire dummy variables for all
##                      # combinations of the factors



## lrm(y ~ rcs(x,4))



## f  <- lrm(y ~ rcs(x,4) + x2 + x3)
## f2 <- update(f, subset=sex=="male")
## f3 <- update(f, .~.-x2)         # remove x2 from model
## f4 <- update(f, .~. + rcs(x5,5))# add rcs(x5,5) to model
## f5 <- update(f, y2 ~ .)     # same terms, new response var.



## y ~ sex + lsp(age,c(20,30,40,50,60)) +
##     sex %ia% lsp(age,c(20,30,40,50,60))



## y ~ lsp(age,30) + rcs(cholesterol,4) +
##     lsp(age,30) %ia% rcs(cholesterol,4)



## options(na.action="na.omit")# don't report frequency of NAs



## options(na.action="na.delete", na.detail.response=TRUE,
##         na.fun.response="mystats")
## # Just use na.fun.response="quantile" if don't care about n
## mystats <- function(y) {
##   z <- quantile(y, na.rm=T)
##   n <- sum(!is.na(y))
##   c(N=n, z)    # elements named N, 0%, 25%, etc.
## }



## fit <- ols(y ~ x1*x2*x3)
## # run, and print results:
## fastbw(fit, optional_arguments)
## # typically used in simulations:
## z <- fastbw(fit, optional_args)
## # least squares fit of reduced model:
## lm.fit(X[,z$parms.kept], Y)



## f <- lrm(y ~ x1 + x2 + ..., data=df, x=TRUE, y=TRUE)
## w <- which.influence(f, .4)
## nam <- names(w)
## for(i in 1:length(nam)) {
##    cat("Influential observations for effect of",
##        nam[i],"\n")
##    print(df[w[[i]],])
## }



## require(rms)               # make new functions available
## ddist <- datadist(cholesterol, treat, num.diseases, age)
## # Could have used ddist <- datadist(data.frame.name)
## options(datadist="ddist")       # defines data dist. to rms
## cholesterol <- impute(cholesterol)
## fit <- lrm(y ~ treat + scored(num.diseases) + rcs(age) +
##                log(cholesterol+10) +
##                treat:log(cholesterol+10))
## describe(y ~ treat + scored(num.diseases) + rcs(age))
## # or use describe(formula(fit)) for all variables used in
## # fit.  describe function (in Hmisc) gets simple statistics
## # on variables
## # fit <- robcov(fit)# Would make all statistics that follow
##                     # use a robust covariance matrix
##                     # would need x=TRUE, y=TRUE in lrm()
## specs(fit)          # Describe the design characteristics
## anova(fit)
## anova(fit, treat, cholesterol) # Test these 2 by themselves
## plot(anova(fit))             # Summarize anova graphically
## summary(fit)                 # Est. effects; default ranges
## plot(summary(fit)) # Graphical display of effects with C.I.
## # Specific reference cell and adjustment value:
## summary(fit, treat="b", age=60)
## # Estimate effect of increasing age: 50->70
## summary(fit, age=c(50,70))
## # Increase age 50->70, adjust to 60 when estimating
## # effects of other factors:
## summary(fit, age=c(50,60,70))
## # If had not defined datadist, would have to define
## # ranges for all variables
## 
## # Estimate and test treatment (b-a) effect averaged
## # over 3 cholesterols:
## contrast(fit, list(treat='b', cholesterol=c(150,200,250)),
##               list(treat='a', cholesterol=c(150,200,250)),
##          type='average')
## 
## p <- Predict(fit, age=seq(20,80,length=100), treat,
##              conf.int=FALSE)
## plot(p)              # Plot relationship between age and
##                      # log odds, separate curve for each
##                      # treat, no C.I.
## plot(p, ~ age | treat)           # Same but 2 panels
## bplot(Predict(fit, age, cholesterol, np=50))
##                      # 3-dimensional perspective plot for
##                      # age, cholesterol, and log odds
##                      # using default ranges for both
## # Plot estimated probabilities instead of log odds:
## plot(Predict(fit, num.diseases,
##              fun=function(x) 1/(1+exp(-x)),
##              conf.int=.9), ylab="Prob")
## # Again, if no datadist were defined, would have to tell
## # plot all limits
## logit <- predict(fit, expand.grid(treat="b",num.dis=1:3,
##                   age=c(20,40,60),
##                   cholesterol=seq(100,300,length=10)))
## # Could obtain list of predictor settings interactively
## logit <- predict(fit, gendata(fit, nobs=12))
## # An easier approach is
## # Predict(fit, treat='b',num.dis=1:3,...)
## 
## # Since age doesn't interact with anything, we can quickly
## # and interactively try various transformations of age,
## # taking the spline function of age as the gold standard.
## # We are seeking a linearizing transformation.
## 
## ag <- 10:80
## logit <- predict(fit, expand.grid(treat="a", num.dis=0,
##                  age=ag,
##                  cholesterol=median(cholesterol)),
##                  type="terms")[,"age"]
## # Note: if age interacted with anything, this would be the
## # age `main effect' ignoring interaction terms
## # Could also use logit <- Predict(f, age=ag, ...)$yhat,
## # which allows evaluation of the shape for any level of
## # interacting factors.  When age does not interact with
## # anything, the result from predict(f, ..., type="terms")
## # would equal the result from Predict if all other terms
## # were ignored
## 
## # Could also specify:
## # logit <- predict(fit,
## #                  gendata(fit, age=ag, cholesterol=...))
## # Unmentioned variables are set to reference values
## 
## plot(ag^.5, logit)   # try square root vs. spline transform.
## plot(ag^1.5, logit)  # try 1.5 power
## 
## # Pretty printing of table of estimates and
## # summary statistics:
## print(fit, latex=TRUE) # print (*\LaTeX*) code to console
## latex(fit)             # invokes latex.lrm, creates fit.tex
## # Draw a nomogram for the model fit
## plot(nomogram(fit))
## 
## # Compose R function to evaluate linear predictors
## # analytically
## g <- Function(fit)
## g(treat='b', cholesterol=260, age=50)
## # Letting num.diseases default to reference value



## age.tertile <- cut2(age, g=3)
## # For auto ranges later, specify age.tertile to datadist
## fit <- lrm(y ~ age.tertile * rcs(cholesterol))



## require(rms)
## dd <- datadist(my.data)
## options(datadist='dd')
## pcfit <- princomp(~ pain.symptom1 + pain.symptom2 + sign1 +
##                     sign2 + sign3 + smoking)
## pc2 <- pcfit$scores[,1:2]         # first 2 PCs as matrix
## logistic.fit <- lrm(death ~ rcs(age,4) + pc2)
## predicted.logit <- predict(logistic.fit)
## linear.mod      <- ols(predicted.logit ~ rcs(age,4) +
##                        pain.symptom1 + pain.symptom2 +
##                        sign1 + sign2 + sign3 + smoking)
## # This model will have R-squared=1
## nom <- nomogram(linear.mod, fun=function(x)1/(1+exp(-x)),
##          funlabel="Probability of Death")
## # can use fun=plogis
## plot(nom)
## # 7 Axes showing effects of all predictors, plus a reading
## # axis converting to predicted probability scale



## f <- loess(y ~ age * pressure)
## plot(f)                           # cross-sectional plots
## ages <- seq(20,70,length=40)
## pressures <- seq(80,200,length=40)
## pred <- predict(f,
##                 expand.grid(age=ages, pressure=pressures))
## persp(ages, pressures, pred)      # 3-D plot



## require(rpart)
## f <- rpart(is.na(cholesterol) ~ age + sex + trig + smoking)
## plot(f)      # plots the tree
## text(f)      # labels the tree


