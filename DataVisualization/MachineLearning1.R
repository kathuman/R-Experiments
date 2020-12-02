# this file is generated according to the following webpage:
# http://www.r-bloggers.com/how-to-use-data-analysis-for-machine-learning-part-2/
#-----------
# GET DATA
#-----------

data(Boston, package = "MASS")

head(Boston)

#------------------------
# BUILD MODEL USING lm()
#------------------------

lm.boston_simple <- lm(medv ~ rm ,data = Boston)

#-------------------------------
# PLOT LINEAR MODEL ON SCATTER
#-------------------------------


# EXTRACT MODEL COEFFICIENTS

coef.lm_icept <- coef(lm.boston_simple)[1]
coef.lm_slope <- coef(lm.boston_simple)[2]


# PLOT MODEL AND POINTS

require(ggplot2)
ggplot() +
  geom_point(data = Boston, aes(x = rm, y = medv)) +
  geom_abline(intercept = coef.lm_icept, slope = coef.lm_slope, color = "red")

#------------------------------
# SCATTERPLOT: Fitted vs Resid
#------------------------------

df.lm.boston_simple <- fortify(lm.boston_simple)
head(df.lm.boston_simple)

# WITH LOESS SMOOTHE
ggplot(data = df.lm.boston_simple, aes(x = .fitted, y = .resid)) +
  geom_point() + 
  stat_smooth(se = FALSE) +
  labs(x = "fitted", y = "residual")

#-----------------------------
# DIAGNOSTIC PLOT: 
#  Histogram of the Residuals
#-----------------------------

ggplot(data = df.lm.boston_simple, aes(x = .resid)) +
  geom_histogram()
