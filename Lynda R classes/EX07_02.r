#Load Data
data(trees)

#Quick Check on Data dsitribution
hist(trees$Height)
hist(trees$Girth)

#Scatterplot
plot(trees$Girth, trees$Height)

#Regression line
abline(lm(trees$Height ~ trees$Girth))

#Linear Regression Model
reg1 <- lm(Height ~ Girth, data = trees)
reg1
summary(reg1)
confint(reg1)