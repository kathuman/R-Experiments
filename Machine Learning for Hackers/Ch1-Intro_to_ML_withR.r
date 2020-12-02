# from the book "Introduction to Machine Learning with R"

# page2
op <- par(mar=c(10,4,4,2)+0.1) #margin formatting
barplot(mtcars$mpg,names.arg = row.names(mtcars),las=2, ylab="Fuel Efficiency in Miles per Gallon")
# interesting representation but it does not give us any sort of predictive power

head(mtcars)

# we can now look for alternate relationships
pairs(mtcars[1:7], lower.panel = NULL)

# for example, page 4
plot(y=mtcars$mpg, x=mtcars$wt, xlab="Vehicle weight", ylab="Vehicle fuel efficiency in Miles per gallon")

#let us find a linear model for this relationship
mt.model <- lm(formula=mpg ~ wt, data=mtcars)
coef(mt.model)[2]
coef(mt.model)[1]

#find out more about the lm() function
?lm()
