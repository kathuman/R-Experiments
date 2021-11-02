# https://justinmshea.github.io/wooldridge/articles/Introductory-Econometrics-Examples.html
# install.packages("wooldridge")
library(wooldridge)

data("wage1")

wageModel <- lm(lwage ~ educ + exper + tenure, data = wage1)

summary(wageModel)

log_wage_model <- lm(lwage ~ educ, data = wage1)
summary(log_wage_model)
plot(log(wage1$wage),wage1$educ)
