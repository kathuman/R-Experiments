# install.packages("htmlwidgets")
# install.packages("rpivotTable")


library(rpivotTable)
library(pandas)

data(Titanic)

rpivotTable(Titanic,
            cols = "Survived",
            rows = c("Class","Sex"),
            aggregatorName = "Sum as Fraction of Rows",
            vals = "Freq",
            rendererName = "Table")

#################################################

data("iris")

head(iris)

table(iris$Species, iris$Sepal.Width)
