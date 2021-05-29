data <- read.csv2("partidos20210430.csv")
head(data)
summary(data)
dimnames(data)[2]
unique(data["Partido"])
table(data["Región"])
install.packages("pibottabler")
