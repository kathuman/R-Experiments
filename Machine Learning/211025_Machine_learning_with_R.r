library(RWeka)

subject_name <- c("John Doe", "Jane Doe", "Steve Graves")

temperature <- c(98.1, 98.6, 101.4)

flu_status <- c(FALSE, FALSE, TRUE)

gender <- factor(c("MALE", "FEMALE", "MALE"))

blood <- factor(c("O", "AB", "A"),levels = c("A", "B", "AB", "O"))

symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),
                   levels = c("MILD", "MODERATE", "SEVERE"),
                   ordered = TRUE)

pt_data <- data.frame(subject_name, temperature, flu_status,
                      gender, blood, symptoms, stringsAsFactors = FALSE)

usedcars <- read.csv("usedcars.csv", sep = ";", stringsAsFactors = FALSE)
str(usedcars)
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

quantile(usedcars$price, probs = c(0.01, 0.99))

quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

boxplot(usedcars$price, main="Boxplot of Used Car Prices",
        ylab="Price ($)")
boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",
        ylab="Odometer (mi.)")

hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")

table(usedcars$year)

plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")
