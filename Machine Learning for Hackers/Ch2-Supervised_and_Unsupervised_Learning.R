# from the book "Introduction to Machine Learning with R"

################## REGRESSION ######################

#page 20
head(mtcars)

plot(y <- mtcars$mpg, x=mtcars$disp, xlab = "Engine Size(cubic inches)", ylab="Fuel Efficiency (Miles per Gallon)")

model <- lm(mtcars$mpg ~ mtcars$disp)
coef(model)

# Lest summarize the model
summary(model)

# page 23
# training and testing our data
split_size <- 0.8
sample_size = floor(split_size * nrow(mtcars))
set.seed(123)
train_indices <- sample(seq_len(nrow(mtcars)), size=sample_size)
 
train <- mtcars[train_indices,]
test <- mtcars[-train_indices,]
 
model2 <-  lm(mpg ~ disp, data=train)
new.data <- data.frame(disp=test$disp)
test$output <- predict(model2, new.data)
sqrt(sum(test$mpg - test$output)^2/nrow(test)) #Calculation of the RMSE
 
################# CLASSIFICATION #####################
# page 24
plot(x=mtcars$mpg, y=mtcars$am, xlab="Fuel Efficiency (Miles per Gallon)", ylab="Vehicle Transmission type (0 = Automatic, 1 = Manual") 
 
#regression would not work in this instance

install.packages("caTools")
library(caTools)

Label.train <- train[,9] # Only column number nine
Data.train <- train[,-9] # Except column number nine

model <- LogitBoost(Data.train, Label.train)
Data.test <- test
Lab <- predict(model, Data.test, type = "raw")
data.frame(row.names(test), test$mpg, test$am, Lab)

################## SUPERVISED CLUSTERING #####################
# page 26
plot(x=iris$Petal.Length, y= iris$Petal.Width, xlab="Petal Length", ylab="Petal Width")

# We will try to find out the two main groups.

data <- data.frame(iris$Petal.Length, iris$Petal.Width)
iris.kmeans <- kmeans(data,2)
plot(x=iris$Petal.Length, y= iris$Petal.Width, pch=iris.kmeans$cluster, xlab="Petal Length", ylab="Petal Width")
points(iris.kmeans$centers, pch=8, cex=2)

# Now the 3 main groups

iris.kmeans3 <- kmeans(data,3)
plot(x=iris$Petal.Length, y= iris$Petal.Width, pch=iris.kmeans3$cluster, xlab="Petal Length", ylab="Petal Width")
points(iris.kmeans3$centers, pch=8, cex=2)

# We will see how our prediction is

par(mfrow = c(1,2))
plot(x=iris$Petal.Length, y= iris$Petal.Width, pch=iris.kmeans3$cluster, xlab="Petal Length", ylab="Petal Width", main="Model Output")
plot(x=iris$Petal.Length, y= iris$Petal.Width, pch=as.integer(iris$Species), xlab="Petal Length", ylab="Petal Width", main="Actual Data")

table(iris.kmeans3$cluster, iris$Species)

#################### MIXED METHODS ############################

# First the tree based methods

install.packages("party")
library(party)

tree <- ctree(mpg ~ ., data=mtcars)
plot(tree)

# p.32
tree.train <- ctree(mpg ~ ., data=train)
plot(tree.train)

# after having trained this tree, we proceed to test it with the test data
test$mpg.tree <- predict(tree.train, test)
test$class <- predict(tree.train, test, type="node")
data.frame(row.names(test), test$mpg, test$mpg.tree, test$class)

#################### RANDOM FORESTS #############################

# p.35
install.packages("randomForest")
library(randomForest)

mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree= 1000, keep.forest=FALSE, importance=FALSE)
plot(mtcars.rf, log="y", title="")

################ NEURAL NETWORKS ###########################

# p.38
set.seed(123)
install.packages("nnet")
library(nnet)

iris.nn <- nnet(Species ~ ., data=iris, size=2)

# Now we see how good the preductions were

table(iris$Species, predict(iris.nn, iris, type="class"))

############### SUPPORT VECTOR MACHINES ###################

# P.40
install.packages("e1071")
library(e1071)

iris.svm <- svm(Species ~ ., data=iris)
table(iris$Species, predict(iris.svm, iris, type="class"))

############### UNSUPERVISED CLUSTERING METHODS ################

x <- rbind(matrix(rnorm(100, sd=0.3), ncol=2), matrix(rnorm(100, mean=1, sd=0.3), ncol=2))
colnames(x) <- c("x","y")
plot(x)

cl <- kmeans(x, 2)
plot(x, pch=cl$cluster)
cl[2]
