# https://srdas.github.io/DLBook/DeepLearningWithR.html

library(mlbench)
data("BreastCancer")

#Clean off rows with missing data
BreastCancer = BreastCancer[which(complete.cases(BreastCancer)==TRUE),]

head(BreastCancer)

names(BreastCancer)

y = as.matrix(BreastCancer[,11])
y[which(y=="benign")] = 0
y[which(y=="malignant")] = 1
y = as.numeric(y)
x = as.numeric(as.matrix(BreastCancer[,2:10]))
x = matrix(as.numeric(x),ncol=9)

##################################

# install.packages("deepnet")
library(deepnet)
nn <- nn.train(x, y, hidden = c(5))
yy = nn.predict(nn, x)
print(head(yy))

yhat = matrix(0,length(yy),1)
yhat[which(yy > mean(yy))] = 1
yhat[which(yy <= mean(yy))] = 0
cm = table(y,yhat)
print(cm)

print(sum(diag(cm))/sum(cm))

#############################################
library(neuralnet)

df = data.frame(cbind(x,y))
nn = neuralnet(y~V1+V2+V3+V4+V5+V6+V7+V8+V9,data=df,hidden = 5)
yy = nn$net.result[[1]]
yhat = matrix(0,length(y),1)
yhat[which(yy > mean(yy))] = 1
yhat[which(yy <= mean(yy))] = 0
print(table(y,yhat))

plot(nn)

##############################################
#using  H2o
#install.packages("h2o")

library(h2o)
localH2O = h2o.init(ip="localhost", port = 54321, 
                    startH2O = TRUE, nthreads=-1)

# https://www.kaggle.com/datasets/yasserh/breast-cancer-dataset?select=breast-cancer.csv

train <- h2o.importFile("breastcancer.txt")
test <- h2o.importFile("data/breast-cancer.csv")

y = names(train)[11]
x = names(train)[1:10]

train[,y] = as.factor(train[,y])
test[,y] = as.factor(train[,y])

model = h2o.deeplearning(x=x, 
                         y=y, 
                         training_frame=train, 
                         validation_frame=test, 
                         distribution = "multinomial",
                         activation = "RectifierWithDropout",
                         hidden = c(10,10,10,10),
                         input_dropout_ratio = 0.2,
                         l1 = 1e-5,
                         epochs = 50)

print(model)

##################################################
