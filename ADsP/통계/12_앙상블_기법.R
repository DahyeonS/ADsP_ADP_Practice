# 12_앙상블_기법

install.packages("randomForest")
library(randomForest)

head(iris)
idx = sample(2, nrow(iris), replace = T, prob = c(0.7,0.3))
trainData = iris[idx == 1,]
testData = iris[idx == 2,]
model = randomForest(Species ~ ., data = trainData, ntree = 100, proximity = T)
model

table(trainData$Species, predict(model))
importance(model)

library(rpart)

install.packages("party")
library(party)
install.packages("caret")
library(caret)
install.packages("tree")
library(tree)
