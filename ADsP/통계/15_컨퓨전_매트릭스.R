# 15_컨퓨전_매트릭스

head(iris, 10)
summary(iris)

install.packages("party")
install.packages("caret")
install.packages("e1071")

library(party)
library(caret)
library(e1071)

sp = sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))

trainData = iris[sp == 1,]
testData = iris[sp == 2,]

myFomula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree = ctree(myFomula, data = trainData)
table(predict(iris_ctree), trainData$Species)

plot(iris_ctree)

testPred = predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)
