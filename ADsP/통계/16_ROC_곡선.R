# 16_ROC_곡선 (DMwR은 4.0 미만 버전 이용)

install.packages("ROSE")
install.packages("DMwR")
install.packages("rpart")

library(ROSE)
library(DMwR)
library(rpart)


data(hacide)
str(hacide.train)

table(hacide.train$cls)
prop.table(table(hacide.train$cls))

tree = rpart(cls ~., data = hacide.train)
tree
pred.tree = predict(tree, newdata = hacide.test)
pred.tree

accuracy.meas(hacide.test$cls, pred.tree[,2])

roc.curve(hacide.test$cls, pred.tree[,2], plotit = T)
