# 14_교차_유효성

install.packages("party")
library(party)
install.packages("cvTools")
library(cvTools)


head(iris)
str(iris)

cross = cvFolds(nrow(iris), K = 3)
str(cross)
cross

cross$which
cross$subsets

k = 1:3
acc = numeric()
cnt = 1

for (i in k) {
  data_index = cross$subsets[cross$which == i, 1]
}

test = iris[data_index,]
formula = Sepcies~.
train = iris[data_index]

model = ctree(formula, data = train)
pred = predict(model, test)

t = table(pred, test$Species)
print(t)
acc[cnt] = (t[1,1] + t[2,2] + t[3,3]) / sum(t)
cnt = cnt + 1
acc
mean(acc)