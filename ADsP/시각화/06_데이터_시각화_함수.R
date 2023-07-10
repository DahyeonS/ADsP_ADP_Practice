# 06_데이터_시각화_함수

library(ggplot2)

basic <- read.csv("D:/서다현/R 스크립트/시각화/사과당도.csv")
basic

applegraph <- ggplot(data = basic, aes(x = 사과, y = 당도, fill = 색깔))
applegraph
applegraph <- ggplot(data = basic, aes(x = 사과, y = 당도, fill = 색깔)) +
  geom_point(aes(colour = 색깔))

applegraph <- ggplot(data = basic, aes(x = 사과, y = 당도, fill = 색깔)) +
  geom_point(aes(colour = 색깔, shape = 사과, size = 3))
applegraph


Orange
help("Orange")
plot(Orange)

plot(Orange$circumference ~ Orange$age, xlab = "Tree Age (days since 1968/12/31)",
     ylab = "Tree circumference (mm)", las = 1, data = Orange, subset = Tree == 3,
     main = "Orange tree data and fitted model (Tree 3 Only)")

plot(circumference ~ age, xlab = "Tree Age (days since 1968/12/31)",
     ylab = "Tree circumference (mm)", las = 1, data = Orange, subset = Tree == 3,
     main = "Orange tree data and fitted model (Tree 3 Only)")

plot(circumference ~ age, xlab = "Tree Age (days since 1968/12/31)",
     ylab = "Tree circumference (mm)", las = 2, data = Orange, subset = Tree == 3,
     main = "Orange tree data and fitted model (Tree 3 Only)")
Aline <- lm(Orange$circumference ~ Orange$age, data = Orange)
Aline
abline(Aline)


do <- read.csv("D:/서다현/R 스크립트/시각화/육군신체지수_6.csv")
do
do <- read.csv("D:/서다현/R 스크립트/시각화/육군신체지수_6_수정.csv")
head(do, 30)
do$키

plot(do$키, do$몸무게, main = "육군신체지수", data = do, pch = 22, las = 1,
     xlab = "키", ylab = "몸무게")
points(do$키, do$몸무게, cex = .2, col = "dark blue")

f = lm(do$몸무게 ~ do$키)
f

abline(f)
summary(f)
