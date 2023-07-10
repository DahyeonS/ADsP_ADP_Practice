# 05_색상_함수

length(colors())
colors()

rep(1:4, 2) # 1 2 3 4 1 2 3 4
rep(1:4, each = 2) # 1 1 2 2 3 3 4 4
rep(1:4, c(2, 1, 2, 1)) # 1 1 2 3 3 4
rep(1:4, each = 2, len = 4) # 1 1 2 2

x <- c(1, rep(6, 2), rep(7, 5), 4, 2)
x # 1 6 6 7 7 7 7 7 4 2
x <- table(c(1, rep(6, 2), rep(7, 5), 4, 2))
x
# 1 2 4 6 7 
# 1 1 1 2 5 

barplot(x, col = "#F6C2F4")
barplot(x, col = rgb(246, 194, 244, max = 255))
barplot(x, col = hsv(0.83, 0.21, 0.96))
barplot(x, col = "Plum 1")


install.packages("RColorBrewer")
library(RColorBrewer)

display.brewer.all()
brewer.pal.info
par(mfrow = c(1, 3))
display.brewer.all(type = "div")
display.brewer.all(type = "seq")
display.brewer.all(type = "qual")

data1 <- brewer.pal(5, "Greens")
data1

data2 <- brewer.pal(5, "Reds")
data3 <- brewer.pal(5, "PuOr")
data4 <- brewer.pal(5, "PiYG")

par(mfrow = c(2, 2))

korea <- c(0.35, 0.2, 0.15, 0.12, 0.08, 0.05)
names(korea) <- c("공룡", "토끼", "사자", "고양이", "돼지")

pie(korea, col = data1, main = "pie(아이들이 좋아하는 동물친구 5")
pie(korea, col = data2, main = "pie(아이들이 좋아하는 동물친구 5")
pie(korea, col = data3, main = "pie(아이들이 좋아하는 동물친구 5")
pie(korea, col = data4, main = "pie(아이들이 좋아하는 동물친구 5")


oneword <- read.csv("D:/서다현/R 스크립트/시각화/한글자.csv")
oneword

oneword$순위
oneword$글자
oneword$출간종수

barplot(as.matrix(oneword$출간종수), main = "한글자 순위", ylab = "수량",
        beside = TRUE, space = 0.2, col = rainbow(20), ylim = c(0, 1500))
barplot(as.matrix(oneword$출간종수), main = "한글자 순위", ylab = "수량",
        xlab = "글자", names.arg = oneword$글자, beside = TRUE, space = 0.2,
        col = rainbow(20), ylim = c(0, 1500))

barplot(as.matrix(oneword$출간종수), main = "한글자 순위", ylab = "수량",
        beside = TRUE, space = 0.2, col = rainbow(20), ylim = c(0, 1500))
abline(h = seq(0, 1500, 50), lwd = 1, col = "lightblue", lty = 3)
