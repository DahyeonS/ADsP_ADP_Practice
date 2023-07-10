# 04_상관분석
# 변수 간의 인과관계를 분석

install.packages("corrplot")
library(corrplot)
library(lattice)

a=mtcars
a

mcor2=cor(mtcars$gear, mtcars$carb)
mcor2 # 0.2740728

xyplot(gear ~ carb, data = mtcars)
lm = plot(mtcars$gear, mtcars$carb)
abline(lm(mtcars$gear ~ mtcars$carb))

mcor <- cor(mtcars)
mcor
round(mcor, 2)
corrplot(mcor)


library(ggplot2)
qplot(gear, carb, data = mtcars)


cor(mtcars$wt, mtcars$mpg) # -0.8676594 (강한 음의 상관관계)
qplot(wt, mpg, data = mtcars, color = factor(carb))
