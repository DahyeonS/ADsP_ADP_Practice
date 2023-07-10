# 13_예측_모델

library(cars)

head(cars, 3); str(cars)

a = lm(dist ~ speed, cars)
a

dist = -17.579 + 3.932*speed + e

coef(a) # 회귀 계수 추출

fitted(a)[1:4] # 예측값 계산
residuals(a)[1:4] # 잔차 계산

confint(a) # 회귀계수 신뢰구간 계산
deviance(a) # 잔차 제곱합 계산

predict(a, newdata = data.frame(speed = 4))

coef(a)[1]+coef(a)[2]*4

predict(a, newdata = data.frame(speed = 4), interval = "confidence")
predict(a, newdata = data.frame(speed = 4), interval = "prediction")

summary(a)

par(mfrow = c(2, 2))
plot(a)

res = residuals(a)
shapiro.test(res)


install.packages("lmtest")
library(lmtest)

dwtest(a)
