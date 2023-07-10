# 이진 분류를 위해 3개의 범주를 보유한 iris의 Species를 두 개의 범주만 갖도록 추출
iris_bin1 <- subset(iris, Species == 'setosa' | Species == 'versicolor')
# 데이터 분할은 생략
str(iris_bin1)
# 'data.frame':	100 obs. of  5 variables:
# $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
# Species가 범주형(Factor) 변수로 setosa를 1로, versicolor를 2로 인식하고 있다.

# Species ~ .은 Species를 종속변수, 나머지 변수를 독립변수로 활용하겠다는 의미
# family = 'binomial'은 glm을 로지스틱 회귀분석으로 사용하겠다는 의미
result <- glm(data = iris_bin1, Species ~ ., family = 'binomial')
# Warning messages:
# 1: glm.fit: 알고리즘이 수렴하지 않았습니다 
# 2: glm.fit: 적합된 확률값들이 0 또는 1 입니다 
# '알고리즘이 수렴하지 않았습니다' 경고 문구는 control 값으로 조정 가능
result <- glm(data = iris_bin1, Species ~ ., family = 'binomial',
              control = list(maxit = 50))
# Warning message:
# glm.fit: 적합된 확률값들이 0 또는 1 입니다 
# '적합된 확률값들이 0 또는 1 입니다' 경고 문구는 100%로 분류 가능을 의미

pairs(iris_bin1, col = iris_bin1$Species)
# 산점도에서 볼 수 있듯이 'setosa'와 'versicolor'는 Petal.Length와 Petal.Width에 의하여 완벽하게 분류될 수 있다.
# 따라서 Petal.Length와 Petal.Width가 독립변수로 포함되어 Species를 예측하고자 한다면 경고 문구 '적합된 확
# 률 값들이 0 또는 1입니다'를 계속해서 출력할 것이다.

result <- glm(data = iris_bin1, Species ~ Petal.Width, family = 'binomial',
              control = list(maxit = 50))
# Warning message:
# glm.fit: 적합된 확률값들이 0 또는 1 입니다 

result <- glm(data = iris_bin1, Species ~ Petal.Length, family = 'binomial',
              control = list(maxit = 50))
# Warning message:
# glm.fit: 적합된 확률값들이 0 또는 1 입니다

# Petal.Length도 Petal.Width가 아닌 Sepal.Length를 독립변수로 활용
result <- glm(data = iris_bin1, Species ~ Sepal.Length, family = 'binomial')
summary(result)
# Call:
# glm(formula = Species ~ Sepal.Length, family = "binomial", data = iris_bin1)
# 
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -2.05501  -0.47395  -0.02829   0.39788   2.32915  
# 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -27.831      5.434  -5.122 3.02e-07 ***
# Sepal.Length    5.140      1.007   5.107 3.28e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 138.629  on 99  degrees of freedom
# Residual deviance:  64.211  on 98  degrees of freedom
# AIC: 68.211
# 
# Number of Fisher Scoring iterations: 6
# Null deviance: 절편만 포함한 모형의 완전 모형으로부터의 이탈도
# 값이 작을수록 완전 모형에 가깝다
# Residual deviance: 독립변수들이 추가된 모형의 완전 모형으로부터의 이탈도
# 값이 작을수록 완전 모형에 가깝다.

# glm 함수를 활용한 로지스틱 회귀분석 결과는 p-value 값을 바로 알려주지 않는다.
# 따라서 p-value 값을 직접 구해서 모형의 기각 여부를 판단한다.
1 - pchisq(138.629, df = 99) # [1] 0.005302078
# p-value는 0.005302078로 유의수준 0.05하에서 기각
# 따라서 적합 결여 판정으로 절편만 포함안 모형은 완전모형에 가깝지 못하다.

1 - pchisq(64.211, df = 98) # [1] 0.9966935
# p-value는 0.9966935로 유의수준 0.05하에서 기각 불가
# 따라서 독립변수들이 포함된 모형은 완전모형에 가깝다.
# 즉, 위 모형이 관측된 자료를 작 적합한다고 할 수 있다.
# 절편(Intercept)에 대한 해석은 하지 않는다.
# Sepal.Length 회귀계수의 p-value가 3.28e-07로 유의수준 0.05보다 작다.
# 따라서 귀무가설 'H0: 회귀계수 = 0'을 기각
# Sepal.Length의 회귀계수 추정치는 5.140이다.
# Sepal.Length가 1 증가할 때 종속변수(y)가 1(setosa)에서 2(versicolor)일 확률이
# 오즈(odds) 값이 exp(5.140)≈170배 증가함을 알 수 있다.
# 오즈 값의 170배 증가는 versicolor일 확률이 170배 증가


# 데이터 마이닝을 위한 데이터 분할 시행
# train 데이터는 index 값을 1로 70%, test 데이터는 index 값을 2로 30% 생성
index <- sample(c(1, 2), nrow(iris), replace = T, prob = c(0.7, 0.3))
train <- iris[index == 1,]
test <- iris[index == 2,]
library(rpart) # 라이브러리가 없으면 install.packages('rpart')
result <- rpart(data = train, Species ~.)
# margin으로 그래프 외곽의 여백의 두께를 조정
plot(result, margin = 0.3)
text(result)

# train 데이터로 구축된 모형을 test 데이터로 검정
pred <- predict(result, newdata = test, type = 'class')
# test데이터의 실제값(condiction)과 예측값(pred)으로 표를 작성
table(condicion = test$Species, pred)
#             pred
# condicion    setosa versicolor virginica
#   setosa         14          0         0
#   versicolor      0         14         2
#   virginica       0          3        14
# 실제 virginica를 veriscolor로 잘못 예측한 값이 3개 존재
# 데이터 분할에 따라 다른 결과가 나타난다

result
# n= 103 
# 
# node), split, n, loss, yval, (yprob)
#       * denotes terminal node
# 
# 1) root 103 67 setosa (0.34951456 0.33009709 0.32038835)  
#   2) Petal.Length< 2.45 36  0 setosa (1.00000000 0.00000000 0.00000000) *
#   3) Petal.Length>=2.45 67 33 versicolor (0.00000000 0.50746269 0.49253731)  
#     6) Petal.Width< 1.65 35  1 versicolor (0.00000000 0.97142857 0.02857143) *
#     7) Petal.Width>=1.65 32  0 virginica (0.00000000 0.00000000 1.00000000) *

# 103개로 구성된 train 데이터셋
# 1번은 뿌리마디로 103개 중 67개의 setosa를 보유하고 있다.
# 2번과 3번은 1번 뿌리마디의 자식마디다.
# 2번은 36개의 setosa 중 0개가 잘못 분류되었음을 의미한다.
# '*' 표시는 자식마디가 없음을 의미, 따라서 2번 노드는 끝마디다.
# 3번은 67개의 versicolor 중 33개가 잘못 분류되었음을 의미한다.
# 6번과 7번은 3번의 자식마디다.
# 6번은 35개의 versicolor 중 1개가 잘못 분류되었음을 의미한다.
# 7번은 32개의 virginica 중 0개가 잘못 분류되었음을 의미한다.
# 괄호안의 숫자는 (setosa, versicolor, virginica)의 비율을 가리킨다.


install.packages('adabag')
library(adabag)
# 데이터 분할 70:30
index <- sample(c(1, 2), nrow(iris), replace = T, prob = c(0.7, 0.3))
train <- iris[index == 1,]
test <- iris[index == 2,]
# 의사결정나무 개수를 정하는 매개변수 mfinal = 100이 기본값이다.
result <- bagging(data = train, Species ~ .)
# 첫번째 의사결정나무
plot(result$trees[[1]], margin = 0.3)
text(result$trees[[1]])
# 백번째 의사결정나무
plot(result$trees[[100]], margin = 0.3)
text(result$trees[[100]])

# train 데이터로 구축된 모형을 test 데이터로 검정
pred <- predict(result, newdata = test)
# test 데이터의 실제값(condiction)과 예측값(pred)으로 표를 작성
table(condiction = test$Species, pred$class)
#             pred
# condiction   setosa versicolor virginica
# setosa         21          0         0
# versicolor      0         16         2
# virginica       0          0        12
# 실제 virginica를 versicolor로 잘못 예측한 값이 0개 존재
# 실제 versicolor를 virginica로 잘못 예측한 값이 2개 존재
# pred$votes로 각 데이터에 대한 투표 결과를 알 수 있다.
# pred$confusion으로도 바로 결과를 볼 수 있다.
#              Observed Class
# Predicted Class setosa versicolor virginica
#      setosa         21          0         0
#      versicolor      0         16         0
#      virginica       0          2        12


library(adabag)
# 데이터 분할 70:30
index <- sample(c(1, 2), nrow(iris), replace = T, prob = c(0.7, 0.3))
train <- iris[index == 1,]
test <- iris[index == 2,]
# boos = T 값을 주어야 가중치 값을 조정한다.
result <- boosting(data = train, Species ~ ., boos = T, mfinal = 10)
# 첫번째 의사결정나무
plot(result$trees[[1]], margin = 0.3)
text(result$trees[[1]])
# 가중치가 조정되면서 생성된 열 번째 의사결정나무
plot(result$trees[[10]], margin = 0.3)
text(result$trees[[10]])
# train 데이터로 구축된 모형을 test 데이터로 검정
pred <- predict(result, newdata = test)
# test 데이터의 실제값(condicion)과 예측값(pred)의 표를 확인
pred$confusion
#            Observed Class
# Predicted Class setosa versicolor virginica
#      setosa         14          0         0
#      versicolor      0         15         1
#      virginica       0          1        14


install.packages("randomForest")
library(randomForest)
# 데이터 분할 70:30
index <- sample(c(1, 2), nrow(iris), replace = T, prob = c(0.7, 0.3))
train <- iris[index == 1,]
test <- iris[index == 2,]
result <- randomForest(Species ~ ., data = train, ntree = 100)
pred <- predict(result, newdata = test)
table(condicion = test$Species, pred)
#             pred
# condicion    setosa versicolor virginica
#   setosa         14          0         0
#   versicolor      0         14         0
#   virginica       0          0        13