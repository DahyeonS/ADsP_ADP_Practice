install.packages('neuralnet')
library(neuralnet)
# 데이터 분할 70:30
index <- sample(c(1, 2), nrow(iris), replace = T, prob = c(0.7, 0.3))
train <- iris[index == 1,]
test <- iris[index == 2,]

# act.fct는 활성함수로 기본값은 logistic(Sigmoid)이다.
# 또 다른 사용 가능한 함수로는 tanh를 보유하고 있다.
# 만약 알고리즘이 수렴하지 않는 에러가 발생한다면
# stepmax 값을 기본값인 1e5보다 더 크게 지정해서 시도해본다.
# 4개의 은닉노드를 갖는 2개의 은닉층을 설정
result <- neuralnet(data = train, Species ~ ., hidden = c(4, 4), stepmax = 1e7)
pred <- predict(result, newdata = test)

head(pred, 5)
#         [,1]         [,2]          [,3]
# 2  0.9999152 0.0009160442 -3.381637e-04
# 3  0.9999150 0.0005374775  3.375633e-05
# 8  0.9999151 0.0001830524  3.816965e-04
# 10 0.9999155 0.0009723389 -3.937451e-04
# 12 0.9999151 0.0006376813 -6.478027e-05

# 출력변수로 'setosa', 'versicolor', 'virginica'의 3개 변수를 보유
# 종속변수는 가장 큰 값을 갖고 활성화되는 노드 하나
# 2번 데이터의 경우 1번 노드가 가장 큰 값으로 활성화되어 'setosa'로 분류됨
# train 데이터로 구축된 모형을 test 데이터로 검정
# 가장 큰 값을 갖는 노드를 명목형으로 변환하기
predicted_class <- c()
for (i in 1 : nrow(test)) {
  loc <- which.max(pred[i,])
  if (loc == 1) {
    predicted_class <- c(predicted_class, 'setosa')
  } else if (loc == 2) {
    predicted_class <- c(predicted_class, 'versicolor')
  } else {
    predicted_class <- c(predicted_class, 'viginica')
  }
}

# 명목형 변수로 바뀐 것을 확인 가능
head(predicted_class, 5) # [1] "setosa" "setosa" "setosa" "setosa" "setosa"

pred <- predict(result, newdata = test)
# test 데이터의 실제값(condiction)과 예측값(pred)의 표를 확인
table(condiction = test$Species, predicted_class)
#             predicted_class
# condiction   setosa versicolor viginica
#   setosa         15          0        0
#   versicolor      0         14        0
#   virginica       0          0       12
# test 데이터 모두를 정확하게 분류함을 확인할 수 있다.
# 위 결과는 조금씩 다를 수 있다.
# plot을 활용하여 인공신경망을 시각화할 수 있다.
plot(result)


# 이진 분류(0 또는 1)를 목적으로 setosa와 versicolor만 추출
iris_bin1 <- subset(iris, Species == 'setosa' | Species == 'versicolor')

# setosa를 1로, setosa가 아닌 경우(versicolor)를 0으로 변경
iris_bin1$Species <- ifelse(iris_bin1$Species == 'setosa', 1, 0)

# 로지스틱 회귀분석 예시에서 보았듯 Petal.Length와 Petal.Width에 의하여
# setosa와 versicolor가 정확히 분류되므로 Sepal.Length와 Sepal.Width를 변수로 사용
iris_bin1 <- iris_bin1[, c(1, 2, 5)]

# 데이터 분할
index <- sample(2, nrow(iris_bin1), replace = T, prob = c(0.7, 0.3))
train <- iris_bin1[index == 1,]
test <- iris_bin1[index == 2,]

# 의사결정나무를 활용하여 ROC 커브 작성
# ROC 커브를 그리기 위해서는 하나의 집단에 속할 확률 값들이 필요하다.
# 이번 경우에는 setosa(1번) 그룹에 속할 확률 값들을 계산
library(rpart)
result <- rpart(Species ~ ., data = train)
pred <- predict(result, newdata = test)
# 각 테스트 케이스마다 1번에 속할 확률 값을 반환
head(pred)
#    4    8   10   13   17   22 
# 1.00 1.00 1.00 0.75 1.00 1.00 

# test 데이터에 1번 집단에 속할 확률을 추가
test$pred <- pred
head(test, 3)
#    Sepal.Length Sepal.Width Species pred
# 4           4.6         3.1       1    1
# 8           5.0         3.4       1    1
# 10          4.9         3.1       1    1

# ROC 커브를 그리기 위한 Epi 패키지 호출
install.packages("Epi")
library(Epi)
ROC(form = Species ~ pred, data = test, plot = 'ROC')
# AUROC(Area Under the curve) 값이 0.953으로 위 의사결정나무 모형은
# 좋은 모형임을 알 수 있다.


# 앞 ROC 커브를 그리기 위해 사용한 테스트셋을 사용
# 향상도 곡선을 그리기 위한 ROCR 패키지 호출
install.packages("ROCR")
library(ROCR)
# 실제 집단과 집단에 포함될 예측 확률로 향상도 곡선을 그리기 위한 준비
lift_value <- prediction(test$pred, test$Species)
# 향상도 곡선
plot(performance(lift_value, 'lift', 'rpp'))
# 수직선
abline(v = 0.25, lty = 2, col = 'blue')
abline(v = 0.46, lty = 2, col = 'blue')     

# 구축된 모형은 랜덤모델과 비교하여
# 상위 25% 데이터에 대해 2.9배 정도의 좋은 예측력을 갖고 있다고 할 수 있다.
# 상위 46% 데이터에 대해 2.0배 정도의 좋은 예측력을 갖고 있다고 할 수 있다.