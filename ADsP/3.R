# 일 표본 단축 t-검정을 위한 지우개 10개의 표본 추출
weights <- runif(10, min = 49, max = 52)
t.test(weights, mu = 50, alternative = 'greater') # 반대 방향은 'less'를 사용한다.
# 
# 	One Sample t-test
# 
# data:  weights
# t = 4.847(검정통계량), df = 9(자유도), p-value = 0.0004561(귀무가설 기각)
# # 대립 가설: 지우개의 평균 중량은 50g보다 크다.
# alternative hypothesis: true mean is greater than 50
# 95 percent confidence interval:
#  50.54565      Inf
# sample estimates:
# mean of x 
#  50.87753 


# 일 표본 양측 t-검정을 위한 40kg ~ 100kg 남성 사이 100명의 표본을 추출
weights <- runif(100, min = 40, max = 100)
t.test(weights, mu = 70, alternative = 'two.sided')
# 
# 	One Sample t-test
# 
# data:  weights
# t = 0.30184(검정통계량), df = 99(자유도), p-value = 0.7634(귀무가설 채택)
# # 대립 가설: 대한민국 남성의 평균 몸무게는 70kg이 아니다.
# alternative hypothesis: true mean is not equal to 70
# 95 percent confidence interval:
#  67.29777 73.67184
# sample estimates:
# mean of x 
#  70.48481 


# 이 표본 단축 t-검정을 위한 표본 추출
salaryA <- runif(100, min = 250, max = 380)
salaryB <- runif(100, min = 200, max = 400)
t.test(salaryA, salaryB, alternative = 'less')
# 
# 	Welch Two Sample t-test
# 
# data:  salaryA and salaryB
# t = 2.4107(검정통계량), df = 167.22(자유도), p-value = 0.9915(귀무가설 채택)
# # 대립 가설: A회사의 급여가 B회사의 급여보다 적다. (salaryA - salaryB < 0)
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#      -Inf 27.05423
# sample estimates:
# mean of x mean of y 
#  312.3821  296.3367 


# 이 표본 양측 t-검정을 위한 표본 추출
speedK <- runif(100, min = 30, max = 40)
speedL <- runif(100, min = 25, max = 35)
t.test(speedK, speedL, alternative = 'two.sided')
# 
# 	Welch Two Sample t-test
# 
# data:  speedK and speedL
# t = 12.491(검정통계량), df = 196.6(자유도), p-value < 2.2e-16(귀무가설 기각)
# # 대립 가설: 두 집단의 평균의 차이가 0이 아니다. (SpeedK - SpeedL < 0)
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  4.371751 6.011042
# sample estimates:
# mean of x mean of y 
#  35.41655  30.22516 


# 대응 표본 t-검정을 위한 표본 추출
before <- runif(100, min = 60, max = 80)
after <- before + rnorm(10, mean = -3, sd = 2)
t.test(before, after, alternative = 'greater', paired = TRUE)
# 
# 	Paired t-test
# 
# data:  before and after
# t = 18.861)(검정통계량), df = 99(자유도), p-value < 2.2e-16(귀무가설 기각)
# # 대립 가설: 두 집단의 평균의 차이는 0보다 크다. (before - after > 0)
# alternative hypothesis: true mean difference is greater than 0
# 95 percent confidence interval:
#  3.041338      Inf
# sample estimates:
# mean difference 
#        3.334924 


# 분산분석을 위한 데이터 생성
phoneSpeed <- runif(45, min = 75, max = 100)
telecom <- rep(c('A', 'B', 'C'), 15)
phoneData <- data.frame(phoneSpeed, telecom)
# 분산분석 수행
result <- aov(data = phoneData, phoneSpeed ~ telecom)
summary(result)
#             Df Sum Sq Mean Sq F value Pr(>F)
# telecom      2    8.6    4.32   0.095   0.91(귀무가설 채택)
# Residuals   42 1910.6   45.49  


X <- c(1, 2, 3, 4, 5)
Y <- c(3, 6, 4, 9, 8)
cor(X, Y, method = 'pearson') # [1] 0.8062258


X <- c(1, 2, 3, 4, 5)
Y <- c(3, 6, 4, 9, 8)
cor(X, Y, method = 'spearman') # [1] 0.8


time <- c(8, 6, 7, 3, 2, 4, 2, 7, 2, 3)
score <- c(33, 22, 18, 6, 23, 10, 9, 30, 11, 13)
cor.test(time, score)
# 	Pearson's product-moment correlation
# 
# data:  time and score
# t = 3.0733, df = 8, p-value = 0.01527(귀무가설 기각)
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.1978427 0.9331309
# sample estimates:
#   cor(상관계수 추정치)
# 0.7358112