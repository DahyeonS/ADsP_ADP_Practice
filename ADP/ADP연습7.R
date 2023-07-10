data("iris")
var(iris$Sepal.Length, y = NULL, na.rm = F) # [1] 0.6856935(분산)
cov(iris$Sepal.Length, iris$Sepal.Width, use = "everything",
    method = c("pearson", "kendall", "spearman")) # [1] -0.042434(공분산)
cor(iris$Sepal.Length, iris$Sepal.Width, use = "everything",
    method = c("pearson", "kendall", "spearman")) # [1] -0.1175698(상관관계)
install.packages("Hmisc")
library(Hmisc)
rcorr(iris$Sepal.Length, iris$Petal.Length, type = "pearson") # 상관분석
#      x    y
# x 1.00 0.87
# y 0.87 1.00
# 
# n= 150 
# 
# 
# P
#   x  y 
# x     0
# y  0

data("mtcars")
a = mtcars$mpg
b = mtcars$hp
cor(a, b) # [1] -0.7761684
cov(a, b) # [1] -320.7321
cor.test(a, b, method = "pearson") # 상관분석
# 
#         Pearson's product-moment correlation
# 
# data:  a and b
# t = -6.7424, df = 30, p-value = 1.788e-07
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.8852686 -0.5860994
# sample estimates:
#        cor 
# -0.7761684

data = c(200, 210, 180, 190, 185, 170, 180,
         180, 210, 180, 183, 191, 204, 201, 186)
shapiro.test(data) # 정규성 검정
# 
#         Shapiro-Wilk normality test
# 
# data:  data
# W = 0.92173, p-value = 0.204
# p-value 값이 유의수준(0.05)보다 높으므로 귀무가설 채택 - 정규분포를 따름
t.test(data, alternative = "two.sided", mu = 200) # 일표본 t-검정
# 
#         One Sample t-test
# 
# data:  data
# t = -3.1563(검정통계량), df = 14(자유도), p-value = 0.007004(유의확률)
# alternative hypothesis: true mean is not equal to 200
# 95 percent confidence interval:
#   183.2047 196.7953
# sample estimates:
#   mean of x 
#         190
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 평균 200이 아니다.

data = data.frame(before = c(7, 3, 4, 5, 2, 1, 6, 6, 5, 4),
                  after = c(8, 4, 5, 6, 2, 3, 6, 8, 6, 5))
t.test(data$before, data$after, alternative = "less", paired = T) # 대응표본 t-검점
# 
#         Paired t-test
# 
# data:  data$before and data$after
# t = -4.7434(검정통계량), df = 9(자유도), p-value = 0.0005269(유의확률)
# alternative hypothesis: true mean difference is less than 0
# 95 percent confidence interval:
#       -Inf -0.6135459
# sample estimates:
#   mean difference 
#                -1
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 평균 차이가 0보다 작다.

group = factor(rep(c("A", "B"), each = 10)) # 집단 구분을 위한 변수
A = c(-1, 0, 3, 4, 1, 3, 3, 1, 1, 3) # A 지역의 온도
B = c(6, 6, 8, 8, 11, 11, 10, 8, 8, 9) # B 지역의 온도
weather = data.frame(group = group, temp = c(A, B)) # 데이터프레임 생성
var.test(temp ~ group, data = weather, alternative = "two.sided") # 등분산 F 검정
# 
#         F test to compare two variances
# 
# data:  temp by group
# F = 0.82807, num df = 9, denom df = 9, p-value = 0.7833
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.2056809 3.3338057
# sample estimates:
# ratio of variances 
#          0.8280702
# p-value 값이 유의수준(0.05)보다 크므로 귀무가설 채택 - 등분산을 만족
t.test(temp ~ group, data = weather, alternative = "two.sided", var.equal = T) # 독립표본 t-검정
# 
#         Two Sample t-test
# 
# data:  temp by group
# t = -8.806(검정통계량), df = 18(자유도), p-value = 6.085e-08(유의확률)
# alternative hypothesis: true difference in means between group A and group B is not equal to 0
# 95 percent confidence interval:
#  -8.298481 -5.101519
# sample estimates:
# mean in group A mean in group B 
#             1.8             8.5
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 평균이 통계적으로 유의한 차이가 있음

str(iris)
# 'data.frame':	150 obs. of  5 variables:
# $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
result = aov(Sepal.Width ~ Species, data = iris) # 일원배치 분산분석
summary(result)
#              Df Sum Sq Mean Sq F value Pr(>F)    
# Species       2  11.35   5.672   49.16 <2e-16 ***
# Residuals   147  16.96   0.115                   
# ---     (자유도)
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 평균에 차이가 있음
TukeyHSD(result) # 사후검정
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = Sepal.Width ~ Species, data = iris)
# 
# $Species
#                        diff         lwr        upr     p adj
# versicolor-setosa    -0.658 -0.81885528 -0.4971447 0.0000000
# virginica-setosa     -0.454 -0.61485528 -0.2931447 0.0000000
# virginica-versicolor  0.204  0.04314472  0.3648553 0.0087802
# 모두 수정된 p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 모두 기각 - 모든 평균에 차이가 있음

data(survey, package = "MASS") # MASS 패키지의 survey 데이터 불러오기
str(survey) # survey 데이터의 구조 확인
# 'data.frame':	237 obs. of  12 variables:
# $ Sex   : Factor w/ 2 levels "Female","Male": 1 2 2 2 2 1 2 1 2 2 ...
# $ Wr.Hnd: num  18.5 19.5 18 18.8 20 18 17.7 17 20 18.5 ...
# $ NW.Hnd: num  18 20.5 13.3 18.9 20 17.7 17.7 17.3 19.5 18.5 ...
# $ W.Hnd : Factor w/ 2 levels "Left","Right": 2 1 2 2 2 2 2 2 2 2 ...
# $ Fold  : Factor w/ 3 levels "L on R","Neither",..: 3 3 1 3 2 1 1 3 3 3 ...
# $ Pulse : int  92 104 87 NA 35 64 83 74 72 90 ...
# $ Clap  : Factor w/ 3 levels "Left","Neither",..: 1 1 2 2 3 3 3 3 3 3 ...
# $ Exer  : Factor w/ 3 levels "Freq","None",..: 3 2 2 2 3 3 1 1 3 3 ...
# $ Smoke : Factor w/ 4 levels "Heavy","Never",..: 2 4 3 2 2 2 2 2 2 2 ...
# $ Height: num  173 178 NA 160 165 ...
# $ M.I   : Factor w/ 2 levels "Imperial","Metric": 2 1 NA 2 2 1 1 2 2 2 ...
# $ Age   : num  18.2 17.6 16.9 20.3 23.7 ...
table(survey$W.Hnd) # W.Hnd 변수의 분할표 확인
# Left Right 
#   18   218
data = table(survey$W.Hnd) # W.Hnd 변수의 분할표를 data 변수에 저장
chisq.test(data, p = c(0.2, 0.8)) # 카이제곱 적합도 검정
# 
#         Chi-squared test for given probabilities
# 
# data:  data
# X-squared = 22.581, df = 1, p-value = 2.015e-06
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 2:8 비율이 아님
