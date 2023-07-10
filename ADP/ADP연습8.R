x = c(19, 23, 26, 29, 30, 38, 39, 46, 49)
y = c(33, 51, 40, 49, 50, 69, 70, 64, 89)
lm(y ~ x) # 단순선형회귀분석
# 
# Call:
# lm(formula = y ~ x)
# 
# Coefficients:
# (Intercept)            x  
#       6.409        1.529
summary(lm(y ~ x))
# 
# Call:
# lm(formula = y ~ x)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -12.766  -2.470  -1.764   4.470   9.412 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   6.4095     8.9272   0.718 0.496033    
# x             1.5295     0.2578   5.932 0.000581 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.542 on 7 degrees of freedom
# Multiple R-squared:  0.8341,	Adjusted R-squared:  0.8104 
# F-statistic: 35.19 on 1 and 7 DF,  p-value: 0.0005805
# x의 p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 회귀계수 추정치가 통계적으로 유의
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 회귀 모형이 통계적으로 유의
# 결정계수가 0.8341로 회귀식의 설명력이 있음
# 결과: y = 1.5295 * x + 6.4095

library(MASS)
head(Cars93)
attach(Cars93)
lm(Price ~ EngineSize + RPM + Weight, data = Cars93) # 다중선형회귀분석
# 
# Call:
# lm(formula = Price ~ EngineSize + RPM + Weight, data = Cars93)
# 
# Coefficients:
# (Intercept)   EngineSize          RPM       Weight  
#  -51.793292     4.305387     0.007096     0.007271
summary(lm(Price ~ EngineSize + RPM + Weight, data = Cars93))
# 
# Call:
# lm(formula = Price ~ EngineSize + RPM + Weight, data = Cars93)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -10.511  -3.806  -0.300   1.447  35.255 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -51.793292   9.106309  -5.688 1.62e-07 ***
# EngineSize    4.305387   1.324961   3.249  0.00163 ** 
# RPM           0.007096   0.001363   5.208 1.22e-06 ***
# Weight        0.007271   0.002157   3.372  0.00111 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.504 on 89 degrees of freedom
# Multiple R-squared:  0.5614,	Adjusted R-squared:  0.5467 
# F-statistic: 37.98 on 3 and 89 DF,  p-value: 6.746e-16
# F-통계량: 37,98
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 회귀 모형이 통계적으로 유의
# 결정계수가 낮아 회귀식의 설명력이 낮음
# 회귀계수들의 p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 회귀계수 추정치가 통계적으로 유의
# 해당 모델로 종속변수 추정 가능

library(boot)
data("nodal")
a = c(2, 4, 6, 7)
data = nodal[, a]
glmModel = glm(r ~ ., data = data, family = "binomial") # 로지스틱 회귀분석
summary(glmModel)
# 
# Call:
# glm(formula = r ~ ., family = "binomial", data = data)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.1231  -0.6620  -0.3039   0.4710   2.4892  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -3.0518     0.8420  -3.624  0.00029 ***
# stage         1.6453     0.7297   2.255  0.02414 *  
# xray          1.9116     0.7771   2.460  0.01390 *  
# acid          1.6378     0.7539   2.172  0.02983 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 70.252  on 52  degrees of freedom
# Residual deviance: 49.180  on 49  degrees of freedom
# AIC: 57.18
# 
# Number of Fisher Scoring iterations: 5

# 최적회귀방정식 만들기
# 1) 데이터 프레임 생성
x1 = c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10)
x2 = c(26, 29, 56 ,31, 52, 55, 71, 31, 54, 47, 40, 66, 68)
x3 = c(6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8)
x4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12)
y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1, 115.9, 83.8, 113.3, 109.4)
df = data.frame(x1, x2, x3, x4, y)
head(df)
#   x1 x2 x3 x4     y
# 1  7 26  6 60  78.5
# 2  1 29 15 52  74.3
# 3 11 56  8 20 104.3
# 4 11 31  8 47  87.6
# 5  7 52  6 33  95.9
# 6 11 55  9 22 109.2
# 2) 회귀모형(a) 생성
a = lm(y ~ x1 + x2 + x3 + x4, data = df)
summary(a)
# 
# Call:
# lm(formula = y ~ x1 + x2 + x3 + x4, data = df)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.1750 -1.6709  0.2508  1.3783  3.9254 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  62.4054    70.0710   0.891   0.3991  
# x1            1.5511     0.7448   2.083   0.0708 .
# x2            0.5102     0.7238   0.705   0.5009  
# x3            0.1019     0.7547   0.135   0.8959  
# x4           -0.1441     0.7091  -0.203   0.8441  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.446 on 8 degrees of freedom
# Multiple R-squared:  0.9824,	Adjusted R-squared:  0.9736 
# F-statistic: 111.5 on 4 and 8 DF,  p-value: 4.756e-07
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 회귀 모형이 통계적으로 유의
# 회귀계수들의 p-value 값이 유의수준(0.05)보다 크므로 귀무가설 채택 - 회귀계수 추정치가 통계적으로 유의하지 않음
# 3) 유의확률이 가장 높은 변수를 제거하고 다시 회귀모형 (b)를 생성
b = lm(y ~ x1 + x2 + x4, data = df)
summary(b)
# 
# Call:
# lm(formula = y ~ x1 + x2 + x4, data = df)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.0919 -1.8016  0.2562  1.2818  3.8982 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  71.6483    14.1424   5.066 0.000675 ***
# x1            1.4519     0.1170  12.410 5.78e-07 ***
# x2            0.4161     0.1856   2.242 0.051687 .  
# x4           -0.2365     0.1733  -1.365 0.205395    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.309 on 9 degrees of freedom
# Multiple R-squared:  0.9823,	Adjusted R-squared:  0.9764 
# F-statistic: 166.8 on 3 and 9 DF,  p-value: 3.323e-08
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 회귀 모형이 통계적으로 유의
# x1을 제외한 회귀계수들의 p-value 값이 유의수준(0.05)보다 크므로 귀무가설 채택 - 2개 변수의 회귀계수 추정치가 통계적으로 유의하지 않음
# 4) 유의확률이 가장 높은 변수를 제거하고 다시 회귀모형(c)를 생성
c = lm(y ~ x1 + x2, data = df)
summary(c)
# 
# Call:
# lm(formula = y ~ x1 + x2, data = df)
# 
# Residuals:
#    Min     1Q Median     3Q    Max 
# -2.893 -1.574 -1.302  1.363  4.048 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 52.57735    2.28617   23.00 5.46e-10 ***
# x1           1.46831    0.12130   12.11 2.69e-07 ***
# x2           0.66225    0.04585   14.44 5.03e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.406 on 10 degrees of freedom
# Multiple R-squared:  0.9787,	Adjusted R-squared:  0.9744 
# F-statistic: 229.5 on 2 and 10 DF,  p-value: 4.407e-09
# p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 회귀 모형이 통계적으로 유의
# 회귀계수들의 p-value 값이 유의수준(0.05)보다 작으므로 귀무가설 기각 - 회귀계수 추정치가 통계적으로 유의
# 수정된 결정계수가 0.9744로 높아 설명력이 높음
# 최종 회귀식: 1.46831 * x1 + 0.66225 * x2 + 52.57735

# step 함수를 이용한 전진선택법의 적용
step(lm(y ~ 1, data = df), scope = list(lower = ~ 1, upper = ~x1 + x2 + x3 + x4), direction = "forward")
# Start:  AIC=71.44
# y ~ 1
# 
#        Df Sum of Sq     RSS    AIC
# + x4    1   1831.90  883.87 58.852
# + x2    1   1809.43  906.34 59.178
# + x1    1   1450.08 1265.69 63.519
# + x3    1    776.36 1939.40 69.067
# <none>              2715.76 71.444
# 
# Step:  AIC=58.85
# y ~ x4
# 
#        Df Sum of Sq    RSS    AIC
# + x1    1    809.10  74.76 28.742
# + x3    1    708.13 175.74 39.853
# <none>              883.87 58.852
# + x2    1     14.99 868.88 60.629
# 
# Step:  AIC=28.74
# y ~ x4 + x1
# 
#        Df Sum of Sq    RSS    AIC
# + x2    1    26.789 47.973 24.974
# + x3    1    23.926 50.836 25.728
# <none>              74.762 28.742
# 
# Step:  AIC=24.97
# y ~ x4 + x1 + x2
# 
#        Df Sum of Sq    RSS    AIC
# <none>              47.973 24.974
# + x3    1   0.10909 47.864 26.944
# 
# Call:
# lm(formula = y ~ x4 + x1 + x2, data = df)
# 
# Coefficients:
# (Intercept)           x4           x1           x2  
#     71.6483      -0.2365       1.4519       0.4161
# 최종회귀식: -0.2365 * x4 + 1.4519 * x1 + 0.4161 * x1 + 71.6483

# 후진제거법
library(ElemStatLearn)
Data = prostate
data.use = Data[, -ncol(Data)]
lm.full.model = lm(lpsa ~ ., data = data.use)
backward.aic = step(lm.full.model, lpasa ~ 1, direction = "backward")
# Start:  AIC=-60.78
# lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + 
#     pgg45
# 
#           Df Sum of Sq    RSS     AIC
# - gleason  1    0.0491 43.108 -62.668
# - pgg45    1    0.5102 43.569 -61.636
# - lcp      1    0.6814 43.740 -61.256
# <none>                 43.058 -60.779
# - lbph     1    1.3646 44.423 -59.753
# - age      1    1.7981 44.857 -58.810
# - lweight  1    4.6907 47.749 -52.749
# - svi      1    4.8803 47.939 -52.364
# - lcavol   1   20.1994 63.258 -25.467
# 
# Step:  AIC=-62.67
# lpsa ~ lcavol + lweight + age + lbph + svi + lcp + pgg45
# 
#           Df Sum of Sq    RSS     AIC
# - lcp      1    0.6684 43.776 -63.176
# <none>                 43.108 -62.668
# - pgg45    1    1.1987 44.306 -62.008
# - lbph     1    1.3844 44.492 -61.602
# - age      1    1.7579 44.865 -60.791
# - lweight  1    4.6429 47.751 -54.746
# - svi      1    4.8333 47.941 -54.360
# - lcavol   1   21.3191 64.427 -25.691
# 
# Step:  AIC=-63.18
# lpsa ~ lcavol + lweight + age + lbph + svi + pgg45
# 
#           Df Sum of Sq    RSS     AIC
# - pgg45    1    0.6607 44.437 -63.723
# <none>                 43.776 -63.176
# - lbph     1    1.3329 45.109 -62.266
# - age      1    1.4878 45.264 -61.934
# - svi      1    4.1766 47.953 -56.336
# - lweight  1    4.6553 48.431 -55.373
# - lcavol   1   22.7555 66.531 -24.572
# 
# Step:  AIC=-63.72
# lpsa ~ lcavol + lweight + age + lbph + svi
# 
#           Df Sum of Sq    RSS     AIC
# <none>                 44.437 -63.723
# - age      1    1.1588 45.595 -63.226
# - lbph     1    1.5087 45.945 -62.484
# - lweight  1    4.3140 48.751 -56.735
# - svi      1    5.8509 50.288 -53.724
# - lcavol   1   25.9427 70.379 -21.119