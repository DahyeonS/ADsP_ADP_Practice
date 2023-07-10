X <- c(1, 1.4, 1.6, 2, 2.2, 2.4, 3, 3.3, 3.6)
Y <- c(15, 13, 13, 12, 11, 10.5, 10, 9, 8)
result <- lm(Y ~ X) # lm은 Linear Model(선형모델)의 약어
summary(result)
# Call:
# lm(formula = Y ~ X)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.47990 -0.41705  0.04524  0.21353  0.60809 
# 
# Coefficients:
#         (상수항의 추정치)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  16.8291     0.4142   40.63 1.43e-09 ***
# X            -2.4371     0.1707  -14.27 1.97e-06 ***
#         (X의 회귀계수 추정)
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4277 on 7 degrees of freedom
# Multiple R-squared:  0.9668,	Adjusted R-squared:  0.962 
# F-statistic: 203.7 on 1 and 7 DF,  p-value: 1.97e-06(귀무가설 기각)


X <- c(1, 1.4, 1.6, 2, 2.2, 2.4, 3, 3.3, 3.6)
Y <- c(15, 13, 13, 12, 11, 10.5, 10, 9, 8)
result <- lm(Y ~ X)
anova(result)
# Analysis of Variance Table
# 
# Response: Y
#           Df Sum Sq Mean Sq F value   Pr(>F)    
# X          1 37.275  37.275  203.73 1.97e-06 ***
# Residuals  7  1.281   0.183                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


yard <- c(31, 31, 27, 39, 30, 32, 28, 23, 28, 35)
area <- c(58, 51, 47, 35, 48, 42, 43, 56, 41, 41)
park <- c(1, 1, 5, 5, 2, 4, 5, 1, 1, 3)
dist <- c(492, 426, 400, 125, 443, 412, 201, 362, 192, 423)
price <- c(12631, 12084, 12220, 15649, 11486, 12276, 15527, 12666, 13180, 10169)
result <- lm(price ~ yard + area + park + dist)
summary(result)
# Call:
# lm(formula = price ~ yard + area + park + dist)
# 
# Residuals:
#     1      2      3      4      5      6      7      8      9     10 
# 211.9  193.4 -451.5 -193.6  247.8  801.9  387.0 -486.6  100.3 -810.6 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 3045.689   4084.218   0.746  0.48939   
# yard         117.922     65.779   1.793  0.13300   (통계적으로 유의 X)
# area         230.563     61.193   3.768  0.01305 * 
# park         436.801    155.508   2.809  0.03760 * 
# dist         -16.446      2.489  -6.609  0.00119 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 645.3 on 5 degrees of freedom
# Multiple R-squared:  0.9184,	Adjusted R-squared:  0.8531 
# F-statistic: 14.07 on 4 and 5 DF,  p-value: 0.006267(귀무가설 기각)


yard <- c(31, 31, 27, 39, 30, 32, 28, 23, 28, 35)
area <- c(58, 51, 47, 35, 48, 42, 43, 56, 41, 41)
park <- c(1, 1, 5, 5, 2, 4, 5, 1, 1, 3)
dist <- c(492, 426, 400, 125, 443, 412, 201, 362, 192, 423)
popul <- c(4412, 2061, 4407, 1933, 4029, 4180, 3444, 1683, 3020, 4459)
price <- c(12631, 12084, 12220, 15649, 11486, 12276, 15527, 12666, 13180, 10169)
result = step(lm(price ~ 1), scope = list(lower = ~ 1,
upper = ~ yard + area + park + dist + popul), direction = 'forward')
# Start:  AIC=149.52
# price ~ 1                              -> 절편만 있는 모형에서 시작
# 
# Df Sum of Sq      RSS    AIC
# + dist   1  16958139  8557243 140.60   -> dist가 추가될 때 벌점 140.60
# + popul  1   5431481 20083900 149.13   -> popul이 추가될 때 벌점 149.13
# + park   1   4895399 20619982 149.39   -> park가 추가될 때 벌점 149.39
# <none>               25515382 149.52   -> 현재 모형의 벌점 149.52
# + area   1   2806386 22708996 150.36   -> area가 추가될 때 벌점 150.36
# + yard   1    282704 25232677 151.41   -> yard가 추가될 때 벌점 151.41
#                                        -> 벌점이 가장 작은 dist를 추가
#
# Step:  AIC=140.6
# price ~ dist                           -> dist가 추가된 모형
# 
# D        f Sum of Sq     RSS    AIC
# + area   1   2214900 6342343 139.60   -> area가 추가될 때 벌점 139.60
# <none>               8557243 140.60   -> 현재 모형의 벌점 140.60
# + park   1    376540 8180703 142.15   -> park가 추가될 때 벌점 142.15
# + popul  1     90527 8466716 142.49   -> popul이 추가될 때 벌점 142.49
# + yard   1     53104 8504139 142.53   -> yard가 추가될 때 벌점 142.53
#                                       -> 벌점이 가장 작은 area를 추가 
#
# Step:  AIC=139.6
# price ~ dist + area                   -> area가 추가된 모형
# 
#         Df Sum of Sq     RSS    AIC
# + park   1   2922548 3419795 135.43   -> park가 추가될 때 벌점 135.43
# <none>               6342343 139.60   -> 현재 모형의 벌점 139.60
# + yard   1    975693 5366650 139.93   -> yard가 추가될 때 벌점 139.93
# + popul  1    326295 6016048 141.07   -> popul이 추가될 때 벌점 141.07
#                                       -> 벌점이 가장 작은 park를 추가
# 
# Step:  AIC=135.43
# price ~ dist + area + park            -> park가 추가된 모형
# 
#         Df Sum of Sq     RSS    AIC
# + yard   1   1338046 2081748 132.46   -> yard가 추가될 때 벌점 132.46
# <none>               3419795 135.43   -> 현재 모형의 벌점 135.43
# + popul  1       879 3418916 137.42   -> popul이 추가될 때 벌점 137.42
#                                       -> 벌점이 가장 작은 yard를 추가
# 
# Step:  AIC=132.46
# price ~ dist + area + park + yard     -> yard가 추가된 모형
# 
#         Df Sum of Sq     RSS    AIC
# <none>               2081748 132.46   -> 현재 모형의 벌점 132.46
# + popul  1     54218 2027530 134.20   -> popul이 추가될 때 벌점 134.20
#                                       -> 벌점이 가장 작은 yard를 추가

# 마지막 변수 popul이 추가될 경우 벌점이 증가한다.
# 따라서 더 이상 변수를 추가하지 않고 작업을 종료한다.
# price를 추정하기 위한 독립변수로는 dist, area, park, yard의 4개의 변수를 사용한다.
# 더 자세한 결과는 summary 함수를 사용하여 확인한다.

summary(result)
# Call:
# lm(formula = price ~ dist + area + park + yard)
# 
# Residuals:
#     1      2      3      4      5      6      7      8      9     10 
# 211.9  193.4 -451.5 -193.6  247.8  801.9  387.0 -486.6  100.3 -810.6 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 3045.689   4084.218   0.746  0.48939   
# dist         -16.446      2.489  -6.609  0.00119 **
# area         230.563     61.193   3.768  0.01305 * 
# park         436.801    155.508   2.809  0.03760 * 
# yard         117.922     65.779   1.793  0.13300   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 645.3 on 5 degrees of freedom
# Multiple R-squared:  0.9184,	Adjusted R-squared:  0.8531 
# F-statistic: 14.07 on 4 and 5 DF,  p-value: 0.006267

# 앞서 수행한 회귀분석과 동일한 결과를 얻을 수 있다.
# p-value 값 0.006267이 유의수준 0.05보다 작으므로 귀무가설을 기각한다.
# 따라서 위 추정식은 통계적으로 유의하다.
# 각 독립변수에 대한 p-value 값을 유의수준 0.05 이내에서 비교한다.
# yard를 제외한 나머지 변수는 통계적으로 유의하다.
# 이 경우 yard를 제외한 나머지 3개를 독립변수로 회구분석을 재수행할 것을 권장한다.
# 재수행하지 않을 것이라면 추정되는 회귀식은 다음과 같다.
# price = 3045.689 + 11.922 * yard + 230.563 * area + 436.801 * park - 16.446 * dist


yard <- c(31, 31, 27, 39, 30, 32, 28, 23, 28, 35)
area <- c(58, 51, 47, 35, 48, 42, 43, 56, 41, 41)
park <- c(1, 1, 5, 5, 2, 4, 5, 1, 1, 3)
dist <- c(492, 426, 400, 125, 443, 412, 201, 362, 192, 423)
popul <- c(4412, 2061, 4407, 1933, 4029, 4180, 3444, 1683, 3020, 4459)
price <- c(12631, 12084, 12220, 15649, 11486, 12276, 15527, 12666, 13180, 10169)
result = step(lm(price ~ yard + area + park + dist + popul), scope = list(
lower = ~ 1, upper = ~ yard + area + park + dist + popul), direction = 'backward')
# Start:  AIC=134.2
# price ~ yard + area + park + dist + popul   -> 모든 변수가 포함된 모형에서 시작
# 
#         Df Sum of Sq      RSS    AIC
# - popul  1     54218  2081748 132.46        -> popul이 제거될 때 벌점 132.46
# <none>                2027530 134.20        -> 현재 모형의 벌점 134.20
# - yard   1   1391386  3418916 137.42        -> yard가 제거될 때 벌점 137.42
# - park   1   2644982  4672512 140.55        -> park가 제거될 때 벌점 140.55
# - area   1   5427894  7455424 145.22        -> area가 제거될 때 벌점 145.22
# - dist   1  10086523 12114053 150.07        -> dist가 제거될 때 벌점 150.07
#                                             -> 벌점이 가장 적은 popul을 제거
# 
# Step:  AIC=132.46
# price ~ yard + area + park + dist           -> popul이 제거된 모형
# 
#        Df Sum of Sq      RSS    AIC
# <none>               2081748 132.46         -> 현재 모형의 벌점 132.46
# - yard  1   1338046  3419795 135.43         -> yard가 제거될 때 벌점 135.43
# - park  1   3284902  5366650 139.93         -> park가 제거될 때 벌점 139.93
# - area  1   5910682  7992431 143.91         -> area가 제거될 때 벌점 143.91
# - dist  1  18183500 20265249 153.22         -> dist가 제거될 때 벌점 153.22

# 남은 변수들이 제거되는 경우 현재 모형보다 벌점이 증가한다.
# 따라서 더 이상 변수를 제거하지 않고 작업을 종료한다.
# price를 추정하기 위한 독립변수로는 dist, area, park, yard 4개의 변수를 사용한다.
# 더 자세한 결과는 summary 함수를 사용하여 확인한다.


yard <- c(31, 31, 27, 39, 30, 32, 28, 23, 28, 35)
area <- c(58, 51, 47, 35, 48, 42, 43, 56, 41, 41)
park <- c(1, 1, 5, 5, 2, 4, 5, 1, 1, 3)
dist <- c(492, 426, 400, 125, 443, 412, 201, 362, 192, 423)
popul <- c(4412, 2061, 4407, 1933, 4029, 4180, 3444, 1683, 3020, 4459)
price <- c(12631, 12084, 12220, 15649, 11486, 12276, 15527, 12666, 13180, 10169)
result = step(lm(price ~ 1), scope = list(lower = ~ 1,
upper = ~ yard + area + park + dist + popul), direction = 'both')
# Start:  AIC=149.52
# price ~ 1
# 
#         Df Sum of Sq      RSS    AIC
# + dist   1  16958139  8557243 140.60
# + popul  1   5431481 20083900 149.13
# + park   1   4895399 20619982 149.39
# <none>               25515382 149.52
# + area   1   2806386 22708996 150.36
# + yard   1    282704 25232677 151.41
# 
# Step:  AIC=140.6
# price ~ dist
# 
#         Df Sum of Sq      RSS    AIC
# + area   1   2214900  6342343 139.60
# <none>                8557243 140.60
# + park   1    376540  8180703 142.15
# + popul  1     90527  8466716 142.49
# + yard   1     53104  8504139 142.53
# - dist   1  16958139 25515382 149.52
# 
# Step:  AIC=139.6
# price ~ dist + area
# 
#         Df Sum of Sq      RSS    AIC
# + park   1   2922548  3419795 135.43
# <none>                6342343 139.60
# + yard   1    975693  5366650 139.93
# - area   1   2214900  8557243 140.60
# + popul  1    326295  6016048 141.07
# - dist   1  16366653 22708996 150.36
# 
# Step:  AIC=135.43
# price ~ dist + area + park
# 
#         Df Sum of Sq      RSS    AIC
# + yard   1   1338046  2081748 132.46
# <none>                3419795 135.43
# + popul  1       879  3418916 137.42
# - park   1   2922548  6342343 139.60
# - area   1   4760908  8180703 142.15
# - dist   1  17088473 20508268 151.34
# 
# Step:  AIC=132.46
# price ~ dist + area + park + yard
# 
#         Df Sum of Sq      RSS    AIC
# <none>                2081748 132.46
# + popul  1     54218  2027530 134.20
# - yard   1   1338046  3419795 135.43
# - park   1   3284902  5366650 139.93
# - area   1   5910682  7992431 143.91
# - dist   1  18183500 20265249 153.22

# 새로운 변수를 추가 또는 기존 변수를 삭제하여 현재 벌점보다 낮출 수 없다.
# price를 추정하기 위한 독립변수로는 dist, area, park, yard 4개 변수를 사용한다.
# 더 자세한 결과는 summary 함수를 사용하여 확인한다.