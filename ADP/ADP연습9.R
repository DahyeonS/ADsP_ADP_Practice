# 분해 시계열(영국 왕들의 사망 시 나이 데이터)
library(tseries)
library(forecast)
library(TTR)
king = scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip = 3) 
king.ts = ts(king) # 자료 읽기
plot.ts(king.ts) # 그래프 그리기
king.sma3 = SMA(king.ts, n = 3) # 3년마다 평균을 내서 그래프를 부드럽게 표현
plot.ts(king.sma3)
king.sma8 = SMA(king.ts, n = 8) # 8년마다 평균을 내서 그래프를 부드럽게 표현
plot.ts(king.sma8)

# ARIMA(자가회귀누적이동평균) 모델
king.ff1 = diff(king.ts, differences = 1) # 1회 차분
plot.ts(king.ff1)
acf(king.ff1, lag.max = 20) # 1을 제외한 모든 구간이 점선 안에 있음
acf(king.ff1, lag.max = 20, plot = F)
# 
# Autocorrelations of series ‘king.ff1’, by lag
# 
# 0      1      2      3      4      5      6      7      8      9     10 
# 1.000 -0.360 -0.162 -0.050  0.227 -0.042 -0.181  0.095  0.064 -0.116 -0.071 
# 11     12     13     14     15     16     17     18     19     20 
# 0.206 -0.017 -0.212  0.130  0.114 -0.009 -0.192  0.072  0.113 -0.093
pacf(king.ff1, lag.max = 20) # 1~3에서 점선 구간 초과, 절단점이 4
pacf(king.ff1, lag.max = 20, plot = F)
# 
# Partial autocorrelations of series ‘king.ff1’, by lag
# 
# 1      2      3      4      5      6      7      8      9     10 
# -0.360 -0.335 -0.321  0.005  0.025 -0.144 -0.022 -0.007 -0.143 -0.167 
# 11     12     13     14     15     16     17     18     19     20 
# 0.065  0.034 -0.161  0.036  0.066  0.081 -0.005 -0.027 -0.006 -0.037
auto.arima(king) # 적절한 ARIMA 모델 찾기
# Series: king 
# ARIMA(0,1,1) - 적절한 ARIMA 모형
# 
# Coefficients:
#           ma1
#       -0.7218
# s.e.   0.1208
# 
# sigma^2 = 236.2:  log likelihood = -170.06
# AIC=344.13   AICc=344.44   BIC=347.56
king.arima = arima(king, order = c(0, 1, 1)) # 10명의 왕이 사망 시 나이 예측
king.forecasts = forecast(king.arima)
king.forecasts
#    Point Forecast    Lo 80    Hi 80    Lo 95     Hi 95
# 43       67.75063 48.29647 87.20479 37.99806  97.50319
# 44       67.75063 47.55748 87.94377 36.86788  98.63338
# 45       67.75063 46.84460 88.65665 35.77762  99.72363
# 46       67.75063 46.15524 89.34601 34.72333 100.77792
# 47       67.75063 45.48722 90.01404 33.70168 101.79958
# 48       67.75063 44.83866 90.66260 32.70979 102.79146
# 49       67.75063 44.20796 91.29330 31.74523 103.75603
# 50       67.75063 43.59372 91.90753 30.80583 104.69543
# 51       67.75063 42.99472 92.50653 29.88974 105.61152
# 52       67.75063 42.40988 93.09138 28.99529 106.50596
# 신뢰 구간은 80% ~ 95% 사이

# 다차원척도법
# (1) 계량적 MDS - 구간척도, 비율척도
library(MASS)
loc = cmdscale(eurodist)
x = loc[, 1]
y = -loc[, 2]
plot(x, y, type = "n", asp = 1, main = "Metric MDS")
text(x, y, rownames(loc), cex = 0.7)
abline(v = 0, h = 0, lty = 2, lwd = 0.5)
# (2) 비계량적 MDS - 순서척도
library(MASS)
data("swiss")
swiss.x = as.matrix(swiss[, -1])
swiss.dist = dist(swiss.x)
swiss.mds = isoMDS(swiss.dist)
plot(swiss.mds$points, type = "n")
text(swiss.mds$points, label = as.character(1:nrow(swiss.x)))
abline(v = 0, h = 0, lty = 2, lwd = 0.5) # isoMDS 사례
swiss.x = as.matrix(swiss[, -1])
swiss.sammon = sammon(dist(swiss.x))
plot(swiss.sammon$points, type = "n")
text(swiss.sammon$points, label = as.character(1:nrow(swiss.x)))
abline(v = 0, h = 0, lty = 2, lwd = 0.5)

# 주성분분석
library(datasets)
data("USArrests")
pairs(USArrests, panel = panel.smooth, main = "USArrets data")
# Murder와 UrbanPop 비율 간의 관련성이 작아 보임
US.prin = princomp(USArrests, cor = T)
summary(US.prin)
# Importance of components:
#                           Comp.1    Comp.2    Comp.3     Comp.4
# Standard deviation     1.5748783 0.9948694 0.5971291 0.41644938
# Proportion of Variance 0.6200604 0.2474413 0.0891408 0.04335752
# Cumulative Proportion  0.6200604 0.8675017 0.9566425 1.00000000
# 2개의 주성분을 활용
loadings(US.prin)
# 
# Loadings:
#          Comp.1 Comp.2 Comp.3 Comp.4
# Murder    0.536  0.418  0.341  0.649
# Assault   0.583  0.188  0.268 -0.743
# UrbanPop  0.278 -0.873  0.378  0.134
# Rape      0.543 -0.167 -0.818       
# 
#                Comp.1 Comp.2 Comp.3 Comp.4
# SS loadings      1.00   1.00   1.00   1.00
# Proportion Var   0.25   0.25   0.25   0.25
# Cumulative Var   0.25   0.50   0.75   1.00
US.prin$scores
#                     Comp.1      Comp.2      Comp.3       Comp.4
# Alabama         0.98556588  1.13339238  0.44426879  0.156267145
# Alaska          1.95013775  1.07321326 -2.04000333 -0.438583440
# Arizona         1.76316354 -0.74595678 -0.05478082 -0.834652924
# Arkansas       -0.14142029  1.11979678 -0.11457369 -0.182810896
# California      2.52398013 -1.54293399 -0.59855680 -0.341996478
# Colorado        1.51456286 -0.98755509 -1.09500699  0.001464887
# Connecticut    -1.35864746 -1.08892789  0.64325757 -0.118469414
# Delaware        0.04770931 -0.32535892  0.71863294 -0.881977637
# Florida         3.01304227  0.03922851  0.57682949 -0.096284752
# Georgia         1.63928304  1.27894240  0.34246008  1.076796812
# Hawaii         -0.91265715 -1.57046001 -0.05078189  0.902806864
# Idaho          -1.63979985  0.21097292 -0.25980134 -0.499104101
# Illinois        1.37891072 -0.68184119  0.67749564 -0.122021292
# Indiana        -0.50546136 -0.15156254 -0.22805484  0.424665700
# Iowa           -2.25364607 -0.10405407 -0.16456432  0.017555916
# Kansas         -0.79688112 -0.27016470 -0.02555331  0.206496428
# Kentucky       -0.75085907  0.95844029  0.02836942  0.670556671
# Louisiana       1.56481798  0.87105466  0.78348036  0.454728038
# Maine          -2.39682949  0.37639158  0.06568239 -0.330459817
# Maryland        1.76336939  0.42765519  0.15725013 -0.559069521
# Massachusetts  -0.48616629 -1.47449650  0.60949748 -0.179598963
# Michigan        2.10844115 -0.15539682 -0.38486858  0.102372019
# Minnesota      -1.69268181 -0.63226125 -0.15307043  0.067316885
# Mississippi     0.99649446  2.39379599  0.74080840  0.215508013
# Missouri        0.69678733 -0.26335479 -0.37744383  0.225824461
# Montana        -1.18545191  0.53687437 -0.24688932  0.123742227
# Nebraska       -1.26563654 -0.19395373 -0.17557391  0.015892888
# Nevada          2.87439454 -0.77560020 -1.16338049  0.314515476
# New Hampshire  -2.38391541 -0.01808229 -0.03685539 -0.033137338
# New Jersey      0.18156611 -1.44950571  0.76445355  0.243382700
# New Mexico      1.98002375  0.14284878 -0.18369218 -0.339533597
# New York        1.68257738 -0.82318414  0.64307509 -0.013484369
# North Carolina  1.12337861  2.22800338  0.86357179 -0.954381667
# North Dakota   -2.99222562  0.59911882 -0.30127728 -0.253987327
# Ohio           -0.22596542 -0.74223824  0.03113912  0.473915911
# Oklahoma       -0.31178286 -0.28785421  0.01530979  0.010332321
# Oregon          0.05912208 -0.54141145 -0.93983298 -0.237780688
# Pennsylvania   -0.88841582 -0.57110035  0.40062871  0.359061124
# Rhode Island   -0.86377206 -1.49197842  1.36994570 -0.613569430
# South Carolina  1.32072380  1.93340466  0.30053779 -0.131466685
# South Dakota   -1.98777484  0.82334324 -0.38929333 -0.109571764
# Tennessee       0.99974168  0.86025130 -0.18808295  0.652864291
# Texas           1.35513821 -0.41248082  0.49206886  0.643195491
# Utah           -0.55056526 -1.47150461 -0.29372804 -0.082314047
# Vermont        -2.80141174  1.40228806 -0.84126309 -0.144889914
# Virginia       -0.09633491  0.19973529 -0.01171254  0.211370813
# Washington     -0.21690338 -0.97012418 -0.62487094 -0.220847793
# West Virginia  -2.10858541  1.42484670 -0.10477467  0.131908831
# Wisconsin      -2.07971417 -0.61126862  0.13886500  0.184103743
# Wyoming        -0.62942666  0.32101297  0.24065923 -0.166651801
arrests.pca = prcomp(USArrests, center = T, scale. = T)
biplot(arrests.pca)
