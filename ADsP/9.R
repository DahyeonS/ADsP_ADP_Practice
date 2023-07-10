# 데이터 생성
x = c(2, 1, 6, 3, 3)
y = c(1, 2, 4, 3, 1)
data = data.frame(x, y)

# 데이터 행에 이름 할당하기
rownames(data) <- c('A', 'B', 'C', 'D', 'E')

# 거리 행렬 데이터 생성
# euclidean은 유클리디안 거리를 의미하며, 그 외에 menhattan, minkowski 등이 있다.
# 기본값은 euclidean이므로 method = 'euclidean'을 생략해도 같은 결과를 출력한다.
dist_data <- dist(data, method = 'euclidean')

# 거리행렬 데이터 살펴보기
print(dist_data)
#          A        B        C        D
# B 1.414214                           
# C 5.000000 5.385165                  
# D 2.236068 2.236068 3.162278         
# E 1.000000 2.236068 4.242641 2.000000

# 군집분석 수행
# single은 최단연결법을 의미하며, 그 외에 complete(최장), average(평균) 등이 있다.
# 기본값은 method = 'complete'이다.
hclust_data <- hclust(dist_data, method = 'single')

# 결과 시각화하기
plot(hclust_data)
# 만약 Height 값을 1.7로 설정한다면
abline(h = 1.7)
# (c), (d), (a, b, e) 3개의 군집으로 나뉘는 것을 확인할 수 있다.


cal <- c(52, 160, 89, 57, 34, 32, 30, 69)
car <- c(112.4, 8.5, 22.8, 14.5, 8.2, 7.7, 7.6, 18.1)
fat <- c(0.2, 14.7, 1.3, 0.7, 0.2, 0.3, 0.2, 0.2)
pro <- c(0.3, 2.0, 1.1, 0.3, 0.8, 0.7, 0.6, 0.7)
fib <- c(2.4, 6.7, 2.6, 2.4, 0.9, 2.0, 0.4, 0.9)
sug <- c(10.4, 0.7, 12.2, 9.9, 7.9, 4.7, 6.2, 15.5)
fruits <- data.frame(cal, car, fat, pro, fib, sug)
rownames(fruits) <- c('apple', 'avocado', 'banana', 'blueberry', 'melon', 'watermelon',
                      'strawberry', 'grape')
# 군집의 중심과 개체 사이의 거리를 구하는 데 단위의 문제가 발생할 것으로 예상된다면
# 표준화를 통해 단위 문제를 해결
fruits <- as.data.frame(scale(fruits, center = T, scale = T))
# 초깃값(K, 군집 수)은 3으로 설정
result <- kmeans(fruits, centers = 3)
# 각 군집의 중심
result$centers
#         cal        car        fat        pro        fib        sug
# 1 -0.6264519 -0.4325878 -0.3708745 -0.3890135 -0.4384435 -0.2734992
# 2  0.1068144  0.7302977 -0.3280178 -0.2059483 -0.1630925  0.9233982
# 3  2.1853644 -0.4605418  2.4675513  2.1738992  2.2430516 -1.6761980
# 2번 군집은 1번과 3번 군집에 비해 탄수화물(car)이 많은 집단인 것을 알 수 있다.

# 원본 데이터에 군집을 추가
fruits$cluster <- result$cluster
head(fruits)
#            cal   car  fat pro fib  sug cluster
# apple       52 112.4  0.2 0.3 2.4 10.4       2
# avocado    160   8.5 14.7 2.0 6.7  0.7       3
# banana      89  22.8  1.3 1.1 2.6 12.2       2
# blueberry   57  14.5  0.7 0.3 2.4  9.9       1
# melon       34   8.2  0.2 0.8 0.9  7.9       1
# watermelon  32   7.7  0.3 0.7 2.0  4.7       1
# 어느 과일이 어느 군집에 속하는지 확인 가능하다.

# 데이터가 2차원일 경우 시각화로도 표현이 가능하다.
# 군집분석을 위한 내장 데이터인 Nclus를 활용
install.packages("flexclust")
library(flexclust)
data("Nclus")
result <- kmeans(Nclus, 3)
# 시각화
plot(Nclus, col = result$cluster, xlim = c(-7, 10), ylim = c(-4, 10),
     xlab = '', y_lab = '')
# 군집의 중심을 표시
par(new = T)
plot(result$centers, pch = 3, cex = 3, xlim = c(-7, 10), ylim = c(-4, 10),
     xlab = '', ylab = '')


# 라이브러리 설치 및 호출
install.packages("fpc")
library(fpc)

# DBSCAN을 수행할 데이터 생성
data <- data.frame(
  x = c(sample(1 : 10, 15, replace = T), sample(20 : 30, 10, replace = T)),
  y = c(sample(1 : 10, 15, replace = T), sample(20 : 30, 10, replace = T))
)
# 데이터 프레임 시각화
plot(data)

# DBSCAN 수행
# 5라는 거리 안에 3개의 점이 있다면 하나의 그룹으로 군집화
dbscan_result <- dbscan(data, eps = 5, MinPts = 3)
# 그룹화 시각화 확인
plot(data, col = dbscan_result$cluster)


# 미국의 온천 분출 시간 자료가 담긴 내장 데이터 faithful의 waiting 열을 활용
# 혼합 분포 군집을 위한 mixtools 패키지를 도출
install.packages("mixtools")
library(mixtools)
waiting <- faithful$waiting
hist(waiting)
# 두 개의 정규분포가 혼합된 것으로 유추 가능
# normalmixEM을 활용하여 혼합 분포 군집을 수행
result <- normalmixEM(waiting)
# number of iterations= 32 
summary(result)
# summary of normalmixEM object:
#            comp 1    comp 2
# lambda  0.360887  0.639113
# mu     54.614893 80.091093
# sigma   5.871245  5.867716
# loglik at estimate:  -1034.002

# 분포 1은 평균이 54.61, 표준편차 5.87인 정규분포를 따르며, 그 비중은 약 36%다.
# 분포 2는 평균이 80.09, 표준편차 5.87인 정규분포를 따르며, 그 비중은 약 64%다.

# plot을 사용해서 시각화한 결과를 얻을 수 있다.
plot(result, density = T)
# Hit <Return> to see next plot:
# 엔터를 누르면 다음 그래프를 볼 수 있다.


cal <- c(52, 160, 89, 57, 34, 32, 30, 69)
car <- c(112.4, 8.5, 22.8, 14.5, 8.2, 7.7, 7.6, 18.1)
fat <- c(0.2, 14.7, 1.3, 0.7, 0.2, 0.3, 0.2, 0.2)
pro <- c(0.3, 2.0, 1.1, 0.3, 0.8, 0.7, 0.6, 0.7)
fib <- c(2.4, 6.7, 2.6, 2.4, 0.9, 2.0, 0.4, 0.9)
sug <- c(10.4, 0.7, 12.2, 9.9, 7.9, 4.7, 6.2, 15.5)
fruits <- data.frame(cal, car, fat, pro, fib, sug)
names <- c('apple', 'avocado', 'banana', 'blueberry', 'melon', 'watermelon',
           'strawberry', 'grape')
rownames(fruits) <- names
# kohonen 패키지 호출
install.packages("kohonen")
library(kohonen)
# fruits 데이터 표준화
fruits_scaled <- scale(fruits, center = T, scale = T)
# 자기조직화지도 수행, 경쟁층의 노드 수는 1×3으로 3개
result <- som(fruits_scaled, grid = somgrid(3, 1))
# 1×2 시각화 표현
par(mfrow = c(1, 2))
plot(result)
plot(result, type = 'mapping', labels = names)
# 왼쪽 그림의 세번째 노드는 설탕(sug)을 많이 포함한 과일들을 의미한다.
# 오른쪽 그림을 보아 세번째 노드는 딸기, 수박, 멜론, 블루베리가 있음을 알 수 있다.
# 왼쪽 그림의 두번째 노드는 탄수화물(car)과 설탕(sug)을 많이 포함한 과일을 의미한다.
# 오른쪽 그림을 보아 두번째 노드는 사과, 포도, 바나나를 포함하고 있음을 알 수 있다.
# 왼쪽 그림의 첫번째 노드는 열량(cal), 단백질(pro), 지방(fat), 식이섬유(fib)를 많이 포함한 과일을 의미한다.
# 오른쪽 그림을 보아 첫번째 노드에는 아보카도가 속해 있음을 알 수 있다.