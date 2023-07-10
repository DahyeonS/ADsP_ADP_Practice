library(ggplot2)

# 1. XY 그래프
data("ChickWeight")
head(ChickWeight)
#   weight Time Chick Diet
# 1     42    0     1    1
# 2     51    2     1    1
# 3     59    4     1    1
# 4     64    6     1    1
# 5     76    8     1    1
# 6     93   10     1    1
# 포인트 그래프
ggplot(ChickWeight, aes(x = Time, y = weight, colour = Diet, group = Chick)) + geom_line() # 기본 XY 그래프
h = ggplot(ChickWeight, aes(x = Time, y = weight, colour = Diet)) 
h + geom_point(alpha = .3)
# 스무스 그래프
h = ggplot(ChickWeight, aes(x = Time, y = weight, colour = Diet))
h + geom_smooth(alpha = .4, size = 3)
# 개선된 그래프
ggplot(ChickWeight, aes(x = Time, y = weight, colour = Diet)) + geom_point(alpha = .3) + geom_smooth(alpha = .2, size = 1)

# 2. 히스토그램
ggplot(subset(ChickWeight, Time = 21), aes(x = weight, fill = Diet)) + geom_histogram(colour = "black",
binwidth = 50) + facet_grid(Diet ~ .) # 가로로 출력
ggplot(subset(ChickWeight, Time = 21), aes(x = weight, fill = Diet)) + geom_histogram(colour = "black",
binwidth = 50) + facet_grid(. ~ Diet) # 세로로 출력

# 3. 포인트 그래프
data(mtcars)
head(mtcars)
#                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
# Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
# Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
# Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
# Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
# 기폰 포인트 그래프
p = qplot(wt, mpg, colour = hp, data = mtcars)
p + coord_cartesian(ylim = c(0, 40))
p + scale_color_continuous(breaks = c(100, 300))
p + guides(colour = "colourbar")
# 치환 데이터를 이용한 포인트 그래프
m = mtcars[1:10,] # 앞부분 10건만 추출
p%+%m # m을 앞에서 그린 그래프 p에 설정

# 4. 막대그래프
# 기본 막대그래프
c = ggplot(mtcars, aes(factor(cyl))) # 범주형 데이터를 factor로 변환
c + geom_bar()
# 다양한 옵션 적용
c + geom_bar(fill = "white", colour = "red")
library(ggplot2movies)
m = ggplot(movies, aes(x = rating))
m + geom_histogram()
m + geom_histogram(aes(fill = ..count..))

# 5. 선 그래프
data("economics")
head(economics)
# A tibble: 6 × 6
#   date         pce    pop psavert uempmed unemploy
#   <date>     <dbl>  <dbl>   <dbl>   <dbl>    <dbl>
# 1 1967-07-01  507. 198712    12.6     4.5     2944
# 2 1967-08-01  510. 198911    12.6     4.7     2945
# 3 1967-09-01  516. 199113    11.9     4.6     2958
# 4 1967-10-01  512. 199311    12.9     4.9     3143
# 5 1967-11-01  517. 199498    12.8     4.7     3066
# 6 1967-12-01  525. 199657    11.8     4.8     3018
# 기본 선 그래프
b = ggplot(economics, aes(x = date, y = unemploy))
b + geom_line()
# 다양한 옵션 적용
b = ggplot(economics, aes(x = date, y = unemploy))
b + geom_line(colour = "blue", size = 0.3, linetype = 3)

# 6. 효과 주기
data("diamonds")
head(diamonds)
# A tibble: 6 × 10
#   carat cut   color clarity depth table price     x     y     z
#   <dbl> <ord> <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
# 1  0.23 Ideal E     SI2      61.5    55   326  3.95  3.98  2.43
# 2  0.21 Prem… E     SI1      59.8    61   326  3.89  3.84  2.31
# 3  0.23 Good  E     VS1      56.9    65   327  4.05  4.07  2.31
# 4  0.29 Prem… I     VS2      62.4    58   334  4.2   4.23  2.63
# 5  0.31 Good  J     SI2      63.3    58   335  4.34  4.35  2.75
# 6  0.24 Very… J     VVS2     62.8    57   336  3.94  3.96  2.48
# 히스토그램
k = ggplot(diamonds, aes(carat, ..density..)) + geom_histogram(binwidth = 0.2)
k + facet_grid(. ~ cut) # carat 종류를 위쪽에 표시
# 막대그래프
library(dplyr)
w = ggplot(diamonds, aes(clarity, fill = cut))
w + geom_bar()
ggplot(diamonds, aes(clarity, fill = cut, order = desc(cut))) + geom_bar()
# 선 그래프
df = data.frame(x = 1:10, y = 1:10)
f = ggplot(df, aes(x = x, y = y))
f + geom_line(linetype = 2)
f + geom_line(linetype = "dotdash")

# 7. 포인트 그래프
# 기본 형태
df = data.frame(x = rnorm(5000), y = rnorm(5000))
h = ggplot(df, aes(x, y))
h + geom_point()
# 포인트 투명도 조절
h = ggplot(df, aes(x, y))
h + geom_point(alpha = 1/10)
# 포인트에 색 할당(요인별)
p = ggplot(mtcars, aes(wt, mpg))
p + geom_point(size = 4)
p + geom_point(aes(colour = factor(cyl)), size = 4)
# 포인트에 모양 할당(요인별)
p = ggplot(mtcars, aes(wt, mpg))
p + geom_point(aes(shape = factor(cyl)), size = 4)
# 포인트에 크기 할당(요인별)
p = ggplot(mtcars, aes(wt, mpg))
p + geom_point(aes(size = qsec))
# 옵션 미지정
p + geom_point()
# 임의의 선 삽입
p + geom_point(size = 2.5) + geom_hline(yintercept = 25, size = 3.5)
# 포인트에 모양 할당(전체)
p + geom_point(shape = 5)
# 포인트에 모양 할당 - 문자(전체)
p + geom_point(shape = "k", size = 3)
# 포인트에 모양 할당 - 모양 없애기
p + geom_point(shape = NA)
# 25가지 유형의 셰이프 이용
df2 = data.frame(x = 1:5, y = 1:25, z = 1:25)
s = ggplot(df2, aes(x = y, y = y))
s + geom_point(aes(shape = z), size = 4) + scale_shape_identity()

# 8. 기타 효과 옵션
# 선형 모델링
dmod = lm(price ~ cut, data = diamonds)
cuts = data.frame(cut = unique(diamonds$cut), predict(dmod, data.frame(cut =
unique(diamonds$cut)), se = T)[c("fit", "se.fit")])
se = ggplot(cuts, aes(x = cut, y = fit, ymin = fit - se.fit, ymax = fit + se.fit,
                      color = cut))
se + geom_pointrange()
# 포인트 그래프 - 박스로 강조
p = ggplot(mtcars, aes(wt, mpg)) + geom_point()
p + annotate("rect", xmin = 2, xmax = 3.5, ymin = 2, ymax = 25, fill = "dark gray", alpha = .5)
# 축의 범위 지정
p = qplot(disp, wt, data = mtcars) + geom_smooth()
p + scale_x_continuous(limits = c(325, 500))
# Boxplot
qplot(cut, price, data = diamonds, geom = "boxplot") # 세로
last_plot() + coord_flip() # 가로
# qplot
qplot(cut, data = diamonds, geom = "bar")

# 9. 다축 그래프
time = seq(7000, 3400, -200)
pop = c(200, 400, 450, 500, 300, 100, 400, 700, 830, 1200, 400, 350, 200, 700, 370, 800,
        200, 100, 120)
grp = c(2, 5, 8, 3, 2, 2, 4, 7, 9, 4, 4, 2, 2, 7, 5, 12, 5, 4, 4)
med = c(1.2, 1.3, 1.2, 0.9, 2.1, 1.4, 2.9, 3.4, 2.1, 1.1, 1.2, 1.5, 1.2, 0.9, 0.5,
        3.3, 2.2, 1.1, 1.2)
par(mar = c(5, 12, 4, 4) + 0.1)
# 첫번째 그래프 생성
plot(time, pop, axes = F, xlim = c(7000, 3400), ylim = c(0, max(pop)),
     xlab = "", ylab = "", type = "l", col = "black", main = "",)
# 첫번째 그래프에 점 추가
points(time, pop, pch = 20, col = "black")
# 첫번째 그래프에 y축 생성
axis(2, ylim = c(0, max(pop)), col = "black", lwd = 2)
# 첫번째 y축 이름 지정
mtext(2, text = "Population", line = 2)
# 두번째 그래프 생성
par(new = T)
plot(time, med, axes = F, xlim = c(7000, 3400), ylim = c(0, max(med)),
     xlab = "", ylab = "", type = "l", lty = 2, lwd = 2, col = "black", main = "",)
# 두번째 그래프에 점 추가
points(time, med, pch = 20, col = "black")
# 두번째 그래프에 y축 생성
axis(2, ylim = c(0, max(med)), col = "black", lwd = 2, line = 3.5)
# 두번째 y축 이름 지정
mtext(2, text = "Median Group size", line = 5.5)
# 세번째 그래프 생성
par(new = T)
plot(time, grp, axes = F, xlim = c(7000, 3400), ylim = c(0, max(grp)),
     xlab = "", ylab = "", type = "l", lty = 3, lwd = 2, col = "black", main = "",)
axis(2, ylim = c(0, max(grp)), col = "black", lwd = 2, line = 7)
# 세번째 그래프에 점 추가
points(time, grp, pch = 20, col = "black")
# 세번째 y축 이름 지정
mtext(2, text = "Number of Groups", line = 9)
# x축 생성 및 이름 지정
axis(1, pretty(range(time), 10))
mtext(side = 1, text = "cal BP", col = "black", line = 2)
# 사용자화
legend(x = 7000, y = 12, legend = c("Population", "Median Group size",
"Number of Groups"), lty = c(1, 2, 3))

library(aplpack)
# 9. 줄기-잎 그림
score = c(1, 2, 3, 4, 10, 2, 30, 42, 31, 50, 80, 76, 90, 87, 21, 43, 65, 76, 32, 12, 34, 54)
score
#  [1]  1  2  3  4 10  2 30 42 31 50 80 76 90 87 21 43 65 76 32 12 34 54
stem.leaf(score)
# 1 | 2: represents 12
#  leaf unit: 1
#             n: 22
#    5    0 | 12234
#    7    1 | 02
#    8    2 | 1
#   (4)   3 | 0124
#   10    4 | 23
#    8    5 | 04
#    6    6 | 5
#    5    7 | 66
#    3    8 | 07
#    1    9 | 0

# 10. 얼굴 그림
faces(WorldPhones)
# effect of variables:
#   modified item       Var       
#   "height of face   " "N.Amer"  
#   "width of face    " "Europe"  
#   "structure of face" "Asia"    
#   "height of mouth  " "S.Amer"  
#   "width of mouth   " "Oceania" 
#   "smiling          " "Africa"  
#   "height of eyes   " "Mid.Amer"
#   "width of eyes    " "N.Amer"  
#   "height of hair   " "Europe"  
#   "width of hair   "  "Asia"    
#   "style of hair   "  "S.Amer"  
#   "height of nose  "  "Oceania" 
#   "width of nose   "  "Africa"  
#   "width of ear    "  "Mid.Amer"
#   "height of ear   "  "N.Amer"

# 11. 별 그림
stars(WorldPhones)
