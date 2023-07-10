a = seq(1, 8); b = seq(7, 9)
x = c(1, 2, 3, 4); y = c(6.25, 3.14, 5.18); z = c("fee", "fie", "fun")
c = matrix(b, 2, 3)
d = matrix(0, 4, 5)
e = matrix(1:20, 4, 5); e
g = matrix(1:4, 2, 2)
rownames(d) = c("1", "2", "3", "4")
colnames(d) = c("a", "b", "c", "d", "e")

data.frame(b, y)
#   b    y
# 1 7 6.25
# 2 8 3.14
# 3 9 5.18
new = data.frame(a = 1, b = 2, c = 3, d = "a"); new
#   a b c d
# 1 1 2 3 a
dfm = as.data.frame(c); dfm
#   V1 V2 V3
# 1  7  9  8
# 2  8  7  9
rbind(b, y)
#   [,1] [,2] [,3]
# b 7.00 8.00 9.00
# y 6.25 3.14 5.18
cbind(b, y)
#      b    y
# [1,] 7 6.25
# [2,] 8 3.14
# [3,] 9 5.18
N = 1000000
dtfm = data.frame(dosage = numeric(N), lab = character(N),
                  responses = numeric(N)) # 데이터 프레임 할당
colnames(dfm) # [1] "V1" "V2" "V3"
merge(x, y)
#    x    y
# 1  1 6.25
# 2  2 6.25
# 3  3 6.25
# 4  4 6.25
# 5  1 3.14
# 6  2 3.14
# 7  3 3.14
# 8  4 3.14
# 9  1 5.18
# 10 2 5.18
# 11 3 5.18
# 12 4 5.18
subset(dfm, select = "V1")
#   V1
# 1  7
# 2  8
subset(dfm, select = c("V1", "V3"))
#   V1 V3
# 1  7  8
# 2  8  9
subset(dfm, select = -3)
#   V1 V2
# 1  7  9
# 2  8  7
colnames(dfm) = c(1, 2, 3); dfm
#   1 2 3
# 1 7 9 8
# 2 8 7 9
NA_cleaning = na.omit(dfm)

as.list(vec) # 벡터 -> 리스트
as.vector(mat)  # 행렬 -> 벡터
cbind(vec); as.matrix(vec); rbind(vec); matrix(vec); matrix(vec, m, n) # 벡터 -> 행렬
unlist(lst) # 리스트 -> 벡터
sapply(a, log) # 벡터에 함수 적용
# [1] 0.0000000 0.6931472 1.0986123 1.3862944 1.6094379 1.7917595
# [7] 1.9459101 2.0794415
save(a, file = "test.Rdata")

source("C:/자료실/서다현/R 스크립트/10.R"); load("C:/자료실/서다현/R 스크립트/10.R")
# 파일 불러오기
data() # 데이터 불러오기
summary(a) # 데이터 요약
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    2.75    4.50    4.50    6.25    8.00
head(dfm, 1)
#   1 2 3
# 1 7 9 8

v = c(24, 23, 52, 46, 75, 25)
w = c(87, 86, 92, 84, 77, 68)
f = factor(c("A", "A", "B", "B", "C", "A"))

groups = split(v, f); groups
# $A
# [1] 24 23 25
# 
# $B
# [1] 52 46
# 
# $C
# [1] 75
groups = split(w, f); groups
# $A
# [1] 87 86 68
# 
# $B
# [1] 92 84
# 
# $C
# [1] 77
groups = unstack(data.frame(v, f)); groups
# $A
# [1] 24 23 25
# 
# $B
# [1] 52 46
# 
# $C
# [1] 75

library(MASS)
sp = split(Cars93$MPG.city, Cars93$Origin)
median(sp[[1]]) # [1] 20

lapply(l, func) # 함수 결과를 리스트 형태로 변환
sapply(l, func) # 함수 결과를 벡터 또는 행렬로 변환
apply(mat, 1, func) # 행렬에 함수 적용(데이터프레임은 동질적인 경우만 가능, 행렬 변환 후 적용)
tapply(vector, index, func) # 집단 별 함수 적용
by(drm, factor, func) # 행집단 함수 적용

nchar("단어") # [1] 2(문자열 길이)
paste("apple", "bear", sep = "-") # [1] "apple-bear"
paste("the pi is approximately", pi) # [1] "the pi is approximately 3.14159265358979"
paste(x, "loves me", collapse = ", and ") # [1] "1 loves me, and 2 loves me, and 3 loves me, and 4 loves me"
substr("statistics", 1, 4) # [1] "stat"
strsplit("pi-pie-po-pum", "-")
# [[1]]
# [1] "pi"  "pie" "po"  "pum"
sub(old, new, string); gsub(old, new, string) # 하위문자열 대체
mat = outer("apple", "bear", paste, sep = " "); mat
#      [,1]        
# [1,] "apple bear"

Sys.Date() # [1] "2023-02-15"(현재 날짜)
as.Date("2022-05-19") # [1] "2022-05-19"
format(Sys.Date(), format = "%m/%d/%y") # [1] "02/15/23"
format(Sys.Date(), "%a") # [1] "수"
format(Sys.Date(), "%b") # [1] "2"
format(Sys.Date(), "%B") # [1] "2월"
format(Sys.Date(), "%d") # [1] "15"
format(Sys.Date(), "%m") # [1] "02"
format(Sys.Date(), "%y") # [1] "23"
format(Sys.Date(), "%Y") # [1] "2023"

d = as.Date("2014-12-25")
p = as.POSIXlt(d)
p$yday # [1] 358
start = as.Date("2014-12-01")
end = as.Date("2014-12-25")
seq(from = start, to = end, by = 1)
# [1] "2014-12-01" "2014-12-02" "2014-12-03" "2014-12-04"
# [5] "2014-12-05" "2014-12-06" "2014-12-07" "2014-12-08"
# [9] "2014-12-09" "2014-12-10" "2014-12-11" "2014-12-12"
# [13] "2014-12-13" "2014-12-14" "2014-12-15" "2014-12-16"
# [17] "2014-12-17" "2014-12-18" "2014-12-19" "2014-12-20"
# [21] "2014-12-21" "2014-12-22" "2014-12-23" "2014-12-24"
# [25] "2014-12-25"