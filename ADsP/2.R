student_number = c(1, 2, 1, 2)
semester = c(1, 1, 2, 2)
math_score = c(60, 90, 70, 90)
english_score = c(80, 70, 40, 60)
score = data.frame(student_number, semester, math_score, english_score)
score
install.packages("reshape")


library(reshape)
melt(score, id = c("student_number", "semester"))


# melt를 활용하여 얻은 결과를 melted_score로 저장
melted_score = melt(score, id = c("student_number", "semester"))

# 학생의 과목별 평균 점수를 알고 싶은 경우
cast(melted_score, student_number ~ variable, mean)

# 학생의 학기별 평균 점수를 알고 싶은 경우
cast(melted_score, student_number ~ semester, mean)

# 학생의 과목별 최댓값을 알고 싶은 경우
cast(melted_score, student_number ~ variable, max)


install.packages("sqldf")
library(sqldf)
sqldf('select * from score')

sqldf('select * from score where student_number = 1')

sqldf('select avg(math_score), avg(english_score) from score group by student_number')


install.packages("plyr")
library(plyr)
class = c("A", "A", "B", "B")
math = c(50, 70, 60, 90)
english = c(70, 80, 60, 80)
score = data.frame(class, math, english)


score

library(plyr)
ddply(score, "class", summarise, math_avg = mean(math), eng_avg = mean(english))

# summarise는 데이터 요약, transform은 기존 데이터에 추가
ddply(score, "class", transform, math_avg = mean(math), eng_avg = mean(english))


year = c(rep(2012, 4), rep(2013, 4))
month = c(rep(1, 2), rep(2, 2), rep(1, 2), rep(2, 2))
value = c(3, 5, 7, 9, 1, 5, 4, 6)
data = data.frame(year, month, value)
data

# 기준이 되는 변수를 2개 이상 묶어서 사용 가능
ddply(data, c("year", "month"), summarise, value_avg = mean(value))

# 원하는 임의의 함수를 작성해서 사용 가능
ddply(data, c("year", "month"), function(x){
  value_avg = mean(x$value)
  value_sd = sd(x$value)
  data.frame(avg_sd = value_avg / value_sd)
})


install.packages("data.table")
library(data.table)


year = rep(c(2012:2015), each = 12000000)
month = rep(rep(c(1:12), each = 1000000), 4)
value <- runif(48000000)
# 같은 데이터로 4800만 개 행을 갖는 데이터프레임과 데이터 테이블을 생성
DataFrame = data.frame(year, month, value)
DataTable = as.data.table(DataFrame)
# 데이터프레임의 검색 시간을 측정
system.time(DataFrame[DataFrame$year == 2012,])
#  사용자  시스템 elapsed 
#    0.58    0.11    0.69 
# 데이터 테이블의 검색 시간을 측정
system.time(DataTable[DataTable$year == 2012,])
# 사용자  시스템 elapsed 
#   0.25    0.03    0.17 
# 명령문의 시작부터 종료까지 0.17초
# 데이터 테이블의 연도 칼럼에 키 값을 설정
# 칼럼이 키 값으로 설정될 경우 자동 오름차순 정렬
setkey(DataTable, year)
# 키 값으로 설정된 칼럼과 J 표현식을 사용한 검색 시간 측정
system.time(DataTable[J(2012)])
#  사용자  시스템 elapsed 
#    0.17    0.01    0.07 
# 명령문의 시작부터 종료까지 0.07초
# 키 값을 활용한 데이터 테이블의 탐색 속도가 더 빠른 것을 확인할 수 있다.



head(iris, 3)

# 기초 통계량
summary(iris)
# 데이터 구조 파악
str(iris)


install.packages("Amelia")
copy_iris <- iris # 원본 데이터를 유지
copy_iris[sample(1:150, 30), 1] <- NA # Sepal.Length에 30개의 결측값 생성
library(Amelia)
missmap(copy_iris)


copy_iris <- iris # 원본 데이터를 유지
dim(copy_iris) # 기존 데이터 수
copy_iris[sample(1:150, 30), 1] <- NA # Sepal.Length에 30개의 결측값 생성
copy_iris <- copy_iris[complete.cases(copy_iris),] #단순대치법
dim(copy_iris) # 30개의 결측값을 보유한 행을 제거한 데이터의 수


install.packages("DMwR2")
library(DMwR2)
# 테스트를 위한 결측값을 가진 iris 데이터 생성
copy_iris <- iris # 원본 데이터를 유지
copy_iris[sample(1:150, 30), 1] <- NA # Sepal.Length에 30개의 결측값 생성
# 평균 대치법
meanValue <- mean(copy_iris$Sepal.Length, na.rm = T) # 결측값을 제외한 평균 계산
copy_iris$Sepal.Length[is.na(copy_iris$Sepal.Length)] <- meanValue # 평균 대치
# centralImputation을 활용한 중앙값 대치
copy_iris[sample(1:150, 30), 1] <- NA
copy_iris <- centralImputation(copy_iris)


# 테스트를 위한 결측값을 가진 iris 데이터 생성
copy_iris <- iris # 원본 데이터를 유지
copy_iris[sample(1:150, 30), 1] <- NA # Sepal.Length에 30개의 결측값 생성
copy_iris <- knnImputation(copy_iris, k = 10)


# 테스트를 위한 결측값을 가진 iris 데이터 생성
copy_iris <- iris # 원본 데이터를 유지
copy_iris[sample(1:150, 30), 1] <- NA # Sepal.Length에 30개의 결측값 생성
library(Amelia)
iris_imp <- amelia(copy_iris, m = 3, cs = "Species")
# cs는 cross-sectional로 분석에 포함될 정보를 의미
# 위 amelia에서 m 값을 그대로 imputataion의 데이터셋에 사용한다.
copy_iris$Sepal.Length <- iris_imp$imputations[[3]]$Sepal.Length


# 테스트를 위한 임의 데이터 생성
data <- c(3, 10, 13, 16, 11, 20, 17, 25, 43)
boxplot(data, horizontal = T)
