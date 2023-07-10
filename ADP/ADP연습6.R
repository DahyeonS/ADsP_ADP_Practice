library(reshape)
head(airquality)
#   Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
# 3    12     149 12.6   74     5   3
# 4    18     313 11.5   62     5   4
# 5    NA      NA 14.3   56     5   5
# 6    28      NA 14.9   66     5   6
aqm = melt(airquality, id = c("Month", "Day"), na.rm = T)
cast(aqm, Day ~ Month ~ variable)

library(sqldf)
sqldf("select * from iris")
sqldf("select Species from iris")

library(dplyr)
student_number = c(1, 2, 1, 2)
semester = c(1, 1, 2, 2)
math_score = c(60, 90, 70, 90)
english_score = c(80, 70, 40, 60)
score = data.frame(student_number, semester, math_score, english_score)
score
dd.test = ddply(score, "student_number", function(x) {
  m.math_score = mean(score$math_score)
  sd.math_score = sd(score$math_score)
  cv1 = round(sd.math_score/m.math_score, 4)
  m.english_score = mean(score$english_score)
  sd.english_score = sd(score$english_score)
  cv2 = round(sd.english_score/m.english_score, 4)
  data.frame(cv.math_score = cv1, cv.english_score = cv2)
})
dd.test

library(data.table)
DF = data.frame(x = runif(2.6e+07), y = rep(LETTERS, each = 10000))
df = data.frame(x = runif(2.6e+07), y = rep(letters, each = 10000))
system.time(x <- DF[DF$y == "C",])
# 사용자  시스템 elapsed 
#    0.11    0.00    0.12 
DT = as.data.table(DF)
setkey(DT, y)
system.time(x <- DT[J("C"),])
# 사용자  시스템 elapsed 
#    0.01    0.00    0.00

require(ggplot2)
data("diamonds")
dia.data = diamonds
head(dia.data)
summary(dia.data)

install.packages("klaR")
library(klaR)
iris2 = iris[, c(1, 3, 5)]
plineplot(Species ~ ., data = iris2, method = "lda", x = iris[, 4], xlab = "Petal.Width")
# [1] 0.03333333

x = c(1, 2, 3, NA)
mean(x) # [1] NA
mean(x, na.rm = T) # [1] 2

data("steam")
complete.cases(steam) # 결측값이 없으면 TRUE
is.na(steam)# 결측값은 TRUE

install.packages("Amelia")
library(Amelia)
amelia() # 다중 대치법으로 결측값 처리
