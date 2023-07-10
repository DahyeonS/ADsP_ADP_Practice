class('abc')
class("abc")
class('1')
class("TRUE")

class(Inf)
class(1)
class(-3)

class(TRUE)
class(FALSE)

sqrt(-3)
class(NA)
class(NULL)

string1 <- 'abc'
"data" -> string2
number1 <<- 15
Inf ->> number2
logical = NA

string1 == 'abc'
string1 != 'abcd'
string2 > 'DATA'
number1 <= 15
is.na(logical)
is.null(NULL)

!TRUE
TRUE&TRUE
TRUE&FALSE
!(TRUE&FALSE)
TRUE|FALSE

v4 <- c(3, TRUE, FALSE)
v4
v5 <- c('a', 1, TRUE)
v5

v1 <- c(1:6)
v1

m1 <- matrix(c(1:6), nrow = 2)
m1
m2 <- matrix(c(1:6), ncol = 2)
m2

m3 <- matrix(c(1:6), nrow = 2, byrow = T)
m3

v1 <- c(1:6)
v1
dim(v1) <- c(2, 3)
v1

a1 <- array(c(1:12), dim = c(2, 3, 2))
a1

a2 <- c(1:12)
dim(a2) <- c(2, 3, 2)
a2

L <- list()
L[[1]] <- 5
L[[2]] = c(1:6)
L[[3]] = matrix(c(1:6), nrow = 2)
L[[4]] = array(c(1:12), dim = c(2, 3, 2))
L

v1 = c(1, 2, 3)
v2 = c('a', 'b', 'c')
df1 <- data.frame(v1, v2)
df1

help(paste)
?paste
paste('This is', 'a pen')
seq(1, 10, by = 2)
rep(1, 5)
a = 1
a
rm(a)
a
ls()
print(10)

v1 = c(1:9)
sum(v1)
mean(v1)
median(v1)
var(v1)
sd(v1)
max(v1)
min(v1)
range(v1)
summary(v1)
.libPaths()
install.packages("fBasics")
library(fBasics)
help(fBasics)
skewness(v1)
kurtosis(v1)

m1 = matrix(c(1:6), nrow = 2)
colnames(m1) = c('c1', 'c2', 'c3')
rownames(m1) = c('r1', 'r2')
m1
colnames(m1)
rownames(m1)
df1 = data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
colnames(df1) = c('c1', 'c2')
rownames(df1) = c('r1', 'r2', 'r3')
df1
colnames(df1)
rownames(df1)

v1 = c(3, 6, 9, 12)
v1[2]
m1 = matrix(c(1:6), nrow = 3)
m1[2, 2]
colnames(m1) = c('c1', 'c2')
m1[, 'c1']
rownames(m1) = c('r1', 'r2', 'r3')
m1['r3', 'c2']

v1 = c(1:6)
v2 = c(7:12)
df1 = data.frame(v1, v2)
df1$v1
df1$v2[3]

v1 = c(1, 2, 3)
v2 = c(4, 5, 6)
rbind(v1, v2)
cbind(v1, v2)

v1 = c(1, 2, 3)
v2 = c(4, 5, 6, 7, 8)
rbind(v1, v2)

for(i in 1:3) {
  print(i)
}
data = c("a", "b", "c")
for (i in data) {
  print(i)
}
i = 0
while (i < 5) {
  print(i)
  i = i + 1
}

number = 5
if (number < 5) {
  print('number는 5보다 작다.')
} else if (number > 5) {
  print('number는 5보다 크다.')
} else {
  print('number는 5와 같다')
}
number = 3
if (number < 5) {
  print('number는 5보다 작다.')
} else if (number > 5) {
  print('number는 5보다 크다.')
} else {
  print('number는 5와 같다')
}
number = 7
if (number < 5) {
  print('number는 5보다 작다.')
} else if (number > 5) {
  print('number는 5보다 크다.')
} else {
  print('number는 5와 같다')
}

compareTo5 = function(number) {
  if (number < 5) {
    print('number는 5보다 작다.')
  } else if (number > 5) {
    print('number는 5보다 크다.')
  } else {
    print('number는 5와 같다')
  }
}
compareTo5(10)
compareTo5(3)
compareTo5(5)

# 1+1을 계산하는 방법
1 + 1

data = 'This is a pen'
tolower(data)
toupper(data)
nchar(data)
substr(data, 9, 13)
strsplit(data, 'is')
grepl('pen', data)
gsub('pen', 'banana', data)

x = c(1:12)
head(x, 5)
tail(x, 5)
quantile(x)

df1 = data.frame(x = c(1, 1, 1, 2, 2), y = c(2, 3, 4, 3, 3))
df2 = data.frame(x = c(1, 2, 3, 4), z = c(5, 6, 7, 8))
subset(df1, x == 1)
merge(df1, df2, by = c('x'))
# 1은 각 행에 함수를 적용, 2는 각 열에 함수를 적용
apply(df1, 1, sum)
apply(df1, 2, sum)

Sys.Date()
Sys.time()
as.Date("2020-01-01")
# %Y는 연도 네 자리, %y는 연도 두 자리, %m은 월, %d는 일, %A는 요일 등
format(Sys.Date(), '%Y/%m/%d')
format(Sys.Date(), '%A')
# 시간 데이터를 unclass하면 타임스탬프를 얻을 수 있다.
unclass(Sys.time())
as.POSIXct(1666455084, origin = '1970-01-01')

x = c(1:10)
y = rnorm(10)
# 파라미터 type에서 p는 점, b는 점과 직선, n은 아무것도 표시하지 않음을 의미
# xlim로 x축의 범위, ylim로 y축의 범위를 설정할 수 있다.
# xlab, ylab으로 각 축의 이름을 지정할 수 있다.
# main으로 산점도의 이름을 정할 수 있다.
plot(x, y, type = 'l', xlim = c(-2, 12), ylim = c(-3, 3), xlab = 'X axis',
     ylab = 'Y axis', main = 'Test plot')
# abline의 v는 수직선, h는 수평선을 그리는 매개변수다.
# col의 매개변수로 색상을 선택할 수 있다.
abline(v = c(1, 10), col = 'blue')

myFunction = function(a, b) {
  result = 1
  for(i in 1:a) {
    result = result + i + b
  }
  print(result)
}
myFunction(3,2)