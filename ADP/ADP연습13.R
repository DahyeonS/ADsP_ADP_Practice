# 공간분석
library(googleVis)
data("Fruits")
head(Fruits)
#     Fruit Year Location Sales Expenses Profit       Date
# 1  Apples 2008     West    98       78     20 2008-12-31
# 2  Apples 2009     West   111       79     32 2009-12-31
# 3  Apples 2010     West    89       76     13 2010-12-31
# 4 Oranges 2008     East    96       81     15 2008-12-31
# 5 Bananas 2008     East    85       76      9 2008-12-31
# 6 Oranges 2009     East    93       80     13 2009-12-31

# 1. 모션 차트
M1 = gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year")
plot(M1)

# 2. 지오차트
data("Exports")
head(Exports)
#         Country Profit Online
# 1       Germany      3   TRUE
# 2        Brazil      4  FALSE
# 3 United States      5   TRUE
# 4        France      4   TRUE
# 5       Hungary      3  FALSE
# 6         India      2   TRUE
# 색상 구분
G1 = gvisGeoChart(Exports, locationvar = "Country", colorvar = "Profit")
plot(G1) # 전세계 국가별 수출/수익 크기
G2 = gvisGeoChart(Exports, "Country", "Profit", options = list(region = "150"))
plot(G2) # 유럽 국가별 수익 크기
# 표시 방식 및 해상도 수준 지정
require(datasets)
states = data.frame(state.name, state.x77)
head(states)
#            state.name Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
# Alabama       Alabama       3615   3624        2.1    69.05   15.1    41.3    20  50708
# Alaska         Alaska        365   6315        1.5    69.31   11.3    66.7   152 566432
# Arizona       Arizona       2212   4530        1.8    70.55    7.8    58.1    15 113417
# Arkansas     Arkansas       2110   3378        1.9    70.66   10.1    39.9    65  51945
# California California      21198   5114        1.1    71.71   10.3    62.6    20 156361
# Colorado     Colorado       2541   4884        0.7    72.06    6.8    63.9   166 103766
G3 = gvisGeoChart(states, "state.name", "Illiteracy", options = list(region = "US",
displayMode = "regions", resolution = "provinces", width = 600, height = 400))
plot(G3) # 미국의 주별 문맹률
G5 = gvisGeoChart(Andrew, "LatLong", colorvar = "Speed_kt",
                  options = list(region = "US"))
# (1) 속도 표시
plot(G5) # 허리케인 경로 - 색상으로 표시
G6 = gvisGeoChart(Andrew, "LatLong", sizevar = "Speed_kt",
                  colorvar = "Pressure_mb", options = list(region = "US"))
plot(G6) # 허리케인 경로 - 원 크기로 표시
# (2) 깊이 표시
require(stats)
data("quakes")
head(quakes)
#      lat   long depth mag stations
# 1 -20.42 181.62   562 4.8       41
# 2 -20.62 181.03   650 4.2       15
# 3 -26.00 184.10    42 5.4       43
# 4 -17.97 181.66   626 4.1       19
# 5 -20.42 181.96   649 4.0       11
# 6 -19.68 184.31   195 4.0       12
quakes$latlong = paste(quakes$lat, quakes$long, sep = ":")
head(quakes$latlong)
# [1] "-20.42:181.62" "-20.62:181.03" "-26:184.1"    
# [4] "-17.97:181.66" "-20.42:181.96" "-19.68:184.31"
G7 = gvisGeoChart(quakes, "latlong", "depth", "mag",
options = list(displayMode = "Markers", region = "009", colorAxis = "{colors:['red', 'grey']}",
               backgroundColor = "lightblue"))
plot(G7) # 지진의 깊이와 진도
# (3) 데이터를 읽어오는 경우 - 고정 데이터
library(XML)
url = "https://en.wikipedia.org/wiki/List_of_countries_by_credit_rating"
x = readHTMLTable(readLines(url), which = 3, header = T)
levels(x$Rating) = substring(levels(x$Rating), 4,
                             nchar(levels(x$Rating)))
x$Ranking = x$Rating
levels(x$Ranking) = nlevels(x$Rating):1
x$Ranking = as.character(x$Ranking)
x$Rating = paste(x$Country, x$Rating, sep = ": ")
G8 = gvisGeoChart(x, "Country/Region", "Ranking", hovervar = "Rating",
                  options = list(gvis.editor = "S&P",
                                 colorAxis = "{colors:['#91BFDB', '#FC8D59']}"))
plot(G8) # 국가별 신용등급 정보
# (4) 데이터를 읽어오는 경우 - 가변 데이터
library(XML)
url = "https://ds.iris.edu/seismon/eventlist/index.phtml"
eq = readHTMLTable(readLines(url),
                   colClasses = c("factor", rep("numeric", 4), "factor"))$evTable
names(eq) = c("DATE", "LAT", "LON", "MAG", "DEPTH", "LOCATION_NAME", "IRIS_ID")
eq$loc = paste(eq$LAT, eq$LON, sep = ":")
G9 = gvisGeoChart(eq, "loc", "DEPTH", "MAG",
                  options = list(displayMode = "Markers",
                                 colorAxis = "{colors:['purple', 'red', 'orange', 'grey']}",
                                 backgroundColor = "lightblue"), chartid = "EQ")
plot(G9) # 최근 30일간 진도 4.0이상의 지진발생 정보

# 3. 샤이니
options(repos = c(RStudio = 'http://rstudio/org/_packages', getOption('repos')))
library(shiny)
runExample("01_hello")
runExample("02_text")
runExample("03_reactivity")

# 4. 모자이크 플룻
library(vcd)
library(datasets); data("Titanic")
str(Titanic)
# 'table' num [1:4, 1:2, 1:2, 1:2] 0 0 35 0 0 0 17 0 118 154 ...
# - attr(*, "dimnames")=List of 4
#  ..$ Class   : chr [1:4] "1st" "2nd" "3rd" "Crew"
#  ..$ Sex     : chr [1:2] "Male" "Female"
#  ..$ Age     : chr [1:2] "Child" "Adult"
#  ..$ Survived: chr [1:2] "No" "Yes"
# 기본 형태
mosaic(Titanic)
# 색상 추가 - 비교
mosaic(Titanic, shade = T, legend = T)
# 색상 추가 - 특정 집단
strucplot(Titanic, pop = F)
grid.edit("rect:Class=1st,Sex=Male,Age=Adult,Survived=Yes", gp = gpar(fill ="red"))
