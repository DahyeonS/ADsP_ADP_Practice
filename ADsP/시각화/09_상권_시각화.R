# 09_상권_시각화

store = read.csv("D:/서다현/R 스크립트/시각화/상권분석.csv")
store

row.names(store) = store$지역
store = store[,2:6]

stars(store, filp.labels = FALSE, draw.segments = FALSE,
      frame.plot = TRUE, full = TRUE,
      main = "전국상권분석 -STAR Chart")
stars(store, filp.labels = FALSE, draw.segments = FALSE,
      frame.plot = FALSE, full = FALSE,
      main = "전국상권분석 -나이팅게일 Chart")


install.packages("fmsb")
library(fmsb)

data = data.frame(5 ,10, 5, 7.5, 2)
data
colnames(data) = c("매출증감률", "공급대비수요",
                   "점포별매출편차", "상권매출규모",
                   "월별매출편차")

data= rbind(rep(20,5), rep(0,5), data)
data

radarchart(data, axistype = 1, seg = 5, pcol = "red",
           plty = 2, title = "예비창업자평가상권")
radarchart(data, axistype = 1, title = "예비창업자평가상권",
           pcol = rgb(0.6, 0.5, 0.5, 0.9),
           pfcol = rgb(0.2, 0.5, 0.5, 0.5), plwd = 3,
           cglcol = "grey", cglty = 1, axislabcol = "grey",
           caxislabels = seq(0, 20, 5), cglwd = 0.9, vlcex = 1.5)


radar <- function(x){
  if(x==1){
    data = data.frame(5, 10, 5, 7.5, 2)
    return(data)
  }else if(x==2){
    data = data.frame(2, 8, 5, 12, 9)
    return(data)
  }else{
    data = data.frame(8, 14, 3, 9.5, 17)
    return(data)
  }
}

radar(1)
radar(2)
radar(3)

par(mfrow = c(2, 2))
for (i in 1:3) {
  p = radar(i)
  print(p)
  colnames(p) = c("매출증감률", "공급대비수요", "점포별매출편차",
                  "상권매출규모", "월별매출편차")
  datap = rbind(rep(20,10), rep(0,10), p)
  if(i==1){
    col = "dark red"
    main = "예비창업자평가상권-강남"
  }else if(i==2){
    col = "dark green"
    main = "예비창업자평가상권-마포"
  }else{
    col = "dark violet"
    main = "예비창업자평가상권-종로"
  }
  radarchart(datap, axistype = 1, seg = 5, pcol = col, plty = 2, title = main)
}
