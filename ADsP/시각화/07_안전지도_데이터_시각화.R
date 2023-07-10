# 07_안전지도_데이터_시각화

install.packages('ggmap')
library(ggmap)

cctv <- read.csv("D:/서울노원구_cctv.csv") # 디렉토리
head(cctv)
gmap <- get_map('Nowon-gu office', zoom = 13, maptype = 'roadmap')
ggmap(gmap) + geom_point(data = cctv, aes(x = cctv$경도, y = cctv$위도),
                         size = 2, alpha = .5, color = 'red')


