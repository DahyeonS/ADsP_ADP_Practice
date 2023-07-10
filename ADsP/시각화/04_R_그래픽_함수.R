# 04_R_그래픽_함수

plot(1:10)
arrows(7,7,8,8)
segments(4,4,6,6)
rect(1,6,3,8)
text(2,7,"배추값 인상률")


olympic <- read.csv("D:/medal.csv")
olympic
olympic$한국
olympic$중국
olympic$이탈리아

barplot(olympic$한국)
barplot(olympic$중국)
barplot(olympic$이탈리아)

barplot(as.matrix(olympic), main = "금매달수", ylab = "수량",
        beside = TRUE, col = rainbow(3), ylim = c(0, 6))
legend(8, 6, c("금", "은", "동"), cex = 0.8, fill = rainbow(3))
