install.packages("fBasics")
library(help = fBasics)
help(package = fBasics)

source("C:/5.R") # 스크립트 파일 실행 # 디렉토리

sink("result.txt", append = FALSE, split = TRUE) # 파일을 생성해서 결과값을 보냄
a = 1
b = 2
a + b
dev.off()

pdf("output.pdf")
biplot(result)
dev.off()
