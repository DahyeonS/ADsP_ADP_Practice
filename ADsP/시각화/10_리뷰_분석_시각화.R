# 10_리뷰_분석_시각화

setwd("D:/서다현/R 스크립트/시각화")

library(KoNLP)
library(wordcloud)
library(RColorBrewer)

data1 <- readLines("Moviescore.txt")
data1

data2 <- sapply(data1, extractNoun, USE.NAMES = FALSE)
data2

data3 <- unlist(data2)
data3 <- Filter(function(x){nchar(x) >= 2}, data3)
data3

write(unlist(data3), "Moviescore2.txt")

data4 <- read.table("Moviescore2.txt")
data4

wordcount <- table(data4)
wordcount

head(sort(wordcount, decreasing = TRUE), 20)

palette <- brewer.pal(9, "Set3")     
palette

wordcloud(names(wordcount), freq = wordcount, scale = c(5, 1),
          rot.per = 0.25, min.freq = 1, random.order = FALSE,
          random.color = TRUE, colors = palette)


data1 <- readLines("m1.txt")
data1

data2 <- sapply(data1, extractNoun, USE.NAMES = FALSE)
data2

data3 <- unlist(data2)
data3 <- Filter(function(x){nchar(x) >= 2}, data3)
data3

write(unlist(data3), "m2.txt")

data4 <- read.table("m2.txt")

wordcount <- table(data4)
wordcount
head(sort(wordcount, decrasing = TRUE), 20)

palette <- brewer.pal(9, "Set3")
display.brewer.all()

wordcloud(names(wordcount), freq = wordcount, scale = c(5, 1),
          rot.per = 0.25, min.freq = 1, random.order = FALSE,
          random.color = TRUE, colors = palette)
