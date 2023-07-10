# 12_자율학습

x <- c(1, 2, 3, 4, 5)
y <- c(4, 1, 1, 6, 5)
xy <- data.frame(cbind(x, y))

x
y
xy

hc <- hclust(dist(xy), method = "complete")
plot(hc, hang = -1)
