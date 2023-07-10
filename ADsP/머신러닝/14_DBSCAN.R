# 14_DBSCAN

set.seed(4)

sample1 <- cbind(rnorm(100, 4, 2), rnorm(100, 4, 2))
sample2 <- cbind(rnorm(100, 14, 2), rnorm(100, 14, 2))
sample3 <- cbind(rnorm(4, 15, 1), rnorm(4, 2, 1))
sample4 <- cbind(rnorm(40, 9, 1.5), rnorm(40, 10, 1.5))
sample5 <- cbind(rnorm(3, 5, 3), rnorm(3, 22, 4))

sample <- do.call(rbind, list(sample1, sample2, sample3, sample4, sample5))
sample <- as.data.frame(sample)
colnames(sample) <- c("x", "y")


install.packages("ggplot2")
library(ggplot2)

ggplot(sample, aes(x=x, y=y)) + geom_point()


set.seed(10)
k <- kmeans(sample, 5)

ggplot(sample, aes(x=x, y=y)) + geom_point(color = k$cluster) +
  geom_point(data = as.data.frame(k$centers), aes(x=x ,y=y), size = 6,
             color = "orange", shape = 17)


install.packages("fpc")
library(fpc)

db <- dbscan(sample, 1.15)
ggplot(sample, aes(x=x, y=y)) + geom_point(color = db$cluster + 1)


n <- max(db$cluster)
space <- as.data.frame(matrix(0, nrow = 1, ncol = 2))
for (i in 1:n) {
  space[i,] <- colMeans(sample[db$cluster == i, ])
}

ggplot(sample, aes(x=x, y=y)) + geom_point(color = db$cluster +1) +
  geom_point(data = space, aes(x=V1, y=V2), size = 6,
             color = "orange", shape = 17)
