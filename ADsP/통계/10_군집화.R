# 10_군집화

data("iris")
a = iris
a$Species = NULL # 붓꽃의 종에 대한 사전정보 제거(사전정보 불필요)

kc = kmeans(a, 3)
kc
summary(kc)
table(iris$Species, kc$cluster)
plot(a[c("Sepal.Length", "Sepal.Width")], col = kc$cluster)
