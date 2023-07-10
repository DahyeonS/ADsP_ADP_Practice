library(rpart)
library(party)
library(ROCR)
x = kyphosis[sample(1:nrow(kyphosis), nrow(kyphosis), replace = F),]
x.train = kyphosis[1:floor(nrow(x)*0.75),]
x.evaluate = kyphosis[floor(nrow(x)*0.75):nrow(x),]
x.model = cforest(Kyphosis ~ Age + Number + Start, data = x.train)
x.evaluate$prediction = predict(x.model, newdata = x.evaluate)
x.evaluate$correct = x.evaluate$prediction == x.evaluate$Kyphosis
print(paste("% of predicted classification correct", mean(x.evaluate$correct)))
# [1] "% of predicted classification correct 0.818181818181818"
x.evaluate$probabilities = 1 - unlist(treeresponse(x.model,
newdata = x.evaluate), use.names = F)[seq(1, nrow(x.evaluate)*2, 2)]
pred = prediction(x.evaluate$probabilities, x.evaluate$Kyphosis)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "ROC curve", colorize = T)
perf = performance(pred, "lift", "rpp")
plot(perf, main = "lift curve", colorize = T)

# 로지스틱 회귀분석
a = iris[iris$Species == "setosa" | iris$Species == "versicolor",]
b = glm(Species ~ Sepal.Length, data = a, family = "binomial")
summary(b)
# 
# Call:
# glm(formula = Species ~ Sepal.Length, family = "binomial", data = a)
# 
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -2.05501  -0.47395  -0.02829   0.39788   2.32915  
# 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -27.831      5.434  -5.122 3.02e-07 ***
# Sepal.Length    5.140      1.007   5.107 3.28e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 138.629  on 99  degrees of freedom
# Residual deviance:  64.211  on 98  degrees of freedom
# AIC: 68.211
# 
# Number of Fisher Scoring iterations: 6
# 오즈비: 약 170(exp(5.140))

# 의사결정나무
# 1) iris data를 이용한 분석
idx = sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))
train.data = iris[idx == 2,]
test.data = iris[idx == 1,]
# 2) train.data를 이용하여 모형생성
iris.tree = ctree(Species ~., data = train.data)
plot(iris.tree)
plot(iris.tree, type = "simple")
# 3) 예측된 데이터와 실제 데이터의 비교
table(predict(iris.tree), train.data$Species)
# 
#            setosa versicolor virginica
# setosa          9          0         0
# versicolor      0          9         0
# virginica       0          0        13
# 4) test data를 적용하여 정확성 확인
test.pre = predict(iris.tree, newdata = test.data)
table(test.pre, test.data$Species)
# 
# test.pre     setosa versicolor virginica
#   setosa         39          0         0
#   versicolor      2         40         5
#   virginica       0          1        32

# 랜덤 포레스트
library(randomForest)
# 1) 모형 만들기
idx = sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))
train.data = iris[idx == 2,]
test.data = iris[idx == 1,]
r.f = randomForest(Species ~ ., data = train.data, ntree = 100, proximity = T)
# 2) 오차율 계산하기
table(predict(r.f), train.data$Species)
# 
#            setosa versicolor virginica
# setosa         13          0         0
# versicolor      0         16         0
# virginica       0          0        13
# 3) 그래프 그리기
plot(r.f)
varImpPlot(r.f)
# 5) test data 예측
pre.rf = predict(r.f, newdata = test.data)
table(pre.rf, test.data$Species)
# 
# pre.rf       setosa versicolor virginica
#   setosa         37          0         0
#   versicolor      0         33         5
#   virginica       0          1        32
plot(margin(r.f, test.data$Species))

# 군집분석
# (1) 계층적 군집분석
idx = sample(1:dim(iris)[1], 40)
iris.s = iris[idx,]
iris.s$Species = NULL
hc = hclust(dist(iris.s), method = "ave")
plot(hc, hand = -1, labels = iris$Species[idx])
# (2) K-평균 군집분석(비계층적 군집분석)
data(iris)
newiris = iris
newiris$Species = NULL
kc = kmeans(newiris, 3)
table(iris$Species, kc$cluster)
# 
#             1  2  3
# setosa      0  0 50
# versicolor  2 48  0
# virginica  36 14  0
plot(newiris[c("Sepal.Length", "Sepal.Width")], col = kc$cluster)

# 연관분석
data("Groceries")
inspect(Groceries[1:3])
#     items                 
# [1] {citrus fruit,        
#      semi-finished bread, 
#      margarine,           
#      ready soups}         
# [2] {tropical fruit,      
#      yogurt,              
#      coffee}              
# [3] {whole milk}
rules = apriori(Groceries, parameter = list(support = 0.01, confidence = 0.3))
# Apriori
# 
# Parameter specification:
#  confidence minval smax arem  aval originalSupport maxtime
#         0.3    0.1    1 none FALSE            TRUE       5
#  support minlen maxlen target  ext
#     0.01      1     10  rules TRUE
# 
# Algorithmic control:
#   filter tree heap memopt load sort verbose
#      0.1 TRUE TRUE  FALSE TRUE    2    TRUE
# 
# Absolute minimum support count: 98 
# 
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
# sorting and recoding items ... [88 item(s)] done [0.00s].
# creating transaction tree ... done [0.00s].
# checking subsets of size 1 2 3 4 done [0.00s].
# writing ... [125 rule(s)] done [0.00s].
# creating S4 object  ... done [0.00s].
# 88개 아이템, 125개 룰
inspect(sort(rules, by = c("lift"), decreasing = T)[1:20])
#      lhs                                       rhs                support    confidence coverage   lift     count
# [1]  {citrus fruit, other vegetables}       => {root vegetables}  0.01037112 0.3591549  0.02887646 3.295045 102  
# [2]  {tropical fruit, other vegetables}     => {root vegetables}  0.01230300 0.3427762  0.03589222 3.144780 121  
# [3]  {beef}                                 => {root vegetables}  0.01738688 0.3313953  0.05246568 3.040367 171  
# [4]  {citrus fruit, root vegetables}        => {other vegetables} 0.01037112 0.5862069  0.01769192 3.029608 102  
# [5]  {tropical fruit, root vegetables}      => {other vegetables} 0.01230300 0.5845411  0.02104728 3.020999 121  
# [6]  {other vegetables, whole milk}         => {root vegetables}  0.02318251 0.3097826  0.07483477 2.842082 228  
# [7]  {whole milk, curd}                     => {yogurt}           0.01006609 0.3852140  0.02613116 2.761356  99  
# [8]  {root vegetables, rolls/buns}          => {other vegetables} 0.01220132 0.5020921  0.02430097 2.594890 120  
# [9]  {root vegetables, yogurt}              => {other vegetables} 0.01291307 0.5000000  0.02582613 2.584078 127  
# [10] {tropical fruit, whole milk}           => {yogurt}           0.01514997 0.3581731  0.04229792 2.567516 149  
# [11] {yogurt, whipped/sour cream}           => {other vegetables} 0.01016777 0.4901961  0.02074225 2.533410 100  
# [12] {other vegetables, whipped/sour cream} => {yogurt}           0.01016777 0.3521127  0.02887646 2.524073 100  
# [13] {tropical fruit, other vegetables}     => {yogurt}           0.01230300 0.3427762  0.03589222 2.457146 121  
# [14] {root vegetables, whole milk}          => {other vegetables} 0.02318251 0.4740125  0.04890696 2.449770 228  
# [15] {whole milk, whipped/sour cream}       => {yogurt}           0.01087951 0.3375394  0.03223183 2.419607 107  
# [16] {citrus fruit, whole milk}             => {yogurt}           0.01026945 0.3366667  0.03050330 2.413350 101  
# [17] {onions}                               => {other vegetables} 0.01423488 0.4590164  0.03101169 2.372268 140  
# [18] {pork, whole milk}                     => {other vegetables} 0.01016777 0.4587156  0.02216573 2.370714 100  
# [19] {whole milk, whipped/sour cream}       => {other vegetables} 0.01464159 0.4542587  0.03223183 2.347679 144  
# [20] {curd}                                 => {yogurt}           0.01728521 0.3244275  0.05327911 2.325615 170
