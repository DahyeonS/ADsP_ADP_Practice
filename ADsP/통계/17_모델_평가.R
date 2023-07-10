# 17_모델_평가

install.packages('functional')
install.packages('ROCR')
library(functional)
library(ROCR)

pivot.titanic <- as.data.frame(Titanic)
pivot.titanic


attach(pivot.titanic)
titanic.class <- c()
titanic.sex <- c()
titanic.age <- c()
titanic.survived <- c()

for(i in 1:nrow(pivot.titanic)){
  n.rep <- functional::Curry(rep, times = Freq[i])
  titanic.class <- append(titanic.class, n.rep(as.character(Class[i])))
  titanic.sex <- append(titanic.sex, n.rep(as.character(Sex[i])))
  titanic.age <- append(titanic.age, n.rep(as.character(Age[i])))
  titanic.survived <- append(titanic.survived,
                             n.rep(as.character(Survived[i])))
}
detach(pivot.titanic)


titanic <- data.frame(
  Idx = 1:length(titanic.class),
  Class = titanic.class,
  Sex = titanic.sex,
  Age = titanic.age,
  Survived = titanic.survived)

titanic

summary(titanic)

titanic$Survived[titanic$Survived=='Yes'] <- 1
titanic$Survived[titanic$Survived=='No'] <- 0
titanic$Survived <- as.numeric(titanic$Survived)

sampling.info <- aggregate(Freq ~ Class + Sex + Age, pivot.titanic, sum)
sampling.info

test.ratio <- 0.7

test.idx <- c()
for(i in 1:nrow(sampling.info)){
  target.row <- sampling.info[i,]
  key <- target.row[1:3]
  target.rows.idx <- merge(x=key, y=titanic, by=c('Class','Sex','Age'))$Idx
  test.idx <- append(test.idx, sample(target.rows.idx, target.row$Freq *
                                        test.ratio))
}

test <- titanic[test.idx,]
train.idx <- setdiff(titanic$Idx, test.idx)
train <- titanic[train.idx,]
nrow(train) # 667
nrow(test) # 1534
nrow(titanic) # 2201

summary(train)
summary(test)

model <- glm(Survived ~ Class + Sex + Age, family = 'binomial', data = train)
summary(model)


require(ROCR)
prob <- predict(model,
                newdata = test, type = 'response')
labeled.score <- merge(titanic,
                       data.frame(Idx = 
                                    as.integer(names(prob)),
                                  score = prob),
                       by = c('Idx'))

write.csv(labeled.score[with(labeled.score, order(-score)),],
          file = 'titanic_logistic_score.csv')

pred <- prediction(prob, test$Survived)
roc <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(roc, col = 'red')
legend('bottomright', c('base', 'logistic'), col = 1:2, lty = 2:1)
abline(a = 0 , b = 1, lty = 'dotted')

auc <- performance(pred, measure = 'auc')
auc <- auc@y.values[[1]]
auc
