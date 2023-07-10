# 19_성능_평가

install.packages("caret")
suppressMessages(library(caret)) # library(caret)
data("GermanCredit")


d <- sort(sample(nrow(GermanCredit), nrow(GermanCredit)*0.7))
train.df <- GermanCredit[d,]
test.df <- GermanCredit[-d,]

credit.var <- setdiff(colnames(train.df), list('Class'))
credit.formula <- as.formula(paste('Class', paste(credit.var, collapse = '+'),
                                   sep = '~'))


credit.m <- glm(credit.formula, family = binomial(link = 'logit'),
                data = train.df)

train.df$pred <- predict(credit.m, newdata = train.df, type = "response")
test.df$pred <- predict(credit.m, newdata = test.df, type = "response")


ggplot(data = test.df) + geom_density(aes(x=pred, color=Class, linetype=Class))


test.df$results75 <- ifelse(test.df$pred > 0.75, "Good", "Bad")
test.df$results75 <- as.factor(test.df$results75)

library(e1071)
confusionMatrix(data = test.df$results75, reference = test.df$Class,
                positive = "Good")


test.df$results60 <- ifelse(test.df$pred > 0.60, "Good", "Bad")
test.df$results60 <- as.factor(test.df$results60)
confusionMatrix(data = test.df$results60, reference = test.df$Class,
                positive = "Good")

test.df$results30 <- ifelse(test.df$pred > 0.30, "Good", "Bad")
test.df$results30 <- as.factor(test.df$results30)
confusionMatrix(data = test.df$results30, reference = test.df$Class,
                positive = "Good")

test.df$results95 <- ifelse(test.df$pred > 0.95, "Good", "Bad")
test.df$results95 <- as.factor(test.df$results95)
confusionMatrix(data = test.df$results95, reference = test.df$Class,
                positive = "Good")
