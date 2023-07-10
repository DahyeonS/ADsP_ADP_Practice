# 08_로지스틱 회귀분석

dose = c(1,1,2,2,3,3)
response = c(0,1,0,1,0,1)
count = c(7,3,5,5,2,8)
toxic = data.frame(dose, response, count)
toxic

out = glm(response ~ dose, weights = count, family = binomial, data = toxic)


plot(response ~ dose, data = toxic, type = "n",
     main = "Predicted Probability of Response")

curve(predict(out, data.frame(dose=x), type="resp"), add=TRUE)
