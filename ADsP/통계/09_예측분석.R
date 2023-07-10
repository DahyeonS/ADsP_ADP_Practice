# 09_예측분석

install.packages("rpart")
library(rpart)

head(iris)
x11(800, 600)
formula = Species ~ .
iris.df = rpart(formula, data = iris, method = "class")
iris.df
plot(iris.df)
text(iris.df, use.n = TRUE, cex = 0.7)
post(iris.df, filename = "")


# 국민총생산 예측 2018~2021
install.packages("WDI")
library(WDI)

gdp <- WDI(country = "KR", indicator = c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
           start = 1960, end = 2017)
head(gdp)
names(gdp) <- c("iso2c", "Country", "Year", "PerCapGDP", "GDP")
kr = gdp$PerCapGDP[gdp$Country == "Korea, Rep."]
kr = ts(kr, start = min(gdp$Year, end = max(gdp$Year)))
kr

install.packages("forecast")
library(forecast)

krts = auto.arima(x = kr)
Forecasts = forecast(object = krts, h = 5)
Forecasts
plot(Forecasts)
