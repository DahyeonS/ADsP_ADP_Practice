# 03_R_그래픽_패키지

install.packages("googleVis")
library(googleVis)

Fruits
M1 <- gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year")
plot(M1)

tbl <- gvisTable(Fruits, options = list(height = 220))
plot(tbl)

M2 <- gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year",
                      date.format = "%Y%m%d")
plot(M2)

M3 <- gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year",
                      date.format = "%YW%W")
plot(M3)


mise <- read.csv("D:/서다현/R 스크립트/시각화/미세먼지.csv", header = T)
mise$시간
mise2 <- gvisMotionChart(mise, idvar = "지점", timevar = "시간")
plot(mise2)


df = data.frame(country = c("US", "UK", "BR"), val1 = c(1, 3, 4),
                val2 = c(23, 12, 32))
Intensity <- gvisIntensityMap(df)
plot(Intensity)

Exports
Geo = gvisGeoChart(Exports, locationvar = "Country", colorvar = "Profit",
                   options = list(projection = "kavrayskiy-vii"))
plot(Geo)


require(datasets)
states <- data.frame(state.name, state.x77)
states
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy", options =
                            list(region = "US", displaymode = "regions",
                                 resolution = "provinces", width = 600,
                                 height = 400))
plot(GeoStates)

?Andrew
GeoMarker <- gvisGeoChart(Andrew, "LatLong", sizevar = "Speed_kt",
                          colorvar = "Pressure_mb", options = list(region = "US"))
plot(GeoMarker)

AndrewMap <- gvisMap(Andrew, "LatLong", "Tip",
                     options = list(showTip = TRUE, howLine = TRUE,
                                    enableScrollWheel = TRUE, mayType = "terrain",
                                    useMapTypeControl = TRUE))
plot(AndrewMap)
