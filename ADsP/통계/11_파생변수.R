# 11_파생변수

install.packages("dplyr")
library(dplyr)
install.packages("hflights")
library(hflights)

str(hflights)

colnames(hflights)
hflight_df <- tibble::as_tibble(hflights)
hflight_df

aa <- mutate(hflight_df, gain = ArrDelay - DepDelay)
aa

reshape2::melt.data.frame(
  data, # melt할 데이터
  id.cars, # 식별자 칼럼들
  meausre.vars, # 측정치 칼럼들
  na.rm = FALSE # 결측치 포함 여부
)

cast()