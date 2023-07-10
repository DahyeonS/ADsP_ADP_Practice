# 05_회귀분석
# 변수 간의 관계여부 분석

year = c(26,16,20,7,22,15,29,28,17,3,1,
       16,19,13,27,4,30,8,3,12)
annual_salary = c(1267,887,1022,511,
                  1193,795,1713,1477,991,455,324,944,
                  1232,808,1296,486,1516,565,299,830)
Data = data.frame(year, annual_salary)

summary(Data)
# year       annual_salary   
# Min.   : 1.00   Min.   : 299.0  
# 1st Qu.: 7.75   1st Qu.: 551.5  
# Median :16.00   Median : 915.5  
# Mean   :15.80   Mean   : 930.5  
# 3rd Qu.:23.00   3rd Qu.:1240.8  
# Max.   :30.00   Max.   :1713.0  

plot(year, annual_salary)
cor(year, annual_salary) # 0.9775856 (강한 상관관계)

LS <- lm(annual_salary ~ year, data = Data)
summary(LS)

# 근무연수는 연봉에 영향을 미친다.(매우 유의미함)