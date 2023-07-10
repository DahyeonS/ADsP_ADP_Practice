# 06_분산분석

install.packages("car")
library(car)

data(anorexia, package = "MASS")
anorexia
leveneTest(Postwt ~ Treat, data = anorexia)

out1 = aov(Postwt ~ Treat, data = anorexia)
out1
summary(out1) 
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Treat        2    919   459.5   8.651 0.000444 ***
# Residuals   69   3665    53.1  

out2 = anova(lm(Postwt ~ Treat, data = anorexia))
out2
summary(out2)
#       Df            Sum Sq        Mean Sq          F value     
# Min.   : 2.00   Min.   : 919   Min.   : 53.12   Min.   :8.651  
# 1st Qu.:18.75   1st Qu.:1606   1st Qu.:154.71   1st Qu.:8.651  
# Median :35.50   Median :2292   Median :256.31   Median :8.651  
# Mean   :35.50   Mean   :2292   Mean   :256.31   Mean   :8.651  
# 3rd Qu.:52.25   3rd Qu.:2979   3rd Qu.:357.90   3rd Qu.:8.651  
# Max.   :69.00   Max.   :3665   Max.   :459.49   Max.   :8.651  
# NA's   :1      
#      Pr(>F)         
# Min.   :0.0004443  
# 1st Qu.:0.0004443  
# Median :0.0004443  
# Mean   :0.0004443  
# 3rd Qu.:0.0004443  
# Max.   :0.0004443  
# NA's   :1   

out3 = oneway.test(Postwt ~ Treat, data = anorexia)
out3
summary(out3)
# F = 9.9047, num df = 2.000, denom df =
# 36.237, p-value = 0.0003702