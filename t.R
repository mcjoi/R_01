

# 여부(left)는 0 : 이직 안함, 1 : 이직이기 때문에 이직 여부는 2개의 수준을 가지고 있는 명목형 변수이고
# 직무만족도(satisfaction_level)는 0 ~ 1 사이에 있는 연속형 변수입니다.

# 연속형 변수를 두 수준을 지니고 있는 명목형 변수에 따라 차이가 있는지 검정하고 싶기에 T 검정을 진행하는 것이 적합한 상황

HR$left = as.factor(HR$left)
HR$Work_accident = as.factor(HR$Work_accident)


install.packages("car")
library(car)


# 등분산 검정
leveneTest(satisfaction_level ~ left , data = HR)
leveneTest(satisfaction_level ~ Work_accident , data = HR)
leveneTest(height ~ gender , data = height)
t.test(height ~ gender, data = height, var.equal = TRUE)
# pr(>F) < 2.2e-16 , 0에 가까우므로
# 유의수준 0.05보다 훨씬 작으므로, 
# 두 집단의 분산이 동일하지 않다. 

#t검정
t.test(satisfaction_level ~ left, data = HR, var.equal = TRUE) # 등분산이 동일한 경우,
t.test(satisfaction_level ~ left, data = HR, var.equal = FALSE) # 등분산이 동일하지 않은 경우,
t.test(satisfaction_level ~ Work_accident, data = HR, var.equal = FALSE) # 등분산이 동일하지 않은 경우,
# 이직 여부에 따라 직무 만족도의 차이가 존재함
# 이직을 하지 않음 : 0.66 / 이직을 했음 : 0.44 -> 이직을 안한쪽의 만족도가 높다. 




height = read.csv("C:\\workspace\\R\\data_sample\\height\\height.csv")
heightANOVA = aov(height ~ city, data = height)
heightANOVA2 = aov(height ~ gender, data = height)
summary(heightANOVA2)
heightTUKEY2 = TukeyHSD(heightANOVA2)
plot(heightTUKEY2)


#분산분석 -> 요인이 3개 이상일때,
# 일원배치 분산분석(One way Anova)
ANOVA = aov(satisfaction_level ~ salary, data = HR)
ANOVA2 = aov(satisfaction_level ~ Work_accident, data = HR)
summary(ANOVA)
summary(ANOVA2)
# 연봉별로 만족도가 같지 않음을 확인
# 얼마나 다른지 확인하기 => 사후검정
TUKEY = TukeyHSD(ANOVA)
TUKEY2 = TukeyHSD(ANOVA2)
plot(TUKEY)
plot(TUKEY2)
# 각 신뢰구간 안에 0이 포함되는지 아닌지만 보시면 됩니다.








