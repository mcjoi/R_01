

Regression = read.csv("C:\\workspace\\R\\data_sample\\regression\\Regression.csv")
head(Regression)

# 선형성을 판단하기 위한 그래프

library(ggplot2)
ggplot(Regression, aes(x= X, y = y)) + 
         geom_point() + 
         geom_smooth(method = 'lm') +
         theme_classic()
         
# 선형이므로 단순회귀분석

Reg = lm(y ~ X, data = Regression)
anova(Reg)
summary(Reg)


#실습

path <- "C:\\workspace\\R\\data_sample\\height\\height.csv"
height <- read.csv(path)

head(height)
str(height)
summary(height)
plot(x= height$gender, y = height$height)
height$gender2 <- as.factor(height$gender)

# 등분산부석
leveneTest(height ~ gender , data = height)
#Pr(>F) 는 0.7이므로 귀무가설을 기각하지 못한다. 즉, 두개 집단의 분산은 같다.
t.test(height ~ gender, data = height, var.equal = TRUE) 
# p-value가 0.9이므로, 유의수준 0.05보다 크다.
# 남녀 구분에 의한 키 차이가 존재하지 않음



 



