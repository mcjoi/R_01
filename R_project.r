
install.packages("languageserver")

# import data from csv

HR = read.csv("C:\\workspace\\R\\data_sample\\HR_comma_sep\\HR_comma_sep.csv")


head(HR)
str(HR)
summary(HR)

summary(HR$left)
HR$Work_accident = as.factor(HR$Work_accident)
HR$left = as.factor(HR$left)
HR$promotion_last_5years = as.factor(HR$promotion_last_5years)

summary(HR)
str(HR)


HR$satisfaction_level_group_1 = ifelse(HR$satisfaction_level > 0.5, 'High', 'Low')
HR$satisfaction_level_group_2 = 
  ifelse(HR$satisfaction_level > 0.5, 'High', 
         ifelse(HR$satisfaction_level > 0.5, 'Mid', 'Low')
  )

HR_High = subset(HR, salary == 'high')
HR_High_IT = subset(HR, salary == 'high' & department == 'IT')
HR_High_IT2 = subset(HR, salary == 'high' | department == 'IT')


str(HR_High)
str(HR_High_IT)
str(HR_High_IT2)


# R에서 피벗테이블 만들기 ; plyr > ddply

install.packages('plyr')
library(plyr)

SS = ddply(HR, 
           c("department", "salary"), summarise, # department, salary 별로 요약값을 계
           M_SF = mean(satisfaction_level),
           COUNT = length(department),
           M_WH = round(mean(average_montly_hours),2)
           )
str(SS)
head(SS)


# GGplot을 활용하여, 그래프 그리기
library(ggplot2)
install.packages("ggplot2")



# 1. bar 그래프
ggplot(HR, 
      aes(x=salary)) 
      + geom_bar(aes(fill = department)) 
      + labs(fill = "Divided by department")



ggplot(HR, aes(x=salary)) + 
  geom_bar(aes(fill = left)) + 
  labs(fill = "Divided by left") + 
  xlab("salary") + ylab("")


# 2. 히스토그램
ggplot(HR, aes(x = satisfaction_level)) +
  geom_histogram(binwidth = 0.01, col='red', fill='royalblue') # col은 테두리색, fill은 채워지는 색


# 3. 밀도그래프
ggplot(HR, aes(x = satisfaction_level)) +
  geom_density(fill = 'blue') +
  ylab ("")

HR$left = sort(HR$left, decreasing = FALSE)

#4.박스플롯
ggplot(HR, aes(x = left, y = satisfaction_level)) +
  geom_boxplot(aes(fill = salary), alpha = I(0.4), outlier.colour = 'red') +
  xlab("이직여부") + ylab("만족도") + ggtitle("BOX PLOT") + labs(fill = "이직여부")


data_set$x = sort(data_set$x, decreasing = TRUE)


# 6. 산점도
ggplot(HR, aes(x = average_montly_hours, y = satisfaction_level)) + 
  geom_point(aes(col = left))





# 데이터 요약보기
summary(HR$salary)
summary(HR$satisfaction_level)

quantile(HR$satisfaction_level, probs = c(0.1, 0.3, 0.6, 0.9))





