# # 성공확률이 0.5인 베르누이 시행을 20회 했을 때의 이항분포 밀도함수
y <- dbinom(0:20, size = 20, prob = 0.5)
plot(0:20, y, type='h', lwd = 5, col="grey", ylab = "prob", xlab = "확률변수 X", main = c("X ~ B(20, 0.5)"))

# 동전을 20번 던졌다. 앞면이 12번 나올 확률은?
# P(X = 12) 확률계산
dbinom(12, size = 20, prob = 0.5) # 약 12%

# 동전을 20번 던졌다. 앞면이 12번 이하로 나올 확률은?
# P(X <= 12) 확률계산 
pbinom(12, size = 20, prob = 0.5, lower.tail = TRUE) # 86%
pbinom(12, size = 20, prob = 0.5) # 디폴트 값이 true임
sum(dbinom(0:12, size = 20, prob = 0.5)) # dbinom을 12까지 더해도 같은 값

# 동전을 20번 던졌다. 앞면이 13번 이상 나올 확률은?
# P(X > 12) 확률계산 
pbinom(12, size = 20, prob = 0.5, lower.tail = FALSE) # 86%
1 - pbinom(12, size = 20, prob = 0.5, lower.tail = TRUE) # 같은 값

# 이항분포 난수 발생
k = rbinom(20, size=200, prob = 0.20)

plot(k2, type='h')
plot(pbinom(0:20, size=20, prob=0.5), type='h')


# 예시, 200번 동전을 던졌다. 100번 이하로 앞이 나올 확률은?
pbinom(99, 200, prob = 0.5)


     