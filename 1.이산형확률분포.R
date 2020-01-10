# 이산형 확률분포

# 1. 이항분포(binom)

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




# 2. 초기하 분포(hyper)
# 이항 분포는 동전이 다시 초기화 됨. 즉 모집단에서 샘플을 빼고, 다시 집어넣는 형태 
# 따라서 뽑힐 확률이 항상 동일했었음
# 하지만, 다시 넣지 않는다면,,, (복원하지 않는다면).. 
# 베르누이 시행이 될 수 없고, 표본을 추출할때마다, 확률도 바뀌게 됨 


# 성공확률이 p, 크기가 N인 모집단에서, n개의 표본을 비복원으로 추출할때,
# 성공이 일어나는 횟수를 X라고 하면, X는 "모수 N, n, p인 초기하 분포를 따른다."고 한다. 

# 모집단을 구성하는 개체가 성공/실패 2가지만 있을 때, 
# 모집단 N이 작을 때는 복원추출이면, 이항분포, 비복원 추출이면, 초기하 분포를 사용해야한다.
# 모집단이 졸라 큰 경우에는 두개의 분포가 근사치에 가까워 진다. 

plot(dhyper(x = c(0:20), m = 5, n = 20, k = 5),
     type = 'h')
# 성공확률이 뒤로 갈수록 줄어듬(하나씩 빠지니까)

# P(X = 3) 확률 계산 
# 빨간색 8개, 파란색 10개의 공이 있음, 이 중에 빨간공 5개를 뽑는 거임, 이때, 빨강공 3개를 뽑을 확률?
dhyper(x = 3, m = 8, n = 10, k = 5)
dhyper(x = 1, m = 8, n = 10, k = 3)
# m과 n은 factor별 갯수, k는 뽑는 횟수, x는 성공(m) 이 되는 횟수

# dhyper(x = 5, m = 8, n = 10, k = 5)	: 1%	5개 중에 5개 전부 m
# dhyper(x = 3, m = 8, n = 10, k = 5)	: 29%	5개 중에 3개가 m
# dhyper(x = 2, m = 8, n = 10, k = 5)	: 39%	5개 중에 2개가 m

# P(X <= 3) 확률 계산 
# 빨간공 8개와 파란공 10개가 있을때, 이중에 5개를 뽑아서, 빨간공이 0,1,2,3 중 하나 일 경우 확률?
phyper(q = 3, m = 8, n = 10, k = 5)


# 특정확률에 해당하는 분위수 구하기
# 빨강공 5개와 파란공 20개, 5번의 비복원 추출의 확률
dhyper(x = 3, m = 5, n = 20, k = 5) # 0.03576134
# 이제, 확률을 알게 되었다면, 이에 해당하는 확률변수 X를 구할 수 있다. 
qhyper(p = 0.03576134, m=5, n=20, k= 5, lower.tail = FALSE)

phyper(q=3, m=5, n=20, k=5, lower.tail = T)
qhyper(p=0.998099, m=5,n=20, k=5, lower.tail = T)

random_hyper <- rhyper(1000, m=5, n=20, k=5)
tr = data.frame(random_hyper)
summary(tr)
plot(y = c(1:1000), x =random_hyper, type = 'h')



# 3. 포아송 확률분포
# 일정한 단위시간, 단위 공간에서, 어떤 사건이 랜덤하게 발생하느 경우에 사용

plot(dpois(x=c(1:10), lambda = 3), type = 'h')

# P(X=15) 확률 
# 어느 은행의 1시간당 방문 고객 수가 ㅅ = 20인 포아송 분포를 따른다고 한다. 1시간에 15명이 올 확률은?
plot(dpois(x = c(1:20), lambda = 20), type = 'h')

# 은행에 시간당 20명씩 온다.. 그런데, 15명이 올 확률은?
ppois(q = 15 , lambda = 20)

# 특정 확률 값에 해당하는 분위수
qpois(p = 0.15 , lambda = 20, lower.tail = T)

# 난수
posr = rpois(n=1000, lambda = 20)
plot(table(posr), type = 'h')





