
#####
# 통계의 (1) 큰 줄기 하나가 모집단에서 
# 표본을 추출해서 주요 통계량을 가지고 
# 표본의 특성(중심화 경향, 퍼짐 정도, 분포모양, 상관성 등)을 알아보는 
# 기술통계(descriptive statistics)가 있고, 
# (2) 또 하나의 큰 줄기가 이렇게 파악한 표본집단의 
# 기술통계량을 가지고 모집단의 모수(parameter)를 추정(estimation)하고 가설을 
# 검정(test)하는 추론통계(inferential staticstics) 입니다. 

# 1) 신뢰구간 (confidence interval) 
# 주어진 확률 1-α (신뢰계수)에 대하여 표본분포의 통계량이 
# 모집단 모수에 포함되는 구간 (θ1 ~ θ2)

# 2) 신뢰계수 (confidence coefficient) 
# n개의 확률표본을 추출하여 신뢰구간을 구하는 잡업을 N번 반복하여 얻은 
# N개의 신뢰구간 중 (1-α)%에 미지의 모수 μ 가 포함되어 있을 확률

# 3) 유의수준 (significance level) 
# n개의 확률표본을 추출하여 신뢰구간을 구하는 잡업을 N번 반복하여 얻은 
# N개의 신뢰구간 중 미지의 모수 μ 가 포함되어 있지 않을 확률 (α)

# 4) 신뢰한계 (confidence limits) : 신뢰구간의 하한값(θ1)과 상한값(θ2)

#  X ~ N(mean=10, sd=3)인 정규분포에서 20개(n)의 확률표본을 추출하여 신뢰구간을 구하는 
#  작업을 100번(N) 번 반복 수행했을 때 얻은 100개(N)의 신뢰구간(confidence interval) 중에서 
#  95% 의 신뢰계수(confidence coefficient, 1-α)만큼은 
#  미지의 모수 μ (모집단의 평균) 가 포함되어 있고, 
#  5%의 유의수준(significance level, α) 만큼은 미지의 모수 μ(모집단의 평균)가 포함되지 않는 경우를, 
#  R을 이용해서 simulation 해보도록 하겠습니다. 
# (신뢰구간을 벗어난 case는 색깔, 선 형태가 다르게 그려지도록 프로그래밍 해놨습니다)

alpha <- 0.05 # significant level
N <- 100 # number of simulation frequency
n <- 20 # sampling number
mu <- 10 # mean of population
sigma <- 3 # SD of population

# Normal distribution plot
x <- seq(0, 20, length = 100) # 0에서 20까지 100으로 쪼갠다.
plot(x, dnorm(x = x, mean = mu, sd = 3), type = 'l')
abline(v = mu, col = "red", lty = 2)
abline(v = mu+1.96*3, col = "red", lty = 2)
abline(v = mu-1.96*3, col = "red", lty = 2)


# 대립가설의 형태에 따른 검정의 분류
# 단측검정 (one-sided test) : population mean > or < than specific value
# 양측검정 (Two-sided test) : population mean =! with specific value





# 검정 통계량(test statistic) 과 P-value
# 검정 통계량 : 귀무가설의 채택 또는 기각여부를 판단하는데 사용하는 통계량
# 표본 자료로부터 관측된 모수의 추정값이 귀무가설 하에서 일어나기 힘든 값, 매우 희귀한 값이면
# 귀무가설을 기각하고, 대립가설을 채택


########### 단일모집단의 모평균에 대한 신뢰구간 추정
# 1) 표본이 크고 정규성 충족시 단일모집단의 모평균에 대한 신뢰구간 추정과 검정
# t.test()

x1 <- c(33:93)
plot(x1, dnorm(x1, mean=63, sd=8), type = 'l')
abline(v=63, col ="blue", lty = 3)
abline(v=63 + 1.96*8, col ="red", lty = 3)


# x : random sample of 15 students's weight in high scool 
x <- c(70.2, 54.9, 67.0, 60.5, 63.4, 61.9, 71.8, 66.1, 72.6, 73.0, 68.7, 70.3, 66.2, 55.6, 65.9)
mean(x)
var(x)
sd(x)
stem(x) #stem - and - leaf plot
# 단측검정
t.test(x, alternative = c("greater"), mu=63, conf.level = 0.95) # 귀무가설기각

# 양측검정
t.test(x, alternative = c("two.sided"), mu=63, conf.level = 0.95) # 귀무가설 기각 못함


