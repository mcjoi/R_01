


## 차트 바꾸기
my_par = par(no.readonly = T)
par(oma = c(0, 0, 1, 0))
par(mfrow = c(1,2))





# 1. 정규분포 : norm
# 샘플의 개수가 증가하면, 정규분포에 근사하게 됨. (중심 극한 정리)

x <- seq(-3, 3, length = 2000)
plot(x, dnorm(x, mean = 0, sd = 1), type = 'l')
plot(x, pnorm(x, mean = 0, sd = 1), type = 'l')

# 정규 분포의 누적분포함수
# P( -1 <= X <= 1)
pnorm(q=c(1), mean=0, sd=1)
pnorm(q=c(-1), mean=0, sd=1)
pnorm(q=c(1), mean=0, sd=1) - pnorm(q=c(-1), mean=0, sd=1)

# P( -2 <= X <= 2)
pnorm(q=c(2), mean=0, sd=1)
pnorm(q=c(-2), mean=0, sd=1)
pnorm(q=c(2), mean=0, sd=1) - pnorm(q=c(-2), mean=0, sd=1)

# P( -3 <= X <= 3)
pnorm(q=c(3), mean=0, sd=1)
pnorm(q=c(-3), mean=0, sd=1)
pnorm(q=c(3), mean=0, sd=1) - pnorm(q=c(-3), mean=0, sd=1)

# lower.tail = FALSE
pnorm(q=c(1), mean=0, sd=1, lower.tail = T)
pnorm(q=c(1), mean=0, sd=1, lower.tail = F)

# 분위수 함수
pnorm(q=c(1), mean=0, sd=1, lower.tail = T) # 누적분포함수
qnorm(p=0.8413447, mean = 0, sd=1, lower.tail = T) # 분위수함수

# 정규분포 난수
rnorm_100 = rnorm(1000000, mean = 0, sd=1)
hist(rnorm_100)


# 2. 균등분포 : unif
# 균등분포 그래프 
library(ggplot2)
# uniform distribution plot(min = 0, max = 10)
ggplot(data.frame(x=c(-2,20)), aes(x=x)) + 
  stat_function(fun=dunif, args=list(min = 0, max = 10), colour="black", size=1)

# 누적 균등분포 그래프
ggplot(data.frame(x=c(-2,20)), aes(x=x)) + 
  stat_function(fun=punif, args=list(min = 0, max = 10), colour="black", size=1)


# 누적 균등분포 함수의 확률 계산
punif(3, min = 0, max = 10, lower.tail = T)

ggplot(data.frame(x=c(-2,20)), aes(x=x)) +
  stat_function(fun=dunif, args=list(min = 0, max = 10), colour="black", size=1) +
  annotate("rect", xmin=0, xmax=3, ymin=0, ymax=0.1, alpha=0.2, fill="yellow") 

# 균등분포 분위수 함수 값 계산
qunif(0.4, min = 0, max = 10, lower.tail = T)

# 난수 발생
ru_100 <- runif(n=100, min=0, max=10)
t_ru_100 <- table(ru_100)
hist(ru_100)


# 3. 지수분포 : exp
# 어떤 사건이 발생하기까지의 시간, ex) 콜센터에서 전화가 걸려오는 시간 등
dexp(x = 0, rate = 1)
# 지수분포 그래프
ggplot(data.frame(x=c(0,10)), aes(x=x)) +
  stat_function(fun=dexp, args=list(rate=1), colour="brown", size=1.5) +
  ggtitle("Exponential Distribution")

# 누적지수분포
pexp(q = 11, rate =1)
# 누적지수분포그래프
ggplot(data.frame(x=c(0,10)), aes(x=x)) +
  stat_function(fun=pexp, args=list(rate=1), colour="brown", size=1.5) +
  ggtitle("Cumulative Exponential Distribution")

pexp(q = 2, rate = 1, lower.tail = T)
qexp(p=0.8646647, rate = 1, lower.tail = T)


# 지수분포의 난수발생
rexp(100, rate = 1)
hist(rexp(100, rate = 1), breaks = 10)


# dexp(x, rate, log = T)
# log = T 옵션을 설정하면, 지수분포의 확률밀도 값의 밑이 e(2.17)인 자연 로그
# ln을 취한 값을 계산합니다. (자연로그인 ln의 역함수는 지수함수인 exp())

# X가 모수, 람다가 1인 지수분포를 따른다고 했을때, X:1,2,3... 10의 확률밀도함수값을
# 계산한 것이다. 
# log=T 옵션을 취한 것과 log(dexp)라는 수식을 입려해서 계산한 값이 동일하다.
dexp <- dexp(c(1:10), rate = 1)
dexp_log <- dexp(c(1:10), rate = 1, log=T)

exp_df = data.frame(cbind(c(1:10), dexp, dexp_log))
exp_df
exp_df <- transform(exp_df, dexp_logarithm = log(dexp))
exp_df




plot(dexp)
plot(dexp_log)

mtext("density function of exponential distributin : log = FALSE vs. log = TRUE",
      outer = TRUE, cex = 1.2)




# 4. t분포
# 정규분포에서는 모분산(σ2)을 알고 있다고 가정하는데요, 
# 실전의 현실세계에서는 모분산(σ2)을 모르는 경우가 대부분이다보니 
# 표본을 추출해서 표본분산(s2)을 계산하여 사용하는 경우가 다반사입니다.  
# 이러다 보니 표준정규분포 통계량 Z 값을 사용할 수 없고 
# 표본확률분포를 사용해야 하는데 그중의 하나가 T통계량을 사용하는 t-분포가 되겠습니다.

# t-분포는 평균이 0 이고, 
# 평균을 중심으로 좌우 대칭형태로 되어있으며, 
# 정규분포보다 가운데의 높이가 조금 낮고 좌우의 옆 부분은 
# 정규분포보다 조금 더 높은 형태를 취하고 있습니다.  
# t-분포는 자유가를 모수로 가지고 있으며, 자유도가 높을 수록, 
# 즉 표본의 갯수가 증가할 수록 
# 중심극한의 정리(central limit theorem)에 의해 정규분포에 근사하게 됩니다.  
# (보통은 표본의 갯수 30개를 기준으로 이보다 많으면 정규분포, 적으면 t-분포를 사용)
# 표본의 수가 많으면 모집단을 대표할 신뢰도가 높아지게 되어 좋기는 합니다만, 
# 많은 경우는 비용과 시간의 한계로 인해서 한정된 수의 표본만을 추출해서 
# 분석해야 하는 경우가 생기게 됩니다.  
# 표본의 수가 적은 경우에 표본이 모집단의 어느 한쪽으로 쏠려서 
# 추출될 경우 모집단을 잘 대표할 수 없는 신뢰도 이슈를 보완하기 위해서, 
# 정규분포일 때보다 평균에서 양쪽으로 멀어지는 바깥쪽 부분의
# 확률의 수준을 더 높인 분포가 t-분포(t-distribution)이 되겠습니다.

# 정규 분포와 T 분포를 겹쳐서 그려보자
ggplot(data.frame(x=c(-3,3)), aes(x=x)) +
     stat_function(fun=dnorm, colour="blue", size=1) +
     stat_function(fun=dt, args=list(df=3), colour="red", size=2) +
     stat_function(fun=dt, args=list(df=1), colour="yellow", size=3) +
     annotate("segment", x=1.5, xend=2, y=0.4, yend=0.4, colour="blue", size=1) +
     annotate("segment", x=1.5, xend=2, y=0.37, yend=0.37, colour="red", size=2) + 
     annotate("segment", x=1.5, xend=2, y=0.34, yend=0.34, colour="yellow", size=3) + 
     annotate("text", x=2.4, y=0.4, label="N(0,1)") +
     annotate("text", x=2.4, y=0.37, label="t(3)") + 
     annotate("text", x=2.4, y=0.34, label="t(1)") + 
     ggtitle("Normal Distribution, t-distribution")



# 정규분포(normal distribution)는 평균(mean)과 표준편차(sd)를 parameter로 사용
# 균등분포(uniform distribution)는 구간의 최소값(min)과 최대값(max)를 parameter로 사용
# 지수분포(exponential distribution)는 Lamda(λ, rate)를 parameter로 사용
# t-분포(t-distribution)은 자유도(df, degrees of freedom) 를 parameter로 사용

# 자유도 (df, degrees of freedom)은 통계량을 구성하는 확률변수들 중에서
# 자유롭게 선택가능한 확률변수의 개수를 의미

ggplot(data.frame(x=c(-3,3)), aes(x=x)) +  
  stat_function(fun=pt, args=list(df=1), colour="brown", size=1.5) + 
  ggtitle("Cumulative t-Distribution : t(1)")

# t분포 누적분포함수(pt(q, df, lower.tail = T/F))
pt(q = 1, df = 1, lower.tail = T)

# t분포 분위수함수(qt(p, df, lower.tail = T/F))
qt(p=0.75, df = 1, lower.tail = T)

# t분포 난수 발생
rt <- rt(50, df=1)
hist(rt, breaks = 20)


# 5. F분포
# 표본분포를 나타낼 때 t-분포, F-분포, 카이제곱 분포 등을 사용하는데요, 
# F-분포는 F-검정 (F-test)과 두 집단 이상의 분산이 같은지 여부를 비교하는
# 분산분석(ANOVA, Analysis of Variance)에 사용되며, 
# (카이제곱 분포처럼) 분산을 제곱한 값만을 사용하므로 양(+)의 값만을 가지고 되고, 
# 왼쪽으로 치우치고 오른쪽으로 꼬리가 긴 비대칭형 형태를 띠고 있습니다.  
# (카이제곱 분포와 모양이 유사함)
df(x = c(0,5), df1 = 5, df2 = 10)

ggplot(data.frame(x=c(0,5)), aes(x=x)) +
     stat_function(fun=df, args=list(df1=5, df2=10), colour="blue", size=1) +
     stat_function(fun=df, args=list(df1=10, df2=30), colour="red", size=2) +
     stat_function(fun=df, args=list(df1=50, df2=100), colour="yellow", size=3) +
     annotate("segment", x=3, xend=3.5, y=1.4, yend=1.4, colour="blue", size=1) +
     annotate("segment", x=3, xend=3.5, y=1.2, yend=1.2, colour="red", size=2) + 
     annotate("segment", x=3, xend=3.5, y=1.0, yend=1.0, colour="yellow", size=3) + 
     annotate("text", x=4.3, y=1.4, label="F(df1=5, df2=10)") +
     annotate("text", x=4.3, y=1.2, label="F(df1=10, df2=30)") + 
     annotate("text", x=4.3, y=1.0, label="F(df1=50, df2=100)") +
     ggtitle("F Distribution")

# f누적 확률 그래프
ggplot(data.frame(x=c(0,5)), aes(x=x)) +
     stat_function(fun=pf, args=list(df1=5, df2=10), colour="blue", size=1) +
     ggtitle("Cumulative F-distribution : F(df1=5, df2=10)")

# f분포 누적확률값 계산
pf(q = 2, df1 = 5, df2 = 10, lower.tail = T)

#f분포 분위수 함수 값 계산
qf(p = 0.835805, df1 = 5, df2 = 10, lower.tail = T)

#f분포 난수 발생
rf <- rf(100, df1 = 5, df2 = 10)
hist(x = rf, breaks = 20)




# 6. 카이제곱분포
# 카이제곱 분포 (chi-squared distribution)은 
# k개의 서로 독립적이고 표준정규분포를 따르는 확률변수 X 를 
# 제곱한 값들을 합하였을 때의 분포이며, 
# 이때 k 는 자유도 (degrees of freedom) 로서 카이제곱 분포의 parameter 가 됩니다.

# 카이제곱 분포는 표본분포 중의 하나이며, 
# 정규분포와도 관련이 있다고 했는데요, 
# 카이제곱 분포 (chi-squared distribution)은 
# 모집단의 분산에 대한 추정과 검정에 활용됩니다.  
# 그리고 F-분포와도 관련이 있는데요, 
# F-분포가 카이제곱 분포를 따르는 두 개의 확률변수를 가지고 
# 자유도 2개를 parameter로 사용하는 분포이구요, 
# 카이제곱 분포는 정규분포를 따르는 확률변수를 가지고 제곱해서 
# 합한 값의 분포로서 자유도 1개만을 parameter로 사용하는 분포가 되겠습니다.

# 카이제곱 분포를 활용해서 집단 독립성 여부 가설에 대해 검정하는
# 예도 마지막에 들어보겠습니다(이것이 카이제곱 분포 공부하는 이유! ^^).

# 카이제곱분포 그래프
ggplot(data.frame(x=c(0,10)), aes(x=x)) +
     stat_function(fun=dchisq, args=list(df=1), colour="black", size=1.2) +
     geom_text(x=0.6, y=1, label="df=1") +
     stat_function(fun=dchisq, args=list(df=2), colour="blue", size=1.2) +
     geom_text(x=0, y=0.55, label="df=2") +
     stat_function(fun=dchisq, args=list(df=3), colour="red", size=1.2) +
     geom_text(x=0.5, y=0.05, label="df=3") +
     ggtitle("Chisq-Distribution")

#df가 3인 경우 f분포의 그래프와 유사


#누적 카이제곱분포 그래프
ggplot(data.frame(x=c(0,10)), aes(x=x)) +
  stat_function(fun=pchisq, args=list(df=1), colour="black", size=1.2) + 
  geom_text(x=2.5, y=0.93, label="df=1") + 
  stat_function(fun=pchisq, args=list(df=2), colour="blue", size=1.2) +
  geom_text(x=2.5, y=0.77, label="df=2") +
  stat_function(fun=pchisq, args=list(df=3), colour="red", size=1.2) +
  geom_text(x=2.5, y=0.45, label="df=3") +
  ggtitle("Cumulative Chisq-Distribution")

#누적 카이제곱분포 확률 값 계산
pchisq(q = 2.5, df = 1, lower.tail = T) # 0.88
pchisq(q = 2.5, df = 2, lower.tail = T) # 0.71
pchisq(q = 2.5, df = 3, lower.tail = T) # 0.52

# 카이제곱 분포 분위수 계산
qchisq(p=0.8861537, df = 1, lower.tail = T)
qchisq(p=0.7134952, df = 2, lower.tail = T)
qchisq(p=0.5247089, df = 3, lower.tail = T)

#카이제곱분포의 난수발생
rchisq <- rchisq(n=100, df=2)
hist(rchisq, breaks = 20)

# 범주형 데이터에 대한 분할표와 카이제곱 검정
# 신약 테스트 결과 불러오기

install.packages("gmodels")
install.packages("vcd")

library(gmodels)
library(vcd)

str(Arthritis)
attach(Arthritis)
CrossTable(Treatment, Improved, 
           expected = T, 
           chisq = T)
detach(Arthritis)

# p-value 는 p =  0.001462643, 효과가 없다는 귀무가설을 기각,
# == 두 집단이 동일하다는 귀무가설을 기각, 즉, 독립이 아니다. 
