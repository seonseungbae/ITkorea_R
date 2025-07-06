# 정규분포
# 정규분포는 확률밀도 그래프의 한 종류로, 키, 점수처럼 연속적인 값의 
# 분포를 정규분포라고 함. 
# 정규분포의 반대는 이항분포(동전 앞 / 뒤 :  결과가 2개밖에 없는 것임.)
# 논문이나 레포트 할 때 많이 사용한다.

students = data.frame(
  name = c("Alice", "Bob", "Charlie", "David", "Eve", "Jose"),
  score = c(85, 92, 78, 95, 88, 200)
)

# 1번 평균하고 표준편차를 조회한다.
#평균
mu = mean(students$score)
# 표준편차
sigma = sd(students$score)

# 2번째 정규분포 곡선용 x, y 값 생성
# 최소 최대값(아래)
# length : 곡선을 부드럾게 그려주는 옵션
x = seq(min(students$score), max(students$score), length = 50)

# dnorm : destiny 와 normal drstribution(정규분포) 결합
# 주어진 평균과 표준편차를 갖는 정규분포에서 특정 x에 대한 확률밀도를 계산
# 즉, 데이터가 특정구간에 있을 확률을 말함.
y = dnorm(x, mu, sigma)

# 정규분포 시각화
# type = 'l'은 라인을 말한다. line
# plot(x, y, type = 'l', lwd = 2, col = 'blue', main = '학생 점수 정규분포'
#     , xlab = '점수', ylab = '확률')

# 조제 200점 때문에 정규분포 그래프가 이상하게 나옴.
# 실무에서는 그래프가 이상한 것을 확인이 되면, 이상치 제거를 해야함.
# 이상치 제거가 중요하고, 이것도 데이터 전처리 과정의 하나이다.
# 데이터의 분포에서 극단적으로 벗어난 값을 탐지하고, 처리하는 방법
# 그래서, 조제의 데이터를 제거해야 한다.

# Z-score 라는 알고리즘(이상치제거)을 사용하여 제거함.
# z스코어가 0이면 데이터 값이 평균과 동일함을 의미함.
# 만약, Z스코어가 양수이면, 데이터 값이 평균보다 크며, 양수 값이 클수록 
# 평균에서 멀리 떨어져 있음을 의미함을 이해해야함. 중요함.....!!!!


print(students)

# 1번 작업 : 평균과 표준편차를 먼저 구해야 한다.

# mean : 평균
mu = mean(students$score)
# sd : 표준편차
sigma = sd(students$score)

print(mu)   # 106.3333
print(sigma)  # 46.26302

# students$z_score =(students$score - mu) / sigma
# abs 절대값
students$z_score = abs((students$score - mu) / sigma)
print(students)

# 2 ~ 3(임계값) 조제의 값이 2.0246554가 나왔음. 양수값이 나옴.
# 따라서, 임계값을 상황에 따라 조절해야 하는 것임.

library(dplyr)

이상치제거_데이터 = students %>% filter(z_score < 2)  
# 2는 임계값으로 상황에따라 조절
print(이상치제거_데이터)    # 아래 콘솔창에서 조제가 사라졌음.


# 위의 이상치 제거 후 정규분포 그래프 시각화
mu = mean(이상치제거_데이터$score)
sigma = sd(이상치제거_데이터$score)
# x = seq(min(students$score), max(students$score), length = 50)

x = seq(min(이상치제거_데이터$score) - 10, max(이상치제거_데이터$score) + 10, length = 50)
y = dnorm(x, mu, sigma)

# x = seq(min(이상치제거_데이터s$score)-10, max(이상치제거_데이터$score)+10, length = 50)
# y = dnorm(x, mu, sigma)
# -10 / +10을 넣어 그래프를 이쁘게 만든다.

plot(x, y, type = 'l', lwd = 2, col = 'blue', main = '학생 점수 정규분포'
     , xlab = '점수', ylab = '확률')

# 중앙값, 상위 20%, 하위 20% 추가

med = median(이상치제거_데이터$score)
print(quantile(이상치제거_데이터$score))    # 사분위수 출력

q20 = quantile(이상치제거_데이터$score, 0.2)  # 하위 20
q80 = quantile(이상치제거_데이터$score, 0.8)  # 상위 20

# abline
# V는 수직선이다. lty는 점선이다.
abline(v=med, col = 'purple', lwd=2, lty=2)
abline(v=q20, col = 'orange', lwd=2, lty=2)
abline(v=q20, col = 'red', lwd=2, lty=2)

# 신뢰구간 추가하기
# 신뢰구간은 진짜 답이 있을 것 같은 범위를 말한다. 데이터 전체를 분석하지않고,
# 신뢰할 수 있는 데이터 범위를 분석함.


# 데이터 행의 수 조회
n = nrow(이상치제거_데이터)
print(n)   # 전체 행의 수 5

# 표준오차 : 내가 구한 평균값이 얼마나 믿을 만한지를 알려주는 숫자
# 표준오차가 작다 : 내가 구한 평균이 진짜 평균에 가까울 확률이 높다.
# 표준오차가 크다 : 내가 구한 평균이 진짜 평균과 많이 다를수 있다.
# 표준오차
# sqrt : 제곱근 
# 예를들어 sqrt(4) --> 2, sqrt(9) --> 3이 나옴
se = sigma / sqrt(n)
# 1.96? 통계분석에서 일반적으로 사용되는 신뢰수준 값 --> 95% 신뢰
ci_low = mu - 1.96*se
ci_high = mu + 1.96*se

# 신뢰구간 그래프 표현
abline(v=ci_low, col = 'black', lwd = 2)
abline(v=ci_high, col = 'black', lwd = 2)
















