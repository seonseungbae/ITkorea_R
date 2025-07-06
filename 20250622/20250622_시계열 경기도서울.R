#4. 데이터 전처리
# 이상치 제거
# 디플리알
library(dplyr)  # 디플리알 불러오기

# 기온 이상치 판단

#1. 이상치 판단 방법 : z-score, IQR
# z-score(평균, 표준편차, 성적 등) : 키, 몸무게 등 정규분포에 많이 사용
# IQR(사분위수) : 근로소득, 강수량, 기온 등을 비대칭데이터를 분석할때 사용

# IQR을 이용해서 이상치 판단(6월 22일 배운것임. 처음 배움)
# 데이터를 크기순서대로 줄 세웠을때, 가운데 50%가 얼마나 퍼져 있는지 알려줌.

Q1 = quantile(weather$temp, 0.25, na.rm = TRUE)
Q3 = quantile(weather$temp, 0.75, na.rm = TRUE)

IQR = Q3- Q1 
# 1.5의 의미는 통계학자 존 튜키가 제안한 것으로 너무 좁지도, 넓지도 않은 적절한
# 범위를 설정하기 위해 표준으로 사용하고 있다.
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR

weather = weather %>%
  mutate(temp = ifelse(temp < lower_bound | temp > upper_bound, NA, temp))

# 이상체 데이터 개수 확인
# 기온데이터가 NA라는것은 이상치라는뜻