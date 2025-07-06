data = data.frame(
  name = c('a','b','c','d','e','f'),   #벡터(엑셀 열)
  heights  = c(120, 130, 140, 150, 160, 200)  # 벡터
)

print(data$name)   # $ : 열접급

print(data[,'name'])  # "a" "b" "c" "d" "e" "f"
print(data[3, c('name', 'heights')])  # 콤마를 기준으로 왼쪽은 행, 오른쪽 열임.

data$birth = c('1999-01-01','1999-01-01','1999-01-01','1999-01-01','1999-01-01','1999-01-01')
# 데이터 구조 확인 $ birth  : chr  "1999-01-01"  이것은 문자이다.
str(data)  
# 문자를 날짜형으로 변환
# 데이터가 연/월/일만 있을때 # birth  : Date, format: "1999-01-01" 
# $ : 조회, 수정, 추가
# [] : 조회(행,열)
data$birth = as.Date(data$birth)  # birth  : Date, format: "1999-01-01" 
str(data)
# 이상치 제거 : IQR
# IQR => 소득, (비대칭데이터)이런 데이터를 탐지할 때 사용
#1. Q1과 Q3을 구하기 변수이름 
Q1 = quantile(data$heights, 0.25) # 하위 25% Q1: 132.5 Q3: 157.5
cat('Q1:', Q1, '\n')
Q3 = quantile(data$heights, 0.75) # 하위 75%
cat('Q3:', Q3, '\n')  
  
#2.IQR(사분위 범위) 구하기
#프로그래밍에서 변수이름이 대문자는 수정하지 마라
IQR_VALUE = Q3 - Q1   # 대문자로 표기시 수정하지 마라 의미(조회만해라)
cat('IQR_VALUE:', IQR_VALUE, '\n')

#.3 이상치 기준선 만들기   
# 변수이름이 소준자이면 수정해도돼 라는 의미
# 1.5는 존 튜키, 통계학자가 제안한것으로 표준 기준으로 사용하고 있음.
lower_bound = Q1 - 1.5 * IQR_VALUE
upper_bound = Q3 + 1.5 * IQR_VALUE
# 키가 95 ~ 195가 아닌 학생들 데이터는 이상치로 판단
cat('lower_bound:', lower_bound, '\n')
cat('upper_bound:', upper_bound, '\n')
# IQR_VALUE: 25 lower_bound: 95 upper_bound: 195 
#4. 이상치 확인
library(dplyr)
outliers = data %>% filter(heights < lower_bound | heights > upper_bound)
print(outliers)      # f     200 1999-01-01 키 200이상 이상치

# 키 데이터 변환
# 데이터를 수집했는데 문자로 했다. 키 단위까지 했다.
data$heights  = c('120cm', '130cm', '140cm', '150cm', '160cm', '200cm')
str(data)
# $ heights: chr  "120cm" "130cm" "140cm" "150cm" ...
# 숫자로 변환할 때 아래와 같이.
#1. 문자열 처리 'cm'제거
# gsub : 문자에서 특정 패턴(cm)을 찾아 다른문자 ('')로 '대체'되어 있다.
# $ heights: chr  "120" "130" "140" "150" ...
data$heights = gsub('cm','',data$heights)
str(data) 
data$heights = as.numeric(data$heights)
str(data) 
print(sum(data$heights))  # 키 총합을 구하기 가능900(숫자로, 문자는 안됨.)










  
