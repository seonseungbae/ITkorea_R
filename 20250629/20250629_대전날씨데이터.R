# csv 파일 불러오기

#print(getwd())  #폴더조회

# print(list.files())  #폴더에 저장된 파일 조회
#애러가 뜰때
#setwd('C:\Users\admin\Desktop\data')  # 강제로 경로지정할 수가 있다.
# print(getwd())  #폴더조회
# print(list.files())  #폴더에 저장된 파일 조회

# setwd('C:\Users\admin\Desktop\r_workspaces\data')

# csv 파일 불러오기

weather = read.csv('OBS_ASOS_TIM_20250629105623.csv', na.strings = c(""), fileEncoding = 'CP949', 
                   encoding = 'UTF-8', check.names = FALSE)
str(weather)
# 9개 컬럼명(벡터명)을 한글에서 영어로 수정
# col : 컬럼
colnames(weather) = c("station_id", "station_name", "datetime", "temp", "precip", "windspeed", 
  "winddir", "humidity","CA")
str(weather)

# 각 컬럼 결측값조회
print(colSums(is.na(weather)))
# ifelse 이용해서 결측값을 0으로 대체함.(아래)
weather$precip = ifelse(is.na(weather$precip), 0, weather$precip)
print(colSums(is.na(weather)))  # 최종확인

# datetime 문자에서 날짜형으로 변환
# 수집한 날짜 데이터에 시간, 분, 초 가 있으면 as.Date로 변환시 생략됨.
weather$datetime = as.POSIXct(weather$datetime, format='%Y-%m-%d %H:%M')

# 산점도

plot(weather$temp, weather$humidity, col = c('red','blue'),
     xlab='온도', ylab='습도', main = '온도와 습도의 관계')

# 범례 추가
legend('topright', legend = unique(weather$station_name), col=c('black')
       , pch=19)  # 우상단 : 대전

# 클러스터링 시각화
# 클러스터 : 덩어리, 군집, 무리 라는 뜻임.
# 클러스터 + ing : 군집을 형성하다 라는 뜻임.
# 예를들면, 비슷한 날짜 패턴을 가진 시점을 그룹화하는 것을 말함.

# Kmeans(K평균 군집화  : k는 갯수를 말함.)
# 기온하고 습도 열만 선택  [ ] 를 활용

weather_var = weather[, c('temp', 'humidity')]
set.seed(123)  # 항상 똑샅은 결과가 나오게 해주라는 명령어

# 데이터를 3개의 그룹으로 나눠주라
clusters = kmeans(weather_var, centers = 3)

# 군집 결과 시각화
plot(weather$temp, weather$humidity, col=clusters$cluster,
     xlab='온도', ylab='습도', main = '날씨와 습도 클러스터링')

# 풍향과 풍속 시각화

library(openair)   # ctrl+s를 누르면 상단에 노랑색 바가 생기는데 이때 install 누름
# 바람데이터를 시각화 할때 사용
windRose(weather, ws = 'windspeed', wd = 'winddir')  # 풍향과 풍속시각화

# 풍속과 습도의 상관관계
print(cor(weather$windspeed, weather$humidity))  # -0.3616063(음의 관계)

# 온도 변화 시계열 그래프 그리기
# 시계열 : 시간의 흐름에 따라 연속적으로 수집된 데이터
# 시계열 DB가 따로 있다. 주의할 것
plot(weather$datetime, weather$temp, type='l', col='blue',
     xlab='날짜', ylab='온도', main='날짜별 온도 변화')

















