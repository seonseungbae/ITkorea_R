# csv 파일 불러오기
weather = read.csv('주요도시날씨데이터.csv',
                   na.strings = c(""),
                   fileEncoding = 'CP949', # 한글 데이터 파일은 인코딩이 맞지 않으면 깨질 수 있음.
                   encoding = 'UTF-8', 
                   check.names = FALSE)
print(weather)
print(getwd())
print(list.files())
# setwd('C:/Users/admin/Desktop/r_workspaces/data')
# print(getwd())  #폴더조회
#print(list.files())  #폴더에 저장된 파일 조회
#애러가 뜰때
#setwd('')  # 강제로 경로지정할 수가 있다.

#1. 데이터구조와 변수확인
str(weather)

#1-1 head로 상위 데이터 확인
print(head(weather))
print(tail(weather))
print(nrow(weather))
print(summary(weather))
print(table(weather$지점명))

#1-2 데이터 한글을 영어로 수정

colnames(weather) = c('station_id', 'station_name', 'datatime', 'temp', 
                      'precip', 'windspeed', 'winddir', 'humidity', 'CA')
print(head(weather))

#1-3 데이터 형 변환

weather$datatime = as.POSIXct(weather$datatime, format = '%Y-%m-%d %H:%M')
str(weather)

#1-4 결측값 조회 및 대체

print(colSums(is.na(weather)))

weather$precip = ifelse(is.na(weather$precip), 0, weather$precip)
print(colSums((is.na(weather))))

# STEP 3 : 기초 통계량 확인 및 시각적 탐색(EDA)
#3-1 풍속, 풍향, 습도의 통계량을 확인합니다.

print(summary(weather$windspeed))
print(summary(weather$winddir))
print(summary(weather$humidity))

#3-2 습도의 표준편차와 변동계수 구하기 # hint sd, mean
humidity_sd = sd(weather$humidity, na.rm = TRUE)
print(humidity_sd)   # 21.64158  표준편차

#변동계수

humidity_avg = mean(weather$humidity, na.rm = TRUE)
CV = (humidity_sd / humidity_avg) * 100
cat('습도 변동계수 :', CV, '\n')    # 36.34135


# 풍속과 습도의 상관관계 조회
# hint : cor

cor_mat = cor(weather[, c('temp', 'windspeed', 'winddir', 'humidity', 'CA')], use='complete.obs')
print(cor_mat)  

# 그래프로 해당 상관관계 표현하기, 단일변수 시각화, 두변수간의 관계 시각화
# 기온, 풍속, 풍향, 습도, 전운량 상관관계 확인 및 시각화
library(corrgram)
corrgram(cor_mat, main = '기온,풍속,풍향,습도,전운량 상관계수',
         lower.panel = panel.shade,
         upper.panel = panel.cor)


# 온도와 습도 데이터 분포를 히스토그램으로 표현
# hint : hist

hist(weather$temp, main ='온도데이터 분포', xlab='온도')
hist(weather$humidity, main ='습도데이터 분포', xlab='습도')

# 풍속의 이상치를 탐색하기 위해 박스플롯 표현
# hint : boxplot

boxplot(weather$windspeed, main = '풍속 박스플롯', ylab ='풍속')

# 온도와 습도의 상관관계 표현, 도시 별 색깔 다르게 지정. 단, 범례도 추가할 것
# hint : plot
# 범례추가 : legend

plot(weather$temp, weather$humidity, col = c('red','blue'), 
     xlab='온도', ylab='습도' , main = '온도 vs 습도')

legend('topright', legend = unique(weather$station_name), col=c('black')
       , pch=19)

# 풍속과 풍향 시각화
# hint : windRose


library(openair)

windRose(weather, ws = 'windspeed', wd = 'winddir')  # 풍향과 풍속시각화

# 풍속과 습도 K 평균 군집화 시각화
# hint : kmeans

# Kmeans(K평균 군집화  : k는 갯수를 말함.)
# 기온하고 습도 열만 선택  [ ] 를 활용

weather_var = weather[, c('windspeed', 'humidity')]
set.seed(123)  # 항상 똑샅은 결과가 나오게 해주라는 명령어

# 데이터를 3개의 그룹으로 나눠주라
clusters = kmeans(weather_var, centers = 3)

print(cor(weather$windspeed, weather$humidity)) 

plot(weather$windspeed, weather$humidity, col=clusters$cluster,
     xlab='풍속', ylab='습도', main = '풍속과 습도 클러스터링')

# 온도 변화 시계열 그래프 그리기
# hint : plot, type="l"

plot(weather$datatime, weather$temp, type='l', col='blue',
     xlab='날짜', ylab='온도', main='날짜별 온도 변화')








