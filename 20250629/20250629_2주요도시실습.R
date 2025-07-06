# csv 파일 불러오기
weather = read.csv('주요도시날씨데이터.csv',
                   na.strings = c(""),
                   fileEncoding = 'CP949', # 한글 데이터 파일은 인코딩이 맞지 않으면 깨질 수 있음.
                   encoding = 'UTF-8', 
                   check.names = FALSE)
str(weather)
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


library(dplyr)  # 디플리알 불러오기
# 풍속 IQR 이상치 제거
# hint : quantile
# 이상치 제거 : IQR
# IQR => 소득, (비대칭데이터)이런 데이터를 탐지할 때 사용
#1. Q1과 Q3을 구하기 변수이름 
Q1 = quantile(weather$windspeed, 0.25, na.rm=TRUE) 
Q3 = quantile(weather$windspeed, 0.75, na.rm=TRUE) 
IQR = Q3- Q1
lower_bound = Q1 - 1.5*IQR
upper_bound = Q3 + 1.5*IQR


# 각 지점별로 평균 기온을 구하시오. (dplyr)
# hint: group_by()와 summarise() 

station_name_avg_temp = weather %>%
  group_by(station_name) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE))
print(station_name_avg_temp)

# 풍속이 3 m/s 이상인 데이터만 골라서, 해당 데이터의 평균 습도를 구하시오. (dplyr)
# hint: filter()와 summarise()
windspeed = weather %>% filter(windspeed >= 3)
print(windspeed)

결과 = weather %>% filter(windspeed >=3) %>% group_by(station_name) %>%
  summarise(avg_humidity = mean(humidity))
print(결과)

#3월부터 5월까지 서울의 평균 강수량, 최대 강수량, 최소 강수량 구하시오. (dplyr)
# hint: filter()와 summarise()

결과 = weather %>% filter(precip) %>% 







