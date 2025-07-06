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


# 데이터 요약
# summary : 내가 분석하고자 하는 컬럼 통계확인

print(summary(weather$humidity))
print(table(weather$station_name))   #  지점별 데이터 수 확인
print(nrow(weather))  # 데이터프레임 총 행의 수

# 데이터 형변환
weather$datatime = as.POSIXct(weather$datatime, format = '%Y-%m-%d %H:%M')
weather$station_name = as.factor(weather$station_name)

# 강수량, 전운량 결측값 0으로 대체
weather$precip = ifelse(is.na(weather$precip), 0, weather$precip)
weather$CA = ifelse(is.na(weather$CA), 0, weather$CA)
print(colSums((is.na(weather))))

# 표준편차, 상관관계 패스 :  ????


# 온도와 습도의 상관관계 표현
colors = rainbow(6) # 지점 총 6개

# 시각화
plot(weather$temp, weather$humidity, col=colors, xlab='온도', ylab='습도',
     main='온도와 습도의 관계')
# pch는 점크기(서울 옆에있는 점크기)
# cex = 0.8은 범례 크기를 80%로 줄인 것이다.
legend('topright', legend=unique(weather$station_name),
       col=colors, pch=19, cex=0.8)

# 풍속과 습도 k평균 군집화 시각화

weather_var = weather[,c('windspeed', 'humidity')]
set.seed(123)  # 항상 같은결과가 나오게 설정
clusters = kmeans(weather_var, centers = 3)
plot(weather$windspeed, weather$humidity, col=clusters$cluster)

# 온도변화 시계열 그래프
weather$dare = as.Date(weather$datatime)
plot(weather$datatime, weather$temp, type='l', col='blue',
     main='날짜별 온도변화')
# 아래 매월이 보이게 하기
month = seq(as.Date('2025-01-01'), as.Date('2025-07-01'), by="month")
axis.Date(1, at=month[month<= max(weather$datatime)],format='%Y-%m', las=1, cex.axis = 0.7)

# 데이터 전처리
# 각 지점별로 평균기온 구하기
mean_temp_by_station = weather %>% group_by(station_name) %>%
        summarise(평균기온 = mean(temp, na.rm = TRUE))

# 풍속이 3이상 데이터만 골라서, 평균 습도
mean_hum_high_wind = weather %>% filter(windspeed >= 3) %>%
  summarise(평균습도 = mean(humidity, na.rm = TRUE))

# 3월부터 5월까지 서울 평균 강수량, 최대, 최소 강수량
seoul_data = weather %>% filter(datatime >= '2025-03-01'&
                                  datatime <= '2025-05-31'&
                                  station_name == '서울') %>%
  summarise(평균강수량 = mean(precip, na.rm = TRUE),
            최대강수량 = max(precip, na.rm = TRUE),
            최소강수량 = min(precip, na.rm = TRUE))
print(seoul_data) # 0.1173455         13          0

# 방법 2

seoul_data2 = weather %>% mutate(month = format(datatime, '%m')) %>%
    filter(month %in% c('03', '04', '05') & station_name == '서울') %>%
      summarise(평균강수량 = mean(precip, na.rm = TRUE),
                최대강수량 = max(precip, na.rm = TRUE),
                최소강수량 = min(precip, na.rm = TRUE))
# 평균강수량 최대강수량 최소강수량 1  0.1173455         13          0

# 각 지점별 기온이 가장 높았던 시간대와 그 값을 구하고 내림차순
max_temp_by_station = weather %>% group_by(station_name) %>%
  filter(temp == max(temp, na.rm = TRUE)) %>%
  select(station_id, station_name, datatime, temp) %>%
  arrange(desc(temp))

max_temp_by_station2 = weather %>% group_by(station_name) %>%
  select(station_id, station_name, datatime, temp) %>%
  slice_max(temp, n = 1) %>% arrange(desc(temp))
print(max_temp_by_station2)



# 날짜별 습도 평균 구하기
mean_hum_by_data = weather %>% mutate(data = as.Date(datatime)) %>%
  group_by(data) %>%
  summarise(평균습도 = mean(humidity, na.rm = TRUE))
print(mean_hum_by_data)


# ts : 시계열 타입으로 형 변환해야 arima 시계열모델에 넣을 수 있다.
# 매우중요함. arima : 시계열 분석의 대표 미래예측 모델
humidity_ts = ts(mean_hum_by_data$평균습도, frequency = 30)
str(humidity_ts)
# Time-Series [1:175] from 1 to 6.8: 57.8 59.6 38.4 48.7 49.9 ...
library(forecast)
auto_model = auto.arima(humidity_ts)
#  h = 에 올바른 값 넣기
forecasted = forecast(auto_model, h = 10) # 향후 10일 습도 예측

predict_data = data.frame(
  time = as.numeric(time(forecasted$mean)),
  forecast = as.numeric(forecasted$mean),
  lower = as.numeric(forecasted$lower[,2]),  # 95% 신뢰구간 하한
  upper = as.numeric(forecasted$upper[,2])   # 95% 신뢰구간 상한
)
actual_data = data.frame(
  time = as.numeric(time(humidity_ts)),
  temp = as.numeric(humidity_ts)
)

library(ggplot2) #고급 시각화

line_plot = ggplot() +
  # 실제값 선그래프
  geom_line(data = actual_data, aes(x = time, y = temp), color = "steelblue", size = 1) +
  
  # 예측 신뢰구간 리본
  geom_ribbon(data = predict_data, aes(x = time, ymin = lower, ymax = upper), 
              fill = "orange", alpha = 0.3) +
  # 예측값 선그래프
  geom_line(data = predict_data, aes(x = time, y = forecast), color = "red",
            size = 1.2, linetype = "dashed") +
  labs(
    title = "습도 예측",
    x = "Time",
    y = "습도"
  ) +
  theme_minimal()

print(line_plot)
















