weather = read.csv('경기도서울.csv',           #csv파일이름
                   na.strings = c(""),
                   fileEncoding = 'CP949',          #한글깨짐 방지
                   encoding = 'UTF-8',      #한글깨짐 방지
                   check.names = FALSE)

colnames(weather) = c('station_id', 'station_name', 'datatime', 'temp', 'precip','windspeed', 'winddir', 'humidity', 'CA')

str(weather)

library(dplyr)

weather_daily = weather %>% mutate(date = as.Date(datatime)) %>%
  group_by(date) %>% summarise(temp_avg = mean(temp, na.rm =TRUE))


print(head(weather_daily)) 

# 시계열 타입으로 변환하기 (매우중요함)
# 변환하는 이유는 예측모델 대부분이 시계열 타입으로 받기때문이다.
# frequency = 30, 한달에 30일 있다고 가정(지금은 데이터가 30일치만 있다.)
#만약, 1년치면 365
temp_ts = ts(weather_daily$temp_avg, frequency = 30)  # 시계열 타입
print(head(temp_ts))   # 타입만 바뀌고 print(head(weather_daily)) 값이 같게나옴.

str(temp_ts)


library(forecast)

# arima, 시계열 데이터를 분석하고, 미래 값을 예측하는데 사용되는 통계 모델임.
auto_model = auto.arima(temp_ts)

# 향후 30일 예측해보기, 위의 h = 30일

forecasted = forecast(auto_model, h = 30)  #  주별로 했으면 30주란 의미임.

print(forecasted)  # 타임 시리즈 : 인덱스번호,신뢰구간과 예측한 온도 데이터

# 예측데이터와, 실제 데이터를 데이트프레임으로 만들어서 시각화

predict_data = data.frame(
  time = as.numeric(time(forecasted$mean)),
  forecast = as.numeric(forecasted$mean),
  lower = as.numeric(forecasted$lower[,2]),  # 95% 신뢰구간 하한
  upper = as.numeric(forecasted$upper[,2])   # 95% 신뢰구간 상한
)
print(predict_data)

actual_data = data.frame(
  time = as.numeric(time(temp_ts)),
  temp = as.numeric(temp_ts)
)

library(ggplot2)

line_plot = ggplot() +
  # 실제값 선그래프
  geom_line(data = actual_data, aes(x = time, y = temp), color = "steelblue", size = 1) +
  # 예측 신뢰구간 리본
  geom_ribbon(data = predict_data, aes(x = time, ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  # 예측값 선그래프
  geom_line(data = predict_data, aes(x = time, y = forecast), color = "red", size = 1.2, linetype = "dashed") +
  labs(
    title = "Temperature Forecast",
    x = "Time",
    y = "Temperature"
  ) +
  theme_minimal()

print(line_plot)


# 예측값과 실제값의 차이가 얼마나 날까?
# 신뢰가 가능한가?

#MAE(모델 성능평가)

actual = weather_daily$temp_avg
actual_length = length(actual)  # 데이터 길이
cat('actual_leanth: ', actual_length, '\n')   # length = 172


predicted = as.numeric(forecasted$mean)
predicted_length = length(predicted)
cat('predicted_length: ', predicted_length, '\n')  
# actual_leanth:  172 / predicted_length:  30 

#MAE(모델 성능평가) : 예측이 얼마나 잘 맞았는지 쉽게 알려주는 함수
# 예를들어, 내일 기온이 25도 일꺼야 예측을 했는데 실제 27도
# 오차 2도, 이런 오차들을 모두 더해서 평균을 내면 MAE가 됩니다.

MAE = mean(abs(actual[1:predicted_length] - predicted))

cat('MAE : ', MAE, '\n')   # MAE :  24.91617 

# 예측값과 실제값의 차이가 평균적으로 24도라는 뜻
# 결과값 actual_leanth:  172 predicted_length:  30 MAE :  24.91617 

