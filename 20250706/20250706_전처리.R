library(dplyr)

#csv
서울시_상권분석_데이터 = read.csv('서울시_상권분석서비스.csv', na.strings = c(""),
                        fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)
#View(서울시_상권분석_데이터)

# 24년도 서울시 행정동 코드명 별 평균 월소득 금액, 평균 지출총금액, 평균
# 교육지출 총금액 조회

서울시_24년도 = 서울시_상권분석_데이터 %>% filter(substr(기준_년분기_코드,1,4)
== '2024') %>% group_by(행정동_코드_명) %>%
  summarise(
    평균_소득금액 = mean(월_평균_소득_금액, na.rm = TRUE),
    평균_지출총금액 = mean(교육_지출_총금액, na.rm = TRUE),
    평균_교육지출금액 = mean(교육_지출_총금액, na.rm = TRUE)
  )
# View(서울시_24년도)

# 25년도 행정동 코드명 별 월_평균_소득_금액 평균 조회

서울시_25년도 = 서울시_상권분석_데이터 %>% filter(substr(기준_년분기_코드,1,4)
                                   == '2025') %>% group_by(행정동_코드_명) %>%
  summarise(
    평균_소득금액 = mean(월_평균_소득_금액, na.rm = TRUE),
  )
# substr 202502 중 2025까지만 출력하겠다.는 뜻 = 매우중요함.
year = '202502'
print(substr(year,1,4)) # 2025년만 출력(year,1,4의 의미) 202502 중 2025년만 나오게함.

# 2025년 히스토그램으로 시각화
par(mfrow = c(1,2))  # 1행 2열로 그래프 배치(단, 기본그패픽)
hist(서울시_25년도$평균_소득금액, col = 'skyblue', main='25년 행정동별 평균 소득분포',
     xlab = '평균소득(원)', ylab='수')

# 바플롯
barplot(서울시_25년도$평균_소득금액, names.arg = 서울시_25년도$행정동_코드_명,
        col = 'orange',
        xlab ='행정동명',
        ylab ='소득금액',
        main ='25년도 행정동별 평균소득')

# 박스플롯
#boxplot(서울시_25년도$평균_소득금액, names.arg = 서울시_25년도$행정동_코드_명,
#        col = 'orange',
#        xlab ='행정동명',
#        ylab ='소득금액',
#        main ='25년도 행정동별 평균소득')


# 24년도 4분기 지출 총금액 조회
서울시_24년도_4분기 = 서울시_상권분석_데이터 %>% 
  filter(기준_년분기_코드 == '20244')%>%
  select(기준_년분기_코드, 행정동_코드_명, 지출_총금액)
  
par(mfrow = c(1,1))
print(서울시_24년도_4분기)

# 박스플롯으로 시각화
#barplot(서울시_24년도_4분기$지출_총금액, names.arg = 서울시_24년도_4분기$행정동_코드_명,
 #               col = 'red',
  #              xlab ='행정동명',
   #             ylab ='지출금액',
    #            main ='24년도 4분기 행정동별 지출_총금액')
# 1e8 : 1뒤에 0이 8개 있다.
total_money = 서울시_24년도_4분기$지출_총금액 / 1e8

barplot(total_money, names.arg = 서울시_24년도_4분기$행정동_코드_명,
           col = 'red',
           xlab ='행정동명',
           ylab ='지출 금액(억 원)',
           main ='24년도 4분기 행정동별 지출_총금액(억 원 단위')        

# 교통지출과 문화지출간의 상관계수 조회

#1. 24년도 필터링

서울시_24년도 = 서울시_상권분석_데이터 %>% filter(substr
     (기준_년분기_코드,1,4) == '2024')

#2. 상관계수조회

cor_mat = cor(서울시_24년도[,c('교통_지출_총금액', 
          '여가_문화_지출_총금액')])
print(cor_mat)
# 상관계수가 0.3 ~ 0.7 중간, 0.7이상이면 강한상관관계, 0.3미만은 약한 관계

# 박스플롯 구현
# 행정동별 식료품 지출 총금액 박스플롯
boxplot(식료품_지출_총금액 ~ 행정동_코드_명, data = 서울시_24년도,
        main = '행정동별 식료품 지출 분포',
        xlab ='행정동',
        ylab ='식료품 지출 총금액',
        col = 'lightgreen')
# 박스플롯은 이상치를 확인할 수 있다. (IQR, Z-SCORE)

# 행정동별 유흥지출 박스플롯 조회
boxplot(유흥_지출_총금액 ~ 행정동_코드_명, data = 서울시_24년도,
        main = '행정동별 유흥 지출 분포',
        xlab ='행정동',
        ylab ='유흥 지출 총금액',
        col = 'yellow')
# 이상치가 거의 없다는 것 확인도 가능


# 교통지출 많은 곳, 문화지출 많은곳 지도 시각화

library(ggplot2)
library(ggiraph)
library(sf)
shp = 'sig.shp'
korea_map = st_read(shp, quiet = TRUE)  # 지도파일 불러오기
seoul_map = korea_map %>% filter(substr(SIG_CD,1,2) == '11')  # 서울지도만 조회

# 서울시 24년도 행정동 코드 문자로 형변환

서울시_24년도$행정동_코드 = as.character(서울시_24년도$행정동_코드)

merged_data = inner_join(seoul_map, 서울시_24년도, by = c('SIG_CD'= '행정동_코드')) %>% 
  group_by(SIG_CD, 행정동_코드_명) %>%
  summarise(평균교통지출 = mean(교통_지출_총금액, na.rm = TRUE)) %>%
  select(SIG_CD, 행정동_코드_명, 평균교통지출) %>%
  arrange(desc(평균교통지출)) %>%
  head(5)
print(merged_data)

plot1 = ggplot(merged_data) + 
  scale_fill_gradient(low = '#ececec', high = 'blue', name = '평균교통지출') +
  geom_sf_interactive(aes(fill = 평균교통지출,
                          tooltip = 행정동_코드_명,      # 마우스 색깔이 바뀌고 행정동이 보이는 코드
                          data_id = SIG_CD)) + theme_minimal()
labs(title = '서울시 평균 교통지출', x = '경도', y = '위도') 
giraph1 = girafe(ggobj = plot1)    
print(giraph1) 

# 24년 1분기 ~ 4분기 전체 문화지출이 가장 많은 행정동 TOP 5

merged_data2 = inner_join(seoul_map, 서울시_24년도,  by = c('SIG_CD' = '행정동_코드')) %>%
  group_by(SIG_CD, 행정동_코드_명) %>%
  summarise(문화지출 = mean(여가_문화_지출_총금액, na.rm = TRUE)) %>%
  select(SIG_CD, 행정동_코드_명, 문화지출) %>%
  arrange(desc(문화지출)) %>% 
  head(5)
plot2 = ggplot(merged_data2) +
  scale_fill_gradient(low = "#ececec", high = "blue", name = "문화지출") + 
  geom_sf_interactive(aes(
    fill = 문화지출,
    tooltip = 행정동_코드_명,
    data_id = SIG_CD
  )) +
  theme_minimal() +
  labs(title = "서울시 문화지출", x = "경도", y = "위도")
giraph2 = girafe(ggobj = plot2) 
print(giraph2)





















