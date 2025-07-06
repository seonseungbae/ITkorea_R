# 교육_지출_총금액 지도로 시각화 하기

library(sf)
library(ggplot2)
library(ggiraph)   # 지도 이벤트 패키지
# install.packages('ggiraph')
#1. 지도파일 불러오기

shp = 'sig.shp'
korea_map = st_read(shp, quiet = TRUE)

library(dplyr)
seoul_map = korea_map %>% filter(substr(SIG_CD,1,2) == '11')
str(seoul_map)  # 데이터 타입 확인

# 행정구역 코드(SIG_CD) 예시 종로구 ===> 11110

print(colnames(korea_map))  # 컬럼(벡터) 확인하기 

#csv
서울시_상권분석_데이터 = read.csv('서울시_상권분석서비스.csv', na.strings = c(""),
                        fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)

# shp파일에 있는 행정동 코드와 csv 서울시 행정동 코드 데이터타입이 서로 다르다.
서울시_상권분석_데이터$행정동_코드 = as.character(서울시_상권분석_데이터$행정동_코드) #타입 변환
# 숫자 ---> 문자로 형변환
str(서울시_상권분석_데이터) #타입확인
print(head(서울시_상권분석_데이터))
print(colnames(서울시_상권분석_데이터))
# 각 컬럼별 결측값 조회
print(colSums(is.na(서울시_상권분석_데이터)))
#options(scipen=999) # 10진수 표현
#print(summary(서울시_상권분석_데이터$음식_지출_총금액))

#서울시 각 행정구 교육지출 총금액을 지도로 시각화
# 1. shp 파일과 서울시 CSV파일을 병합.
# 2. 교집합 컬럼이 필요함.

# inner_join : 두 파일을 병합
# 22년 ~ 25년까지 데이터가 존재해서 group_by를 한것임.
merged_data = inner_join(seoul_map, 서울시_상권분석_데이터, 
                         by = c('SIG_CD' = '행정동_코드')) %>%
  group_by(SIG_CD, 행정동_코드_명) %>%
  summarise(교육지출 = mean(교육_지출_총금액, na.rm = TRUE)) %>%
  select(SIG_CD, 행정동_코드_명, 교육지출)
  arrange(desc(교육지출)) %>%   # 교육지출 내림차순
    head(5)

View(merged_data)

# 지도 시각화

result = ggplot(merged_data) + 
  scale_fill_gradient(low = '#ececec', high = 'blue', name = '교육지출총금액') +
  geom_sf_interactive(aes(fill = 교육지출총금액,
                          tooltip = 행정동_코드_명,      # 마우스 색깔이 바뀌고 행정동이 보이는 코드
                          data_id = SIG_CD)) + theme_minimal()
labs(title = '서울시 22~25년 교육지출총금액', x = '경도', y = '위도') 

giraph = girafe(ggobj = result)    

# print(result)  # 월평균소득이 지도로 시각화 된다.       
print(giraph)   # 마우스를 지도에 대면 지도 이벤트 추가(색깔이 바뀜)






