# install.packages('sf')  설치는 한번만한다. pc를 온 오프해도 설치된상태임.

library(sf)  # 공간테이터를 처리하고 시각화하기 위한 패키지
library(ggplot2)

# R 경로확인
print(getwd())

# SHP 파일 불러오기  
# SHP 파일? 공간데이터를 저장하고 표현하는데 사용하는 파일
# (우리나라 좌표가 모두 저장되어 있다.) 업체가 있다.

shp = 'sig.shp'  # 파일이름(저장되어 있는 파일임.)
korea_map = st_read(shp, quiet = TRUE) # 지도파일 불러오기

# print(korea_map)  좌표가 위도 경도가 표시된다.


library(dplyr)
# substr : 특정위치의 부분 문자열 추출
# 서울은 11로 시작
seoul_map = korea_map %>% filter(substr(SIG_CD,1,2) == '11')

result = ggplot(seoul_map) + 
  geom_sf(fill = 'white', color = 'black')  # 흰색채우기, 검은색 경계선
  labs(x='경도', y ='위도', title ='전국 행정구역') +
  coord_sf()  # 지도 비율유지  
  theme_minimal()  # 회색배경제거
print(result)
















