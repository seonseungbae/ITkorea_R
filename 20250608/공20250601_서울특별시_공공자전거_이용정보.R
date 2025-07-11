# 한국어 파일의 경우, CP949 또는 EUC-KR 인코딩을 사용하는 것이 일반적
data = read.csv('서울특별시_공공자전거_이용정보.csv',
                na.strings = c(""), # ""를 NA로 표현한다.
                fileEncoding = 'CP949', 
                encoding = 'UTF-8', 
                check.names = FALSE)
# View(data)

# 데이터전처리만 진행해주세요.


#문제 1: 이동거리(M)가 2000 이상인 데이터 중 해당 대여소명과 이동거리(M), 이용시간(분)만 조회.


#문제 2: 대여소 별 이용건 수 조회.


#문제 3: 일일회원과 정기회원 이용 건 수, 평균 이용시간 조회. 단, 일일회원권 중 비회원은 제외


#문제 4: 탄소량이 0.8 이상인 이용 건수는 몇 건인지 조회.


#문제 5: 연령대별로 평균 이동거리(M) 조회.


#문제 6: 연령대별로 이용건수의 합과 평균 운동량을 구한 뒤, 운동량 평균이 가장 높은 연령대 조회.


#문제 7: 대여소명에 "역"이 포함된 대여소에서 발생한 총 운동량의 합 조회.


#문제 8: 10대 여성 회원의 평균 운동량, 평균 이동거리 조회. 단, 평균 운동량으로 내림차순 할 것


#문제 9: 운동량을 데이터 스케일링 min-max로 변환한 scaled_운동량 컬럼을 


#추가 0.8 이하인 회원 이동거리 사분위수 출력













