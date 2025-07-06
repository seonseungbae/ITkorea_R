health = read.csv('국민건강보험공단_건강검진정보.csv', 
                  fileEncoding = 'CP949', 
                  encoding = 'UTF-8', 
                  check.names = FALSE)
# View(health)   # 1은 높은 것이고, 0은 낮은것으로 퉁친다.

# 혈색소 데이터를 min-max 스케일링 하시오.
# 최솟값, 최대값
혈색소_최소값 = min(health$혈색소)
혈색소_최대값 = max(health$혈색소)
health$혈색소_스케일링 = (health$혈색소 - 혈색소_최소값) /
  (혈색소_최대값 - 혈색소_최소값)

# View(health)   # 이것은 View(data) 확인하고 반드시 주석처리 해라.

library(dplyr)  # 로드
# nrow() : 행의 수
# 0.8을 기준으로 잡는다 0.81 ~ 1 사이의 데이커를 이상치값 이라고 함.
결과 = health %>% filter(혈색소_스케일링 > 0.5) %>% nrow()
print(결과)


# 문제1. 데이터 구조 확인
str(health)

# 문제2. 상위 5행까지 출력
head(health, 5)


# 괄호가 있는 컬럼은 `` 을 사용한다. 엑셀 컬럼 혈청지오티(ast)

결과 = health %>% select(`혈청지오티(AST)`)  # `` 숫자 1 옆에 ~과 같이있음.


# 1-1. 기초문제
# 데이터셋 dplyr 기초

#문제1: dplyr 패키지를 불러오고 현재 작업 폴더 경로 출력하기

library(dplyr)
str(health)
print(getwd())


#문제2: 국민건강보험공단_건강검진정보.csv 파일 불러와 View로 출력하기.
health = read.csv('국민건강보험공단_건강검진정보.csv', 
                  fileEncoding = 'CP949', 
                  encoding = 'UTF-8', 
                  check.names = FALSE)
#View(health)

#문제3: 데이터프레임의 앞부분 출력 단, 10행 까지
print(head(health, 10))

#문제4: 데이터프레임의 뒷부분 출력 단, 5행 까지
print(tail(health, 5)) 

#문제5: 데이터프레임의 데이터 타입 확인
str(health)

#문제5: 성별, 연령대, 그리고 지역 열만 조회
결과 = health %>% select(성별, `연령대코드(5세단위)`, 시도코드)

#문제7: 2022년에 건강검진을 받은 사람 중 음주여부가
#1인 사람의 가입자일련번호, 성별 조회 년도가 문자일 경우 '2022'
결과 = health %>% filter(기준년도 == 2022 & 음주여부 == 1) %>% 
  select(가입자일련번호, 성별)

#문제8: 키(height)와 몸무게(weight) 열을 사용하여 새로운 열 BMI를 추가하세요.
# BMI공식은 아래와 같습니다.
# BMI
# 추가, 수정 --> mutate
# ** --> 지수  추가된 BMI에 HEALTH 에 대입
health = health %>% mutate(BMI = `신장(5cm단위)` / (`체중(5kg단위)` / 100)**2 )
# View(health)


#문제9: 음주여부와 흡연상태가 1인 사람의 수축기혈압 , 성별 조회.
#단, 혈압 내림차순으로 정렬하세요.
결과 = health %>% filter(음주여부 == 1 & 흡연상태 == 1) %>% 
  select(수축기혈압, 성별) %>% arrange(desc(수축기혈압))

#문제10: 성별(성별)로 데이터를 그룹화하고, 각 그룹별 평균 BMI를 계산하세요. 
#결과는 성별과 평균_BMI 열로 구성되어야 합니다.
결과 = health %>% group_by(성별) %>% summarise(평균_BMI = mean(BMI, na.rm = TRUE))

#문제11: 식전혈당 126 이상인 사람 중 수축기혈압 상위 5명 출력
결과 = health %>% filter(`식전혈당(공복혈당)`>= 126) %>% 
  arrange(desc(수축기혈압)) %>% slice(1:5)

#문제12: 허리둘레 중앙값 조회
결과 = health %>% summarise(허리둘레_중앙값 = median(허리둘레, na.rm = TRUE))

# 성별로 음주를 하는 1인 평균 체중을 막대그래프로 표현
avg_weight_by_gender = health %>% filter(음주여부 == 1) %>%
  group_by(성별) %>% summarise(평균체중 = mean(`체중(5kg단위)`, na.rm = TRUE))
                             
# 데이터 시각화
barplot(avg_weight_by_gender$평균체중, names.arg = avg_weight_by_gender$성별,
        col = c('blue', 'red'),
        main = '음주를 하는 성별 평균체중',
        ylab = '평균 체중(kg)')  # y축이름


# 연령별 식전혈당과 표준편차 평균 구하기 ---> 막대그래프로 표현

# 연령대별 평균 식전혈당, 표준편차 00

avg_result_by_age = health %>% group_by(`연령대코드(5세단위)`) %>%
  summarise(
    식전혈당_표준편차 = sd(`식전혈당(공복혈당)`, na.rm = TRUE),
    식전혈당_평균 = mean(`식전혈당(공복혈당)`, na.rm = TRUE)
  )
print(avg_result_by_age)


# 사분위수 계산
# 퍼센트별 상위 00% 값이 나옴.
q = quantile(avg_result_by_age$식전혈당_표준편차)
print(q)

# 표준편차와 식전혈당 행렬로 묶기  mat

 mat = rbind(avg_result_by_age$식전혈당_표준편차, 
             avg_result_by_age$식전혈당_평균)
 print(mat)
 
 # 묶은 것을 가지고 막대그래프 시각화
 barplot(mat, names.arg = avg_result_by_age$`연령대코드(5세단위)`,
         beside = TRUE,   # 두 막대 표현
         main = '연령대 별 식전혈당 평균과 표준편차',
         ylab = '식전혈당',
         xlab = '연령대코드(5세단위)',
         col = c('blue', 'red'),   # 순서 맞춰주기 색깔
         legend.text = c('표준편차', '평균')  # 범례표시(legend)
         )

 
# 산점도
# 남성의 혈청지오티(간기능)을 mim-max로 변환후 변환된 값이 0.8보다 큰
# 남성의 가입자일련번호, 신장, 체중, 혈청지오티 조회하기 단, 혈청지오티 
# 기준으로 내림차순
 ast_result_by_male = health %>% filter(성별 == 1) %>%
   mutate(AST_SCALED = (`혈청지오티(AST)` - min(`혈청지오티(AST)`)) / 
            (max(`혈청지오티(AST)`) - min(`혈청지오티(AST)`)))
# View(ast_result_by_male)
 
 # 0.8보다 큰 남성 조회
 high_ast_male = ast_result_by_male %>% filter(AST_SCALED >= 0.8) %>%
   select(가입자일련번호, `신장(5cm단위)`, `체중(5kg단위)`, `혈청지오티(AST)`) %>%
   arrange(desc(`혈청지오티(AST)`))
 print(high_ast_male) # 2명 조회
 
 
 # 간수치와 나이 관계 (산점도 표현)
 age = ast_result_by_male$`연령대코드(5세단위)`
 ast = ast_result_by_male$AST_SCALED
 plot(age, ast, main = '연령대와 혈청지오티 관계',
      xlab = '연령대코드',
      ybal = '혈청지오티',
      col = 'blue',
      pch = 20
 )
 # lm: 회귀선
 model = lm(ast ~ age)
 # abline : add a line : 선 추가하다
 abline(model, col ='red', lwd =2)
 
 
 

 
 
 
 
 
 
 
 








