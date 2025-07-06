# setwd('') # 폴더(디렉토리) 변경

print(getwd())  # 디렉토리 경로확인

print(list.files()) # 현재 디렉토리 파일 목록 출력

# emp.csv 파일을 불러오기는 다음과 같음.
emp = read.csv('emp.csv')  # 데이터 프레임 불러오기
# View(emp) # 확인

# 문제 1 : 행과 열의 개수 파악
print(dim(emp))
# 문제 2 : 전체 컬럼만 조회
print(colnames(emp))
# 문제 3 : 데이터 상위 1 ~ 4행 출력하기
print(head(emp,4))
# 문제 4 : 데이터 마지막 3행 출력하기
print(tail(emp,3))
# 문제 5 : *** 데이터 타입 확인 : 외우기 코드
str(emp)   # 각 벡터들이 숫자인지 문자인지 날짜형인지 나타냄.

#### dplyr(디플리알) : data frame piler 의 줄임말 '데이터프레임을 다루는 공구'
# 실무에서 자주 사용되고, 특히 대규모 데이터셋에서 빠른속도록 제공함.
# 이것은 설치를 해야하는데 아래와 같이 함.

# install.packages('dplyr') # 설치명령어.
library(dplyr)  # 설치된 디플리알 불러오기, 이후 주석처리 한다.
#(앞의 것 인스톨 패키지 주석처리)

# 문제 : 급여가 3000 이상인 사원조회
# 방향(람다식표현) : emp %>%
급여3000 = emp %>% filter(SAL >= 3000)
print(급여3000)

# 급여가 3000 이상인 사원 이름, 급여, 사원번호 조회
급여3000이름 = emp %>% filter(SAL >= 3000) %>% select(ENAME, SAL, empNO)
print(급여3000이름)

# 사원 직책(JOB) 이 MANAGER 인 사원의 이름, 직책, 부서번호, 급여조회
결과 = emp %>% filter(JOB == 'MANAGER') %>% select(ENAME, JOB, DEPTNO, SAL)
View(결과)

# 새로운 열 추가 & 수정
# mutate : 변화하다
결과 = emp %>% mutate(TOTAL_COMM = SAL + 100)
View(결과)

# 급여와 커미션의 합계를 TOTAL_COMM에 수정 ****
결과 = emp %>% mutate(TOTAL_COMM = SAL + ifelse(is.na(COMM),0,COMM))
# View(결과) # 결과확인

# group by 
# group by는 데이터를 특정기준으로 묶어 그룹화 하는것임.
# 단, R은(group by) 사용시 단, SUMMARIZE(요약하다) 같이 사용
# AVG_SAL 열이름을 넣는다.
group_result = emp %>% group_by(JOB) %>% summarize(AVG_SAL = mean(SAL))
print(group_result)

# 부서번호별(DEPTNO) 평균급여
dept_result = emp %>% group_by(DEPTNO) %>% summarize(AVG_SAL = mean(SAL))
print(dept_result)

# 부서별(DEPTNO) 최소, 최대, 평균 급여 조회
dept_result = emp %>% group_by(DEPTNO) %>% summarize(MIN_SAL = min(SAL), 
MAX_SAL = max(SAL), AVG_SAL = mean(SAL))
print(dept_result)

# 직책ㄹ별 직원 수 조회
# n() : 각각 그롭의 행 개수를 계산하는 것임
job_count = emp %>% group_by(JOB) %>% summarize(COUNT = n())
print(job_count)

# 정렬 arrange(조정하다, 배열하다.)
# arrange(COUNT) : count 기준으로 오름차순
job_count = emp %>% group_by(JOB) %>% summarize(COUNT = n()) %>%
 arrange(COUNT)
print(job_count)

# 내림차순
# desc : Descending 내림차순
job_count = emp %>% group_by(JOB) %>% summarize(COUNT = n()) %>%
  arrange(desc(COUNT))
print(job_count)

# 급여기준으로 내림차순으로 정렬
#sal_count = emp %>% group_by(SAL) %>% summarize(COUNT = n()) %>%
 # arrange(desc(COUNT))
# print(sal_count)
# 항상 절렬은 마지막에 진행
급여내림차순 = emp %>% select(ENAME,SAL) %>% arrange(desc(SAL))
print(급여내림차순)

# 부서별 최대 급여 직원 조회 : ~ 00별이 나오면 무조건 group_by 사용
# slice : 자르다 , 일부분
부서별급여킹 = emp %>% group_by(DEPTNO) %>% slice_max(SAL, n = 1)
print(부서별급여킹)
# 지금까지 한 것이 전처리 이다. *****************************
# 매우 중요함. 박스플롯 하면 그래프가 나온다.
# 박스플롯
#boxplot(emp$SAL ~ emp$DEPTNO, main = '부서별 급여', xlab = '부서번호',
#    ylab = '급여', col = c('orange', 'green', 'lightblue'))

# 문제 : 근속연수일 컬럼 추가(아래의 내용임.)

오늘날짜 = Sys.Date() # 현재 날짜 조회
str(emp)  # 데이터 프레임 데이터 확인 문자로되어 있다는 것 확인됨.
#방법 1번
emp = emp %>% mutate(HIREDATE = as.Date(HIREDATE))

#방법 2번
# emp$HIREDATE = as.Date(emp$HIREDATE)
str(emp)  # 꼭 확인할 것 아주 중요한 것임.

결과 = emp %>% mutate(근속일 = difftime(오늘날짜, HIREDATE))
# View(결과)

# 문제 : 급여가 2000 이상인 직원 중 세후 급여(SAL_TAX) 컬럼 추가하기.
# 단, 사원이름, 급여만 조회, 급여 내림차순까지
# 3.3% 원천징수  중요한 명령어, mutate(수정 및 조정 추가할때 사용)
결과 = emp %>% filter(SAL >= 3000) %>% select(ENAME,SAL) %>% 
  mutate(SAL_TAX = SAL * 0.967) %>% arrange(desc(SAL))
print(결과)

# 문제 : 부서번호가 30 인 직원 중 이름, 직업, 부서번호만 조회, 단 이름 오름차순 정렬
결과 = emp %>% filter(DEPTNO == 30) %>% select(ENAME, JOB, DEPTNO) %>% 
   arrange(ENAME)
print(결과)

# 문제 : 1981-01-01 이후 입사한 직원이름, 입사일, 급여, 근무연수 조회
str(emp)  # 데이터 타입확인 (무조건 먼저 데이터 타입을 확인한다.)
결과 = emp %>% filter(HIREDATE > as.Date('1981-01-01')) %>%
  select(ENAME, HIREDATE, SAL) %>% mutate(근무연수 = 
  as.numeric(오늘날짜 - HIREDATE) / 365)  # 날짜 --> 숫자로 변환
print(결과)

















