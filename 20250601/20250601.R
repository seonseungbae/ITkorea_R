# 경로확인
print(getwd())
print(list.files()) # 해당경로에 있는 파일 조회

# csv 파일 불러오기

emp = read.csv('emp.csv')
# View(emp)   # 뷰는 확인하고 항상 주석처리하라.

# 데이터 확인
# 문제 1 : 행과 열의 개수파악 # dimension
print(dim(emp))

# 문제 2 : 전체 컬럼만 조회  # col 컬럼 + names :  이름들
print(colnames(emp))

# 문제 3 : 데이터 상위 1 ~ 2 행 출력하기
print(head(emp,2))

# 문제 4 : 데이터 마지막 3개행 출력하기
print(tail(emp,3))
# 문제 5 : 데이터 타입 확인
str(emp)

## dplyr (디플리알) --> 데이터 가공(전처리)
# 데이터 전처리가 실무에서 80 ~ 90% 하는일이 전처리이다.
# install.packages("dplyr") # 1. 설치필요
library(dplyr) # 설치한 프로그램 가져오기(임포트 import)

# 급여(SAL)가 3000 이상인 직원들의 이름(ENAME)과 직업(JOB)
결과 = emp %>% filter(SAL >= 3000) %>% select(ENAME, JOB)
print(결과)

# 직업(JOB)별 평균 급여(SAL)를 계산하고 출력하세요.
# R은 group_by 만 단독적으로 사용시 의미없는 결과가 나온다.
# 그룹병 평균, 총합, 최대, 최소값, 중앙, 표준편차 ... summarize(요약하다)와
# 함께 사용해야 한다.
# 결과 = emp %>% group_by(JOB) %>% summarizes(AGV_SAL = mean(SAL))
# 여러개를 구하고 싶을때 아래 공식 적용
결과 = emp %>% group_by(JOB) %>% 
  summarise(AGV_SAL = mean(SAL), EMP_COUNT = n(), SUM_SAL = sum(SAL))
print(결과)

# 급여가 2000 이상인 직원들만 필터링 한 후,  부서번호(DEPTNO)별 직원수를 
# 계산하세요.
결과= emp %>% filter(SAL >= 2000) %>% group_by(DEPTNO) %>%
  summarize(n())
print(결과)

# dept 데이터 불러오기, str 구조확인

dept = read.csv('dept.csv')
str(dept)
# View(dept)

# 디플리안 병합(JOIN)
# JOIN은 두 데이터 프레임을 특정 컬럼을 기준으로 병합합니다.
# by ~~로 dept
조인결과 = emp %>% inner_join(dept, by = "DEPTNO")

# View(조인결과)

조인결과 = emp %>% inner_join(dept, by = "DEPTNO") %>% 
  filter(LOC == 'DALLAS') %>% select(ENAME, JOB)
print(조인결과)

# slice

결과 = emp %>% slice(2, 4)  # 2번째 4번째 행 추출
print(결과)

# 첫번째 ~ 세번째 직원행 추출
결과 = emp %>% slice(1:3)
print(결과)
# slice는 맨 마지막에 작성(대부분의 경우)


# 문제풀기 2, 4, 8, 10번 풀기

# 문제 2: "RESEARCH" 부서에 근무하는 직원들의 이름(ENAME)과 급여(SAL)를 출력하세요.
# 힌트 :  inner_join(), filter(), select()

조인결과 = emp %>% inner_join(dept, by = "DEPTNO") %>% 
  filter(LOC == 'RESEARCH') %>% select(ENAME, SAL)
print(조인결과)

# 문제 4: 각 부서(DNAME)별 직원 수를 계산하고 출력하세요.
# 힌트 : group_by(), summarize()
조인결과 = emp %>% inner_join(dept, by = "DEPTNO") %>% group_by(DNAME) %>%
  summarize(EMP_COUNT = n()) %>% slice(1)

# 문제 8: "SALES" 부서에서 근무하는 직원들의 이름(ENAME), 급여(SAL), 커미션(COMM)을 출력하세요.
# 힌트 : inner_join(), filter(), select() 


# 문제 10: 직업(JOB)이 "MANAGER"인 직원들의 이름(ENAME), 부서명(DNAME), 급여(SAL)을 출력하세요.
# 힌트 :  inner_join(), filter(), select()

조인결과 = emp %>% inner_join(dept, by = "DEPTNO") %>% 
  filter(JOB == 'MANAGER') %>% select(ENAME, DNAME, SAL)
print(조인결과)

# 문제 4: 각 부서(DNAME)별 직원 수를 계산하고 출력하세요.
# 힌트 : group_by(), summarize()  arrange 정렬 desc 내림차순
조인결과 = emp %>% inner_join(dept, by = "DEPTNO") %>% group_by(DNAME) %>%
  summarize(EMP_COUNT = n()) %>% arrange(desc(EMP_COUNT)) %>% slice(1:2)
print(조인결과)




























