# 시각화
# emp.csv파일 불러오기
# dplyr도 임포트

library(dplyr)
emp = read.csv('emp.csv')
# 1. 박스플롯
# 박스플롯은 여러 그룹의 데이터 분포를 비교하여 중앙값과 퍼짐정도 차이 분석
# 예) 남 녀 영어점수 비교, a팀 b팀 영업성과 비교
# 부서별 급여 박스플로으로 시각화
# xlab ylab 축을 의미하고, main은 그래프 이름을 의미함.
# col 컬럼
# 해당그래프를 pdf로 변환

pdf('부서별 급여_박스플롯.pdf', family = 'Korea1deb')
boxplot(emp$SAL ~ emp$DEPTNO, main = '부서별 급여 현황', 
        xlab = '부서', ylab = '급여', col = c('orange','green','blue'))

dev.off() # pdf 다운로드 종료



# 막대그래프
# 막대그래프는 범주형데이터로 빈도나 크기를 비교할 때 사용
# 예를들면, 제품별 판매량 비교, 직업에 따른 평균 소득

# 부서별 평균 급여를 막대그래프로 표현
# 전처리로 평균 급여 조회

dept_avg_sal = emp %>% group_by(DEPTNO) %>% summarise(AVG_SAL = mean(SAL))

# names.arg 는 x축 데이터 표시
# main 은 그래프 이름
# ylab 은 y축
barplot(dept_avg_sal$AVG_SAL, names.arg = dept_avg_sal$DEPTNO,
        main = '부서별 평균 급여', ylab = '급여')

# 히스토 그램
# 데이터를 일정한 구간으로 나누고, 각 구간에 속하는 데이터의 빈도를 
# 막대의 높이로 표현

# 문제 mutate 사용해서, 직원 COMM이 NA 인 직원만 100 지급
# mutate는 수정 추가 시 사용한다.
emp = emp %>% mutate(COMM = ifelse(is.na(COMM), 100, COMM))  # 수정한것임.

# 문제 mutate 사용해서, 직원 급여와 직원 커미션을 더한 새로운 컬럼
# SUM_SAL_COMM 만들기
emp = emp %>% mutate(SUM_SAL_COMM = SAL + COMM) # 추가
print(emp)

hist(emp$SUM_SAL_COMM, main = '직원 급여 + 커미션 분포', 
     xlab = '급여+커미션', ylab = '빈도')

# 그래프에다가 평균선 추가(위의 그래프에다가 평균선 적색 추가한 모습)
# abline : add a line 직선을 추가하다.
pdf('salary_hist.pdf', family = 'Korea1deb')

abline(v = mean(emp$SUM_SAL_COMM), col = 'red', lwd = 2)

dev.off()       # pdf 다운로드 종료(pdf 창닫기)

# 산점도 
# 변수간의 관계를 시각화할때 사용한다. 키와 몸무게 관계
# 온도와 에너지 소비량 관계, 흡연과 건강검진 결과 관계

x = c(1,4,2,6,10,15)
y = c(2,3,6,6,10,2)

plot(x, y, main = '산점도 예시', xlab = 'x값', ylab = 'y값', col = 'blue',
     pch = 20)






















