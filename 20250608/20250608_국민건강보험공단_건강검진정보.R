library(dplyr)  # 로드
# 한국어 파일의 경우, CP949 또는 EUC-KR 인코딩을 사용하는 것이 일반적
data = read.csv('국민건강보험공단_건강검진정보.csv',
                na.strings = c(""), # ""를 NA로 표현한다.
                fileEncoding = 'CP949', 
                encoding = 'UTF-8', 
                check.names = FALSE)
# View(data)

# 건강검진 최종 10 문제
# 문제 1. : 남성의 허리둘레 사분위수 조회.
# 문제 2. : 성별 허리둘레와 체중 상관관계 조회.
# 문제 3. : 혈색소 수치는 남성은 13~17, 여성은 12~16이 정상입니다.
# 정상과 의심을 구별할 수 있는 컬럼 혈색소결과를 만드시오.
# 문제 4. : 식전혈당이 126이상은 위험, 100미만은 정상 그외는 주의를 나타내는 컬럼 당뇨병위험을 추가하시오.
            #추가 후 당뇨병위험 별 인원 수 조회.
# 문제 5. : 연령대 코드가 5~8인 사람 중 혈색소의 중앙값, 하위 30%, 상위 10%, 표준편차 조회.
# 문제 6. : 음주와 흡연을하는 남성의 혈색소 이상치를 제거한 데이터 수 조회. 임계치는 2로 필터링
# 문제 7. : 연령대 코드별로 허리둘레의 분포를 박스플롯으로 나타내세요.
# 문제 8. : 연령대 코드가 5~8인 사람의 신장과 체중의 관계를 산점도로 나타내시오.
             #회귀선도 추가하시오.
# 문제 9. : 감마지티피의 분포를 정규분포그래프으로 나타내세요.
# 중앙값, 하위 20%, 상위 20%, 신뢰구간도 표현해주세요.
# 문제 10. : 혈청지오티와 혈청지피티의 관계를 산점도로 나타내시오.
  # 단, 혈청지오티와 혈청지피티를 min-max로 스케일링 후 비교하시오. 
  # 회귀선도 추가하시오.
