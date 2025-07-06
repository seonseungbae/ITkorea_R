# 산점도 
# 변수간의 관계를 시각화할때 사용한다. 키와 몸무게 관계
# 온도와 에너지 소비량 관계, 흡연과 건강검진 결과 관계

x = c(1,4,2,6,10,15)
y = c(2,3,6,6,10,2)
# pch 는 점크기이다.

plot(x, y, main = '산점도 예시', xlab = 'x값', ylab = 'y값', col = 'blue',
     pch = 20)

# (산점도 분석 후 회귀선 추가)
# 회귀선 추가
# linear model : 선형 모델 선형 회귀
model = lm(y ~ x)

# abline : add a line
abline(model, col = 'red', lwd = 2)

### 데이터 스케일링
# 데이터 스케일링은 전처리 방법 중 하나이고, 
# 분석과 머신러닝에서 중요한 과정입니다.

# 데이터 프레임 생성
data = data.frame(
  height_cm = c(150, 160, 170, 180, 190),  # 키
  weight_kg = c(50, 60, 70, 80, 90)        # 몸무게
)

# 수치를 통일한다.
# min-max라는 기법을 통해서 분석할 데이터를 0과 1사이로 변환한다.
# 즉, 모든 데이터는 0과 1사이에 존재함.

# 암기가 필요한 내용이다.
# 스케일링 = 기존값 - 최소값 / 최대값- 최소값  # 공식이다.

height_min = min(data$height_cm)  # 키 최소값
height_max = max(data$height_cm)  # 키 최대값

# 스케일링 결과 컬럼 추가

data$height_scaled = (data$height_cm - height_min) / (height_max - height_min)
# View(data)

# 몸무게 스케일링해서 결과확인
weight_min = min(data$weight_kg)  # 몸무게 최소값
weight_max = max(data$weight_kg)  # 몸무게 최대값
data$weight_scaled = (data$weight_kg - weight_min) / (weight_max - weight_min)
# View(data)   # 이것은 View(data) 확인하고 반드시 주석처리 해라.


























