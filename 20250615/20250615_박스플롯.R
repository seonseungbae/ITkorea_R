# 박스플롯 : 데이터의 분포를 시각적 표현, 최솟값, 최댓값, 중앙값 등의 
# 요약 통계치를 보여주는데, 그룹핑 할때 사용한다.

# 제조사별 칼로리 분포를 박스플롯으로 표현해보기

library(ggplot2)

scereal = read.csv('UScereal.csv')

boxplot = ggplot(scereal, aes(x = mfr, y = calories, fill = mfr)) +
  geom_boxplot()+
  labs(title = '제조사별 칼로리 분포',
       x ='제조사',
       y ='칼로리')
  theme_minimal()  # 뒤에 회색배경 제거하는 것임.
  theme(panel.grid = element_blank())  # 뒤에 바둑판 (그리드) 제거
print(boxplot) 
  
# 제조사(mfr)  별 나트륨(sodium) 분포 박스플롯

boxplot = ggplot(scereal, aes(x = mfr, y = sodium, fill = mfr)) +
  geom_boxplot()+
  labs(title = '제조사별 나트륨 분포',
       x ='제조사',
       y ='나트륨')
theme_minimal()  # 뒤에 회색배경 제거하는 것임.
# theme(panel.grid = element_blank())  # 뒤에 바둑판 (그리드) 제거

print(boxplot) 

# ggsave('boxplot.pdf', width = 10, height = 6, dpi = 300)  




