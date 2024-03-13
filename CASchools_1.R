# 현재 작업환경 내 변수들을 모두 삭제합니다. Is()가 현재 작업 공간에 있는 모든 객체 이름을 반환, 이걸 list로 만들어서 제거.
rm(list=ls())

# 작업 디렉토리를 설정합니다.
setwd('C:/Users/82105/Desktop/°è·®°æÁ¦/R')

# readxl 라이브러리를 불러옵니다.
library(readxl)

# 'caschool (1).xlsx' 파일을 읽어옵니다.
CASchools <- read_excel('caschool (1).xlsx')

# 'testscr'를 종속변수로, 'str'을 독립변수로 하는 선형 회귀분석을 수행합니다.
lm(testscr ~ str, data = CASchools)

# 'testscr'과 'str'의 산점도를 그립니다.
plot(testscr ~ str, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")

# 선형 회귀분석 모델을 적합시킵니다. 변수에 저장한 거임.
linear_model <- lm(testscr ~ str, data = CASchools)

# 적합시킨 회귀선을 그립니다.
abline(linear_model)

