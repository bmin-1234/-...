# 현재 작업 환경(workspace)에 있는 모든 객체를 삭제
rm(list=ls())

# 작업 디렉토리를 설정
setwd('C:/Dropbox/Teaching/Econometrics/ECO3007/2021S/R')

# readxl 라이브러리를 불러옴
library(readxl)

# 'caschool.xlsx' 파일을 읽어와 데이터프레임에 저장
CASchools <- read_excel('caschool.xlsx')

# 선형 회귀분석 수행
lm(testscr ~ str, data = CASchools)

# 'testscr'과 'str'의 산점도를 그림
plot(testscr ~ str, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")

# 회귀분석 결과를 'ols'라는 변수에 저장
ols <- lm(testscr ~ str, data = CASchools)

# 적합된 회귀선을 그림
abline(ols) 

# 선형 회귀분석 결과를 요약해서 출력
summary(ols)

# 'sandwich' 라이브러리를 불러옴 (표준 오차 계산을 위해)
library(sandwich)

# Huber-White 추정량을 사용하여 표준 오차 계산 후 'Cov'에 저장
Cov <- vcovHC(ols, type = "HC1")

# 'lmtest' 라이브러리를 불러옴 (가설 검정을 위해)
library(lmtest)

# 표준 오차 고려한 회귀 계수에 대한 가설 검정 수행
coeftest(ols, vcov. = Cov)

# 신뢰 구간 계산 후 출력
coefci(ols, vcov. = Cov)


'''
표준 오차 (Standard Error):

표준 오차는 특정 추정값(예: 회귀 계수)의 불확실성을 나타내는 지표입니다.
회귀분석에서 주로 사용되며, 회귀 계수의 표준 편차를 의미합니다.
표준 오차가 작을수록 해당 추정값이 더 정밀하다고 해석할 수 있습니다.
회귀 분석에서 일반적으로 표준 오차는 잔차 제곱합과 자유도를 사용하여 계산됩니다.

공분산 (Covariance):

공분산은 두 변수 간의 관계를 나타내는 지표로, 두 변수가 함께 어떻게 변하는지를 측정합니다.
두 변수가 함께 증가하거나 감소하면 공분산은 양수이고, 서로 반대로 움직이면 음수입니다.
공분산은 특정 단위에 의존하기 때문에 단위에 영향을 받습니다.
두 변수의 상관 관계를 파악하는 데 사용되지만, 각 변수의 변동 크기에 따라서 해석이 어려울 수 있습니다.'''

