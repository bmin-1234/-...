# 현재 작업환경(workspace)에 있는 모든 객체를 삭제
rm(list=ls())

# 작업 디렉토리를 설정
setwd('C:/Users/82105/Desktop/°è·®°æÁ¦/R')

# readxl 라이브러리를 불러옴
library(readxl)

# 'caschool (1).xlsx' 파일을 읽어와 데이터프레임에 저장
CASchools <- read_excel('caschool (1).xlsx')

## Estimation of simple linear regression model by OLS
# 단순 선형 회귀분석 모델 추정
lm(testscr ~ str, data = CASchools)

# 'testscr'과 'str'의 산점도를 그림
plot(testscr ~ str, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")

# 적합된 회귀선을 그림
ols <- lm(testscr ~ str, data = CASchools)
abline(ols) 

## Homoskedasticiy-only standard errors
# 잔차의 등분산성 가정을 가지고 있는 경우의 표준 오차 계산
summary(ols)

## Heteroskedasticiy-robust standard errors
# 이분산성을 고려한 표준 오차 계산
library(sandwich)
Cov <- vcovHC(ols, type = "HC1") #HC3 is default
library(lmtest)
coeftest(ols, vcov. = Cov)
coefci(ols, vcov. = Cov)

## When X is binary
# X가 이진(binary)인 경우
CASchools$D <- CASchools$str < 20 
olsb <- lm(testscr ~ D, data = CASchools) # X is an indicator for str<20

# 히터로스카스티시티(이분산성)를 고려한 표준 오차 계산
Covb <- vcovHC(olsb, type = "HC2")
coeftest(olsb, vcov. = Covb)
coefci(olsb, vcov. = Covb)

## Compare results with differences-of-means estimation
# 평균 차이로 결과 비교
library(dplyr)

# 각 그룹의 평균, 표준편차, 데이터 수 계산
avgs <- CASchools %>% 
  group_by(D) %>% 
  summarise(mean=mean(testscr), sd=sd(testscr), n=n())

print(avgs)

# 이진 변수에 따라 그룹 나누기
avgs_s <- avgs %>% filter(D == 1)
avgs_l <- avgs %>% filter(D == 0) 

# 평균 차이, 표준 오차, t-통계량, p-value 계산
gap <- avgs_s$mean - avgs_l$mean
gap_se <- sqrt(avgs_s$sd^2 / avgs_s$n + avgs_l$sd^2 / avgs_l$n)
gap_t <- (gap - 0) / gap_se 
gap_pval <- 2 * pnorm(-abs(gap_t)) 

result <- cbind(gap, gap_se, gap_t, gap_pval)
print(result, digits = 5)

# T-검정으로 결과 비교
CASchools_s <- CASchools %>% filter(D == 1)
CASchools_l <- CASchools %>% filter(D == 0)
t.test(CASchools_s$testscr, CASchools_l$testscr, var.equal = FALSE)
