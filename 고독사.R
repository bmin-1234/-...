rm(list=ls())
setwd('C:/Users/82105/Desktop/예술클래식')

library(readxl)
DATA <- read_excel('고독사 설문.xlsx')

library(dplyr)
library(lmtest)
library(sandwich)

olsb_1 <- lm(bad ~ clean, data = DATA)
Covb <- vcovHC(olsb_1, type = "HC2")
coeftest(olsb_1, vcov. = Covb)


plot(bad ~ clean, 
     data = DATA,
     main = "고독사", 
     xlab = "청결도",
     ylab = "우울감")

ols <- lm(bad ~ clean, data = DATA)
abline(ols) 

olsb_1 <- lm(bad ~ money, data = DATA)
Covb <- vcovHC(olsb_1, type = "HC2")
coeftest(olsb_1, vcov. = Covb)


plot(bad ~ money, 
     data = DATA,
     main = "고독사", 
     xlab = "금전적 여유",
     ylab = "우울감")

ols <- lm(bad ~ moeny, data = DATA)
abline(ols)

olsb_1 <- lm(bad ~ lone, data = DATA)
Covb <- vcovHC(olsb_1, type = "HC2")
coeftest(olsb_1, vcov. = Covb)


plot(bad ~ lone, 
     data = DATA,
     main = "고독사", 
     xlab = "외로움",
     ylab = "우울감")

ols <- lm(bad ~ lone, data = DATA)
abline(ols)


olsb_1 <- lm(bad ~ outside, data = DATA)
Covb <- vcovHC(olsb_1, type = "HC2")
coeftest(olsb_1, vcov. = Covb)


plot(bad ~ outside, 
     data = DATA,
     main = "고독사", 
     xlab = "외출빈도",
     ylab = "우울감")

ols <- lm(bad ~ outside, data = DATA)
abline(ols)


