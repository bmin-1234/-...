rm(list=ls())

setwd('C:/Dropbox/Teaching/Econometrics/ECO3007/2021S/R')

library(readxl)
CASchools <- read_excel('caschool.xlsx')

lm(testscr ~ str, data = CASchools)

plot(testscr ~ str, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")

ols <- lm(testscr ~ str, data = CASchools)
abline(ols) 

summary(ols)
library(sandwich)
Cov <- vcovHC(ols, type = "HC1") #HC3 is default
library(lmtest)
coeftest(ols, vcov. = Cov)
coefci(ols, vcov. = Cov)
