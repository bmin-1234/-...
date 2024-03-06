rm(list=ls())

setwd('C:/Users/82105/Desktop/계량경제/R')

library(readxl)
CASchools <- read_excel('caschool (1).xlsx')

## Estimation of simple linear regression model by OLS
lm(testscr ~ str, data = CASchools)

plot(testscr ~ str, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")

ols <- lm(testscr ~ str, data = CASchools)
abline(ols) 

## Homoskedasticiy-only standard errors
summary(ols)

## Heteroskedasticiy-robust standard errors
library(sandwich)
Cov <- vcovHC(ols, type = "HC1") #HC3 is default
library(lmtest)
coeftest(ols, vcov. = Cov)
coefci(ols, vcov. = Cov)

## When X is binary
CASchools$D <- CASchools$str<20 
olsb <- lm(testscr ~ D, data = CASchools) #X is an indicator for str<20
Covb <- vcovHC(olsb, type = "HC2")
coeftest(olsb, vcov. = Covb)
coefci(olsb, vcov. = Covb)

## Compare results with differences-of-means estimation
library(dplyr)

avgs <- CASchools %>% 
  group_by(D) %>% 
  summarise(mean=mean(testscr), sd=sd(testscr), n=n())

print(avgs)

avgs_s <- avgs %>% filter(D==1)
avgs_l <- avgs %>% filter(D==0) 

gap <- avgs_s$mean - avgs_l$mean
gap_se <- sqrt(avgs_s$sd^2/avgs_s$n + avgs_l$sd^2/avgs_l$n)
gap_t <- (gap - 0) / gap_se 
gap_pval <- 2 * pnorm(-abs(gap_t)) 

result <- cbind(gap, gap_se, gap_t, gap_pval)
print(result, digits = 5)

CASchools_s <- CASchools %>% filter(D==1)
CASchools_l <- CASchools %>% filter(D==0)
t.test(CASchools_s$testscr, CASchools_l$testscr, var.equal=FALSE)
