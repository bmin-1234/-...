#Linear regression of test scores on student teacher ratio
#Data from school districts in California
#Replicating results in SW Chapter 7

# Clean workspace
rm(list=ls())
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")
setwd('C:/Users/82105/Desktop/계량경제/R')

library(readxl)
CASchools <- read_excel('caschool (1).xlsx')
CASchools$expn <- with(CASchools, expn_stu/1000) #in 1K dollars
summary(CASchools)

## dummy variable trap
## equation 5.18 with binary independent variable
CASchools$strs <- as.numeric(CASchools$str<20)
CASchools$strl <- as.numeric(CASchools$str>=20)


lm(testscr ~ strs, data = CASchools)
lm(testscr ~ strs + strl, data = CASchools)
lm(testscr ~ strs + strl - 1, data = CASchools)

## equation 7.6
regx <- lm(testscr ~ str + expn + el_pct, data = CASchools)
library(sandwich)
Covx <- vcovHC(regx, type = "HC1") 
library(lmtest)
coeftest(regx, vcov. = Covx) 
coefci(regx, vcov. = Covx) 

## test of a joint hypothesis
## H_0: beta_1 = 0 and beta_2 = 0
library(car)
lht(regx, c("str=0","expn=0"), vcov. = Covx)

## draw heteroskedasticity robust 95% confidence set for coefficients 
## on class size and expenditure per student
confidenceEllipse(regx,
                  fill = T,
                  lwd = 0,
                  which.coef = c("str", "expn"),
                  main = "95% Confidence Sets",
                  vcov. = Covx)
## add homoskedasticity only 95% confidence set
confidenceEllipse(regx,
                  fill = T,
                  lwd = 0,
                  which.coef = c("str", "expn"),
                  main = "95% Confidence Sets",
                  col = "red",
                  add = T)

## testing a single restriction involving multiple coefficients
## H_0: beta_1 = -beta_2
lht(regx, "str=-expn", vcov = Covx)

regx2 <- lm(testscr ~ str + I(expn-str) + el_pct, data = CASchools)
Covx2 <- vcovHC(regx2, type = "HC1")
coeftest(regx2, vcov = Covx2)
lht(regx2, c("str"), vcov = Covx2)

## estimate different model specifications
## column (1) in Table 7.1 & equation 5.8
spec1 <- lm(testscr ~ str, data = CASchools)
## column (2) in Table 7.1 & equation 7.5
spec2 <- lm(testscr ~ str + el_pct, data = CASchools)
## column (3) in Table 7.1
spec3 <- lm(testscr ~ str + el_pct + meal_pct, data = CASchools)
## column (4) in Table 7.1
spec4 <- lm(testscr ~ str + el_pct + calw_pct, data = CASchools)
## column (5) in Table 7.1
spec5 <- lm(testscr ~ str + el_pct + meal_pct + calw_pct, data = CASchools)

## gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(spec1, type = "HC1"))),
               sqrt(diag(vcovHC(spec2, type = "HC1"))),
               sqrt(diag(vcovHC(spec3, type = "HC1"))),
               sqrt(diag(vcovHC(spec4, type = "HC1"))),
               sqrt(diag(vcovHC(spec5, type = "HC1"))))

## generate a table using stargazer
library(stargazer)
stargazer(spec1, spec2, spec3, spec4, spec5,
          se = rob_se,
          digits = 3,
          header = F,
          type = "text",
          title = "Table 7.1")
