
rm(list=ls()) #clear workspace
if(!is.null(dev.list())) dev.off() #clear plots
cat("\014") #clear console

setwd('C:/Dropbox/Teaching/Econometrics/ECO3007/2021S/R')

library(readxl)
SReturns <- read_xlsx("Stock_Returns_1931_2002.xlsx")

#SReturns$Date <- as.yearmon(SReturns$time + (SReturns$Month - 1)/12, format = "%Y-%m")

SReturns$ExReturn.L1 <- Lag(SReturns$ExReturn)
SReturns$ExReturn.L2 <- Lag(SReturns$ExReturn,2)
SReturns$ExReturn.L3 <- Lag(SReturns$ExReturn,3)
SReturns$ExReturn.L4 <- Lag(SReturns$ExReturn,4)

SRsub <- subset(SReturns, time>=1932)

ar1 <- lm(ExReturn ~ ExReturn.L1, data = SRsub)
ar2 <- lm(ExReturn ~ ExReturn.L1 + ExReturn.L2, data = SRsub)
ar4 <- lm(ExReturn ~ ExReturn.L1 + ExReturn.L2 + ExReturn.L3 + ExReturn.L4, data = SRsub)

library(sandwich)
rob_se <- list(sqrt(diag(sandwich(ar1))),
               sqrt(diag(sandwich(ar2))),
               sqrt(diag(sandwich(ar4)))) 

library(stargazer)
stargazer(ar1, ar2, ar4,
          se = rob_se,
          digits = 3,
          header = F,
          type = "text",
          omit.stat = "rsq",
          column.labels=c("AR(1)","AR(2)","AR(4)"))

#Note that stargazer does not adjust overall F-stat and keeps reporting 
#homoskedasticity-only F-stat even when se options are used. 
#Heteroskedasticity-robust SEs are as follows:
library(car)
lht(ar1, c("ExReturn.L1=0"), vcov. = sandwich)
lht(ar2, c("ExReturn.L1=0","ExReturn.L2=0"), vcov. = sandwich)
lht(ar4, c("ExReturn.L1=0","ExReturn.L2=0","ExReturn.L3=0","ExReturn.L4=0"), vcov. = sandwich)
