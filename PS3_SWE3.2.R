#SW E3.2
rm(list=ls())
setwd('D:/Dropbox/Teaching/Econometrics/ECO3007/2020S/PS/PS4')

library(readxl)
scard <- read_excel('Sportscards.xlsx') 

#b.
n <- nrow(scard)
trade_mean <- mean(scard$trade)
trade_sd <- sd(scard$trade)
trade_se <- sqrt(trade_sd^2/n)
trade_cil <- trade_mean - 1.96 * trade_se
trade_ciu <- trade_mean + 1.96 * trade_se
trade_t <- (trade_mean-0.5)/trade_se
trade_pval <- 2*pnorm(-abs(trade_t))

#c.
library(dplyr)

avgs <- scard %>% 
  group_by(dealer) %>% 
  summarise(mean=mean(trade), sd=sd(trade), n=n())

avgs$se <- sqrt(avgs$sd^2/avgs$n)
avgs$cil <- avgs$mean - 1.96 * avgs$se
avgs$ciu <- avgs$mean + 1.96 * avgs$se
avgs$t <- (avgs$mean-0.5)/avgs$se
avgs$pval <- 2*pnorm(-abs(avgs$t))

print(avgs)

avgs_d <- avgs %>% filter(dealer == 1)
avgs_nd <- avgs %>% filter(dealer == 0) 

gap <- avgs_d$mean - avgs_nd$mean
gap_se <- sqrt(avgs_d$sd^2/avgs_d$n + avgs_nd$sd^2/avgs_nd$n)
gap_t <- (gap - 0) / gap_se 
gap_pval <- 2 * pnorm(-abs(gap_t)) 
gap_ci_l <- gap - 1.96 * gap_se 
gap_ci_u <- gap + 1.96 * gap_se

result <- cbind(gap, gap_se, gap_t, gap_pval, gap_ci_l, gap_ci_u)
print(result, digits = 3)
