# E 3.2 b ---------------------------------------------
rm(list=ls())
getwd()
setwd('C:/Users/82105/Desktop/계량경제/R/과제')
library(readxl)
card <- read_excel('Sportscards.xlsx') #import data
library(dplyr)

C <- card %>% 
  group_by(goodb, trade) %>% 
  summarise(n())


A_t0 <- C %>% filter(goodb == 0, trade==0)  
B_t0 <- C %>% filter(goodb == 1, trade==0) 
A_t1 <- C %>% filter(goodb == 0, trade==1)
B_t1 <- C %>% filter(goodb == 1, trade==1)


colnames(A_t0) <- c('goodb','trade','n')
colnames(B_t0) <- c('goodb','trade','n')
colnames(A_t1) <- c('goodb','trade','n')
colnames(B_t1) <- c('goodb','trade','n')

# Receiving good A or B
# Receiving good A or B / sum
PR_RA <- (A_t0$n+A_t1$n)/((A_t0$n+A_t1$n)+(B_t0$n+B_t1$n))
PR_RB <- (B_t0$n+B_t1$n)/((A_t0$n+A_t1$n)+(B_t0$n+B_t1$n))
print(PR_RA)
print(PR_RB)

# Prefer good A or B
# Prefer good A : Receiving good A not trade + Receiving good B trade / sum
PR_PA <- (A_t0$n+B_t1$n)/((A_t0$n+A_t1$n)+(B_t0$n+B_t1$n))
PR_PB <- (B_t0$n+A_t1$n)/((A_t0$n+A_t1$n)+(B_t0$n+B_t1$n))
print(PR_PA)
print(PR_PB)
  
trading_rate <- PR_RA*PR_PB + PR_RB*PR_PA
print(trading_rate)

print(trading_rate - 0.5)
# trading_rate is equal zero.
# The fraction is not significantly different from 50%. same.
# There is no evidence of an endowment effect.


# E 3.2 c ---------------------------------------------
rm(list=ls())
getwd()
setwd('C:/Users/82105/Desktop/계량경제/R/과제')
library(readxl)
card <- read_excel('Sportscards.xlsx')
library(dplyr)

D <- card %>% 
  group_by(dealer, goodb, trade) %>% 
  summarise(n())

nd_A_t0 <- D %>% filter(dealer==0, goodb == 0, trade==0)
nd_A_t1 <- D %>% filter(dealer==0, goodb == 0, trade==1)
nd_B_t0 <- D %>% filter(dealer==0, goodb == 1, trade==0)
nd_B_t1 <- D %>% filter(dealer==0, goodb == 1, trade==1)
d_A_t0 <- D %>% filter(dealer==1, goodb == 0, trade==0)
d_A_t1 <- D %>% filter(dealer==1, goodb == 0, trade==1)
d_B_t0 <- D %>% filter(dealer==1, goodb == 1, trade==0)
d_B_t1 <- D %>% filter(dealer==1, goodb == 1, trade==1)


colnames(nd_A_t0) <- c('dealer','goodb','trade','n')
colnames(nd_A_t1) <- c('dealer','goodb','trade','n')
colnames(nd_B_t0) <- c('dealer','goodb','trade','n')
colnames(nd_B_t1) <- c('dealer','goodb','trade','n')
colnames(d_A_t0) <- c('dealer','goodb','trade','n')
colnames(d_A_t1) <- c('dealer','goodb','trade','n')
colnames(d_B_t0) <- c('dealer','goodb','trade','n')
colnames(d_B_t1) <- c('dealer','goodb','trade','n')


PR_nd_RA <- (nd_A_t0$n+nd_A_t1$n)/((nd_A_t0$n+nd_A_t1$n)+(nd_B_t0$n+nd_B_t1$n))
PR_nd_RB <- (nd_B_t0$n+nd_B_t1$n)/((nd_A_t0$n+nd_A_t1$n)+(nd_B_t0$n+nd_B_t1$n))

PR_nd_PA <- (nd_A_t0$n+nd_B_t1$n)/((nd_A_t0$n+nd_A_t1$n)+(nd_B_t0$n+nd_B_t1$n))
PR_nd_PB <- (nd_B_t0$n+nd_A_t1$n)/((nd_A_t0$n+nd_A_t1$n)+(nd_B_t0$n+nd_B_t1$n))

print(PR_nd_PA)
print(PR_nd_PB)

nd_trading_rate <- PR_nd_RA*PR_nd_PB + PR_nd_RB*PR_nd_PA
print(nd_trading_rate)

phat <- nd_trading_rate
n <- (nd_A_t0$n+nd_A_t1$n)+(nd_B_t0$n+nd_B_t1$n)

# H0: trading rate = 0.5 vs H1: trading rate != 0.5
s <- sqrt(phat*(1-phat)/n)
t <- (phat-0.5)/s
pval <- 2 * pnorm(-abs(t))
print(t)
print(pval)

# non dealer
# we can not reject the test with 5% significance level..

PR_d_RA <- (d_A_t0$n+d_A_t1$n)/((d_A_t0$n+d_A_t1$n)+(d_B_t0$n+d_B_t1$n))
PR_d_RB <- (d_B_t0$n+d_B_t1$n)/((d_A_t0$n+d_A_t1$n)+(d_B_t0$n+d_B_t1$n))

PR_d_PA <- (d_A_t0$n+d_B_t1$n)/((d_A_t0$n+d_A_t1$n)+(d_B_t0$n+d_B_t1$n))
PR_d_PB <- (d_B_t0$n+d_A_t1$n)/((d_A_t0$n+d_A_t1$n)+(d_B_t0$n+d_B_t1$n))

print(PR_d_PA)
print(PR_d_PB)

d_trading_rate <- PR_d_RA*PR_d_PB + PR_d_RB*PR_d_PA
print(d_trading_rate)

phat_ <- d_trading_rate
n_ <- (d_A_t0$n+d_A_t1$n)+(d_B_t0$n+d_B_t1$n)

# H0: trading rate = 0.5 vs H1: trading rate != 0.5
s_ <- sqrt(phat_*(1-phat_)/n_)
t_ <- (phat_-0.5)/s_
pval_ <- 2 * pnorm(-abs(t_))
print(t_)
print(pval_)

# dealer
# we can not reject the test with 5% significance level..

# From the data, we can know that no significant difference in their behavior.
# and no endowment effect.