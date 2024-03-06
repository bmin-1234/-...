# Comparing means

# # # # # # # # # # # # # # # # # # # # # # # # # 
# 1. CA class size example in the lecture slide
Ybar_s <- 657.4
Ybar_l <- 650.0
sY_s <- 19.4
sY_l <- 17.9
n_s <- 238
n_l <- 182

dhat <- Ybar_s - Ybar_l
se <- sqrt(sY_s^2/n_s + sY_l^2/n_l)
t <- (dhat-0)/se

pval_a <- 2*pnorm(-abs(t)) #H_1: d!=0
pval_b <- 1 - pnorm(t) #H_1: d>0
pval_c <- pnorm(t) #H_1: d<0


# # # # # # # # # # # # # # # # # # # # # # # # # 
# 2. Gender wage gap over time

# clean workspace
rm(list=ls())

# find the current working directory
getwd() 
# set working directory
setwd('C:/Users/82105/Desktop/계량경제/R')
#install.packages(c("readxl", "dplyr"))

# load readxl package
library(readxl)
cps <- read_excel('CPS96_15.xlsx') #import data 

# data description
nrow(cps)
names(cps) #colnames(cps)
head(cps)
tail(cps)
summary(cps)

# load dplyr package
library(dplyr)
# group data by gender and year and compute the mean, 
# standard deviation, and number of obs. for each group
avgs <- cps %>% 
  group_by(female, year) %>% 
  summarise(mean(ahe), sd(ahe), n())
# The pipe operator %>% chain different R functions that
# produce compatible input and output. Here, we take the 
# dataset cps and use it as an input for the function 
# group_by(). The output of group_by is subsequently  
# used as an input for summarise().

# print the results to the console
print(avgs)

# split the dataset by gender
avgs_m <- avgs %>% filter(female == 0) 
avgs_f <- avgs %>% filter(female == 1)

# rename columns of both splits
colnames(avgs_m) <- c("Female", "Year", "Y_bar_m", "s_m", "n_m")
colnames(avgs_f) <- c("Female", "Year", "Y_bar_f", "s_f", "n_f")

# estimate gender gaps, compute SEs and CIs for 1996 & 2015
gap <- avgs_m$Y_bar_m - avgs_f$Y_bar_f
gap_se <- sqrt(avgs_m$s_m^2 / avgs_m$n_m + avgs_f$s_f^2 / avgs_f$n_f)
gap_t <- (gap - 0) / gap_se #H_0: gender wage gap is zero
gap_pval <- 2 * pnorm(-abs(gap_t)) #two-sided alternative
gap_ci_l <- gap - 1.96 * gap_se #95% CI
gap_ci_u <- gap + 1.96 * gap_se

result <- cbind(avgs_m[,-1], avgs_f[,-(1:2)], gap, gap_se, gap_t, gap_pval, gap_ci_l, gap_ci_u)
print(result, digits = 3)
