rm(list=ls())
setwd('C:/Users/82105/Desktop/계량경제/birthweight_smoking')

library(readxl)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)

bwgt <- read_excel('birthweight_smoking.xlsx')

### 7.1 (a)
reg1 <- lm(birthweight ~ smoker, data = bwgt)
Cov1 <- vcovHC(reg1, type = "HC1")
robustse1 <- sqrt(diag(Cov1))


### 7.1 (a)
reg2 <- lm(birthweight ~ smoker + alcohol + nprevist, data = bwgt)
Cov2 <- vcovHC(reg2, type = "HC1")
robustse2 <- sqrt(diag(Cov2))

### 7.1 (a)
reg4 <- lm(birthweight ~ smoker + alcohol + nprevist + unmarried, data = bwgt)
Cov4 <- vcovHC(reg4, type = "HC1")
robustse4 <- sqrt(diag(Cov4))

reg5 <- lm(birthweight ~ smoker + alcohol + nprevist + unmarried + age + educ, data = bwgt)
Cov5 <- vcovHC(reg5, type = "HC1")
robustse5 <- sqrt(diag(Cov5))

stargazer(reg1, reg2, reg4, reg5, se = list(robustse1, robustse2, robustse4, robustse5), 
          type = "text", title = "PS7 SW E7.1", keep.stat = c("n", "ser", "adj.rsq"), 
          digits = 3, order = c("smoker","alcohol","nprevist","unmarried","age","educ"))

### 7.1 (b), 7.1 (e)-i
coefci(reg1, vcov=Cov1)
coefci(reg2, vcov=Cov2)
coefci(reg4, vcov=Cov4)

### 7.1 (c)
# Yes it seems so. The coefficient falls by roughly 30 percent in magnitude when
# additional regressors are included in the regression as in column (2) of the table. 
# This change is substantively large and large relative to the standard error of the
# coefficient estimate on Smoker in regression (1).

### 7.1 (d)
# Yes it seems so. The coefficient falls by roughly 20 percent in magnitude when
# unmarried is added as an additional regression. This change is substantively 
# large and large relative to the standard error of the coefficient estimate in 
# regression (2).

### 7.1 (e)
# i.   [-241.40139, -132.86508]
# ii.  Yes. The 95% confidence interval does not include zero. 
# iii. Yes. On average, birthweight is 187 grams lower for unmarried mothers.
# iv.  No. As the question suggests, unmarried is a control variable that captures the 
# effects of several factors that differ between married and unmarried mothers such 
# as age, education, income, diet and other health factors, and so forth.

### 7.1 (f), 7.1 (b), 7.1 (e)-i
stargazer(reg1, reg2, reg4, reg5, se = list(robustse1, robustse2, robustse4, robustse5), ci = T, 
          type = "text", title = "PS7 SW E7.1", keep.stat = c("n", "ser", "adj.rsq"), 
          digits = 3, order = c("smoker","alcohol","nprevist","unmarried","age","educ"))

# When Age and Educ (years of education) are included in the regression, the coefficient 
# on Smoker is very similar to its value in regression (3).
