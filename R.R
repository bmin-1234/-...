rm(list=ls())
setwd('C:/Users/82105/Desktop/계량경제/birthweight_smoking')

library(readxl)
DATA <- read_excel('birthweight_smoking.xlsx')

# E5.3 a.
'''avgs <- mean(DATA$birthweight)
print(avgs)'''

library(dplyr)
library(lmtest)
library(sandwich)

'''avgs <- DATA %>% 
  group_by(smoker) %>% 
  summarise(mean=mean(birthweight), sd=sd(birthweight), n=n())

print(avgs)'''

# E5.3 b.    E7.1 (1)

olsb_1 <- lm(birthweight ~ smoker, data = DATA)
Covb <- vcovHC(olsb_1, type = "HC2")
coeftest(olsb_1, vcov. = Covb)
coefci(olsb_1, vcov. = Covb)

# E6.1 a.
# y_hat = 3432.06 - 253.228 x1_hat


# E7.1 (2)
olsb_2 <- lm(birthweight ~ smoker + alcohol + nprevist, data = DATA)
Covb <- vcovHC(olsb_2, type = "HC2")
coeftest(olsb_2, vcov. = Covb)
coefci(olsb_2, vcov. = Covb)

# E6.1 b.

'''summary(olsb)'''

# E6.1 c.

'''b1 <- lm(birthweight ~ alcohol + nprevist, data = DATA)
b2 <- lm(smoker ~ alcohol + nprevist, data = DATA)
b3 <- lm(b1$resid ~ b2$resid)

coeftest(b3, Vcov. = Covb)'''


# E6.1 d.
'''olsb <- lm(birthweight ~ smoker + alcohol + tripre2 + tripre3+ tripre0, data = DATA)
Covb <- vcovHC(olsb, type = "HC2")
coeftest(olsb, vcov. = Covb)
coefci(olsb, vcov. = Covb)


summary(olsb)'''


# E7.1 (3)

olsb_3 <- lm(birthweight ~ smoker + alcohol + nprevist+unmarried, data = DATA)
Covb <- vcovHC(olsb_3, type = "HC2")
coeftest(olsb_3, vcov. = Covb)
coefci(olsb_3, vcov. = Covb)


# E7.1 a.
# (1) : -253.228 (26.821),  (2) : -217.5801 (26.1244), (3) : -175.3769 (26.8456)

# E7.1 b.
# (1) (-305.866, -200.5907), (2) (-270.21771, -164.94244) , (3) (-228.01455, -122.7393)

# E7.1 c. d.
# Each regressor was added, the coefficient on Smoker became smaller.
# So the coefficient on Smorker seems to be suffer from omitted variable bias.


# E7.1 e.
# ⅰ. (-241.42916, -132.8373)
# ⅱ. The t-value is -6.7578 and p-value is 0.00000000001677 (near to 0)
# So the coefficient is statistically significant at 1% significant level
# ⅲ. the magnitude of coefficient is 187.1332 that is most biggest magnitude in regression.  
# ⅳ. Marital status is not the issue. We need to analyze the environment 
# of why They couldn't get married and how They got a child. I think They have no ability
# to nurture the child.

# E7.1 f.
olsb_4 <- lm(birthweight ~ smoker + alcohol + nprevist+unmarried+drinks, data = DATA)
Covb <- vcovHC(olsb_4, type = "HC2")
coeftest(olsb_4, vcov. = Covb)
coefci(olsb_4, vcov. = Covb)

# the coefficient of dirks. -0.49425 (17.57981) CI : (-34.71174, 33.68584)
# t-value is -0.0281. It is not statistically significant at 5% level.
