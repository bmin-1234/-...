rm(list=ls())

setwd('C:/Users/82105/Desktop/계량경제/R')

library(readxl)
CASchools <- read_excel('caschool (1).xlsx')

lm(testscr ~ str, data = CASchools)

plot(testscr ~ str, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")

linear_model <- lm(testscr ~ str, data = CASchools)
abline(linear_model) 
