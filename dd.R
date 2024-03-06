rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\014")
setwd('C:/Users/82105/Desktop/계량경제/R/과제')

library(readxl)
Returns <- read_excel('please.xlsx')

A <- xts(x = Returns$ExReturn, order.by = Returns$Month)["1931::2002"]
AA <- xts(400 * log(GDP/lag(GDP)))
