#Time series analysis
#Data on quarterly GDP growth
#Replicating results in SW Chapter 15

# Clean workspace
rm(list=ls())
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014")

setwd('C:/Users/82105/Desktop/계량경제/R/와우')

library(readxl)
USmacro <- read_xlsx('us_macro_quarterly.xlsx')

# provides convenient functions for plotting and computing with time series data
library(quantmod) 
USmacro$Date <- as.yearqtr(USmacro$freq, format = "%Y:0%q") #in yearqtr format

GDP <- xts(x = USmacro$GDPC1, order.by = USmacro$Date)["1960::2017"]
GDPGR <- xts(400 * log(GDP/lag(GDP)))

# reproduce Figure 15.1 (a)
plot(log(as.zoo(GDP)),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "U.S. Quarterly Real GDP")

# reproduce Figure 15.1 (b) of the book
plot(as.zoo(GDPGR),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "U.S. Real GDP Growth Rates")

# autocorrelation
acf(na.omit(GDPGR), lag.max = 4, plot = F)

# subset data
GDPGRsub <- data.frame(GDPGR["1961-07::2017-07"],
                       Lag(GDPGR["1961-07::2017-07"], 1),
                       Lag(GDPGR["1961-07::2017-07"], 2))

colnames(GDPGRsub) <- c("GDPGR","GDPGR.L1","GDPGR.L2")

# AR(1) model
ar1 <- lm(GDPGR ~ GDPGR.L1, data = GDPGRsub[-2,])

# AR(2) model
ar2 <- lm(GDPGR ~ GDPGR.L1 + GDPGR.L2, data = GDPGRsub[-2,])

library(sandwich)
rob_se <- list(sqrt(diag(sandwich(ar1))),
               sqrt(diag(sandwich(ar2)))) #sandwich equivalent to vcovHC 

library(stargazer)
stargazer(ar1, ar2,
          se = rob_se,
          digits = 3,
          header = F,
          type = "text",
          omit.stat = "rsq",
          column.labels=c("AR(1)","AR(2)"),
          out = "Results.txt")

# forecast GDP growth rate in 2017:Q4
GDPGRsub2 <- data.frame("GDPGR" = GDPGR["2017-04::2017-10"],
                        "GDPGR.L1" = Lag(GDPGR["2017-04::2017-10"], 1),
                        "GDPGR.L2" = Lag(GDPGR["2017-04::2017-10"], 2))

colnames(GDPGRsub2) <- c("GDPGR","GDPGR.L1","GDPGR.L2")

library(forecast)
forecast(ar1, newdata = GDPGRsub2)
forecast(ar2, newdata = GDPGRsub2)
