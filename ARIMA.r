chooseCRANmirror()
library(tseries)
library(forecast)
library(hydroGOF)

#import data
setwd("C:/Users/Koohoko/Desktop/roaming/work/本科毕业设计/Time-series-analysis")
data <- read.csv('medata.csv',row.names = 'X')
GPcase <- read.table('case.txt')[,3]
str(data)

#the daily environmental data were converted into weekly resolution by
#taking the average
week_average <- function(a){
      result <- 0
      for (i in 1:325){
            aver <- mean(a[(7*i-5):(i*7+1)])
            result <- c(result, aver)
      }
      result <- result[-1]
      return(result)
}

pressure <- week_average(data[,2])
temperature <- week_average(data[,4])
dew_point <- week_average(data[,6])
humidity <- week_average(data[,7])
sunshine <- week_average(data[,10])
data[,9][data[,9] == 'Trace'] <- 0
data[,9] <- as.numeric(as.character(data[,9]))
rainfall <- week_average(data[,9])
length(rainfall)

#making time series objects
GPcasets <- ts(GPcase[1:260],frequency = 52,start = 2010)
# summary(GPcasets)
# plot(GPcasets)
# abline(reg=lm(GPcasets~time(GPcasets)))
# cycle(GPcasets)
# plot(aggregate(GPcasets,FUN=mean))
# boxplot(GPcasets~cycle(GPcasets))
# adf.test(GPcasets, alternative="stationary", k=0)
# acf(GPcasets,lag.max = 260)

#we need to remove unequal variances. We do this using log of the series
logGPcasets <- ts(log(GPcase[1:260]),frequency = 52,start = 2010)
summary(logGPcasets)
plot(logGPcasets)
plot(diff(logGPcasets))
cycle(logGPcasets)
plot(aggregate(logGPcasets,FUN=mean))
boxplot(logGPcasets~cycle(logGPcasets))
adf.test(logGPcasets, alternative="stationary", k=0) #the result shows that the case series is already stationary
acf(diff(logGPcasets),lag.max = 260)
pacf(diff(logGPcasets))

#ARIMA
#change the paratmeters when needed
# fit <- arima(logGPcasets,c(1,1,1))
# tsdiag(fit)
# summary(fit)
# (1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))*2#P-value
# 
# pred <- predict(fit, n.ahead = 52)
# ts.plot(GPcasets,exp(pred$pred), log = "y", lty = c(1,3))
# ts.plot(ts(GPcase,frequency = 52,start = 2010))#real cases
# rmse(pred$pred, log(GPcase[261:312]))
# #sqrt(mean((pred$pred-log(GPcase[261:312]))^2))
# 
# #SARIMA
# acf(logGPcasets, lag.max = 260)
# pacf(logGPcasets, lag.max = 260)
# acf(diff(logGPcasets, 52), lag.max = 260 )
# pacf(diff(logGPcasets, 52), lag.max = 260)
# acf(diff(diff(logGPcasets, 52)), lag.max = 260 )
# pacf(diff(diff(logGPcasets, 52)), lag.max = 260)
# 
# sfit <- arima(logGPcasets, c(0, 1, 2), seasonal = list(order = c(0, 1, 1), period = 52))
# tsdiag(sfit)
# summary(sfit)
# (1-pnorm(abs(sfit$coef)/sqrt(diag(sfit$var.coef))))*2#P-value
# 
# spred <- predict(sfit, n.ahead = 52)
# ts.plot(ts(GPcase[1:312],frequency = 52,start = 2010),exp(spred$pred), log = "y", lty = c(1,1), col = c(1,2))
# rmse(spred$pred, log(GPcase[261:312]))
# #ts.plot(ts(GPcase[1:312],frequency = 52,start = 2010))#real cases

#predict the last half year
log286 <- ts(log(GPcase[1:286]),frequency = 52,start = 2010)
GPcase286 <- ts(GPcase[1:286],frequency = 52,start = 2010)
acf(diff(log286),lag.max = 286)
pacf(diff(log286))

#ARIMA
fit <- arima(log286,c(2,1,2))
tsdiag(fit)
summary(fit)
(1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))*2#P-value
pred <- predict(fit, n.ahead = 26)
ts.plot(ts(GPcase[1:312],frequency = 52,start = 2010),exp(pred$pred), log = "y", lty = c(1,1), col = c(1,2))
rmse(pred$pred, log(GPcase[287:312]))
#sqrt(mean((pred$pred-log(GPcase[261:312]))^2))

#SARIMA
acf(log286, lag.max = 260 )
pacf(log286, lag.max = 260)
acf(diff(log286, 52), lag.max = 260 )
pacf(diff(log286, 52), lag.max = 260)
acf(diff(diff(log286, 52)), lag.max = 260 )
pacf(diff(diff(log286, 52)), lag.max = 260)

sfit <- arima(log286, c(0, 1, 2), seasonal = list(order = c(0, 1, 1), period = 52))
tsdiag(sfit)
summary(sfit)
(1-pnorm(abs(sfit$coef)/sqrt(diag(sfit$var.coef))))*2#P-value

spred <- predict(sfit, n.ahead = 26)
ts.plot(ts(GPcase[1:312],frequency = 52,start = 2010),exp(spred$pred), log = "y", lty = c(1,1), col = c(1,2))
rmse(spred$pred, log(GPcase[287:312]))
