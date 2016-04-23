library(tseries)
library(forecast)

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
GPcasets <- ts(GPcase[-313:-325],frequency = 52,start = 2010)
summary(GPcasets)
plot(GPcasets)
abline(reg=lm(GPcasets~time(GPcasets)))
cycle(GPcasets)
plot(aggregate(GPcasets,FUN=mean))
boxplot(GPcasets~cycle(GPcasets))
adf.test(GPcasets, alternative="stationary", k=0)

#we need to remove unequal variances. We do this using log of the series
logGPcasets <- ts(log(GPcase[-313:-325]),frequency = 52,start = 2010)
summary(logGPcasets)
plot(logGPcasets)
abline(reg=lm(logGPcasets~time(logGPcasets)))
cycle(logGPcasets)
plot(aggregate(logGPcasets,FUN=mean))
boxplot(logGPcasets~cycle(logGPcasets))
adf.test(logGPcasets, alternative="stationary", k=0) #the result shows that the case series is already stationary
acf(logGPcasets)
pacf(logGPcasets)
auto.arima(logGPcasets)
fit <- arima(logGPcasets, c(1, 1, 1))
summary(fit)
tsdiag(fit)
#fit <- arima(logGPcasets, c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 52))
forecast <- forecast.Arima(fit,h=13,level=c(99.5))
forecast
plot.forecast(forecast)

# pred <- predict(fit, n.ahead = 13)
# ts.plot(GPcasets,exp(pred$pred), log = "y", lty = c(1,3))
# plot.ts(ts(GPcase,frequency = 52,start = 2010))#real cases
