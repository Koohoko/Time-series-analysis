setwd("C:/Users/Koohoko/Desktop/roaming/work/本科毕业设计/Time-series-analysis")
data <- read.csv('medata.csv',row.names = 'X')
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

week_mean_pressure <- week_average(data[,2])
