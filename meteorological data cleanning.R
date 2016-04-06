setwd("C:/Users/Koohoko/Desktop/roaming/work/本科毕业设计/time")
#import case data
casedata <- read.table('case.txt')
names(casedata) <- c('date','GOPC','GP')
str(casedata)
casedata[,1]
#change the first col to date format

#import meteorological data
for(i in 2010:2016){
      name = paste0(i,'.csv')
      assign(paste0('data,', i), read.csv(name,header = F))
}
      
      