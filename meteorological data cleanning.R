setwd("C:/Users/Koohoko/Desktop/roaming/work/本科毕业设计/Time-series-analysis")
#import case data
casedata <- read.table('case.txt')
names(casedata) <- c('date','GOPC','GP')
str(casedata)
casedata[,1] <- as.character(casedata[,1])
##change the first col to date format
for (i in 1:52){
      casedata[i,1] <- paste0('2010/',casedata[i,1])
}
for (i in 53:105){
      casedata[i,1] <- paste0('2011/',casedata[i,1])
}
for (i in 106:157){
      casedata[i,1] <- paste0('2012/',casedata[i,1])
}
for (i in 158:209){
      casedata[i,1] <- paste0('2013/',casedata[i,1])
}
for (i in 210:261){
      casedata[i,1] <- paste0('2014/',casedata[i,1])
}
for (i in 262:313){
      casedata[i,1] <- paste0('2015/',casedata[i,1])
}
for (i in 314:325){
      casedata[i,1] <- paste0('2016/',casedata[i,1])
}
casedata$Date <-  as.Date(casedata[,1],'%Y/%d/%m')
str(casedata)

#import meteorological data
medata <- data.frame()
for(i in 2010:2016){
      name = paste0(i,'.csv')
      assign(paste0('data.', i), read.csv(name,header = F))
      medata = rbind(medata, read.csv(name,header = F, stringsAsFactors = F))
}
str(medata)

##generate week ID for medata
id = rep(1:325, each = 7)
id <- c(0,id,rep(326,6))
length(id)
medata$id <- id


##add GP cases number to medata
GP <- casedata$GP/7
GP <- rep(GP, each = 7)
length(GP)
GP <- c(0,GP)
medata <- medata[1:2276,]
medata$GP <- GP
str(medata)

##add col names
names(medata) <- c('Day','Mean Pressure (hPa)','Absolute Daily Max Air Temperature (deg. C)', 'Mean Air Temperature (deg. C)', 'Absolute Daily Min Air Temperature (deg. C)', 'Mean Dew Point (deg. C)', 'Mean Relative Humidity (%)', 'Mean Amount of Cloud (%)', 'Total Rainfall (mm)', 'Total Bright Sunshine (hours)', 'Prevailing Wind Direction (degrees)', 'Mean Wind Speed (km/h)', 'WeekID', 'GP')
str(medata)

##add date
seq <- seq(14610, 14610+2275)
medata$Date <- as.Date(seq, origin = "1970-01-01")

##save csv
write.csv(medata,'medata.csv')

