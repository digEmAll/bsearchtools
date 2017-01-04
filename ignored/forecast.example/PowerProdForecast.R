####################################################################################################
#                                                                                                  #
#   In this script a forecast of the electric power production of the Netherlands is calculated.   #
#   This is performed using a loop, where for each sample to forecast we iteratively look for      #
#   the most "similar" historical observations in the past and we calculate their weighted mean.   #
#   The code is pretty self-explanatory, but let's give a simple example :                         #
#   To forecast the date 'Monday, 7 December, 2016', we first search all the values                #
#   in the past that are Mondays, on December and having a temperature close to temperature of     #
#   our forecast day.                                                                              #
#   If no value respecting all the conditions is found, we relax the last of the given conditions  #
#   (i.e. on the first iteration the temperature) until we found some values and we compute        #
#   the forecast as weighted mean of them.                                                         #
#                                                                                                  #
#   This is a very simplistic forecast approach, but its main purpose is to test and               #
#   see the performance of bsearchtools in a real-case.                                            #
#                                                                                                  #
#   The hourly Netherlands electric power production (2014-2016) has been downloaded from the      #
#   public site :   http://energieinfo.tennet.org/                                                 #
#                                                                                                  #
####################################################################################################


library(bsearchtools)


DF <- read.table(file='powerprod_2014_2016.csv',header=TRUE,sep=',',dec='.',
                 colClasses=c('character',rep.int('integer',5),'character','numeric'))

# here we split the multivariate series in 2 parts
# 2014-2015 : data used to estimate the following period
# 2016      : data used to verify the forecast error
hist <- DF[DF$year < 2016,]
# we add a weight column to give less
# importance to the oldest historical values
hist$weight <- 1:nrow(hist) 
bench <- DF[DF$year >= 2016,]

# we create a DFI object indexing all the columns exluding weight and power.prod
histDFI <- DFI(hist,indexes.col.names=colnames(hist)[!(colnames(hist) %in% c('weight','power.prod'))])

system.time({
  Rprof()
  forecast <- rep(NA,nrow(bench))
  for(i in 1:nrow(bench)){
    row <- bench[i,]
    
    orderedConditions <- list(
      # fcst$hour == hist$hour
      sameHour = EQ('hour',row$hour),
      # (hist$month-1)%%12 <= fcst$month <= (hist$month+1)%%12
      sameMonthRange = IN('month',c((row$month-1)%%12,row$month,(row$month+1)%%12)),
      # fcst$month == hist$month
      sameMonth = EQ('month',row$month),
      # fcst$holiday == hist$holiday
      sameHoliday = EQ('holiday',row$holiday),
      # fcst$week.day == hist$week.day
      sameWeekDay = EQ('week.day',row$week.day)
    )
    
    # first we try to get the values in the hist data.frame respecting all the conditions in the list 
    # if no values is found, then we remove the last condition in the list and we try again 
    # until we found some values. Finally we compute our forecast as weighted average of the values found.
    while(length(orderedConditions) > 0){
      samples <- DFI.subset(histDFI,filter=do.call(AND,orderedConditions),sort.indexes=FALSE, colFilter = c('weight','power.prod'))
      if(nrow(samples) > 0){
  
        forecast[i] <- sum(samples$power.prod * samples$weight) / sum(samples$weight)
        break
      }
      orderedConditions[[length(orderedConditions)]] <- NULL
    }
  }
  Rprof(NULL)
})
summaryRprof()
forecast <- round(forecast,0)

MAPE <- sum(abs((bench$power.prod - forecast) / bench$power.prod)) / length(forecast)
MAE <- sum(abs(bench$power.prod - forecast)) / length(forecast)
message('MAPE: ',MAPE)
message('MAE: ', MAE)

# verify that result is always the same (a further unit test)
expected.fcst <- read.table(file='forecast.csv',header=TRUE,sep=',',dec='.',colClasses=c('character','numeric'))
if(all.equal(data.frame(date=bench$date,power.prod.fcst=forecast,stringsAsFactors=FALSE),expected.fcst) != TRUE){
  stop('result is different from expected!')
}
#write.csv(data.frame(date=bench$date,power.prod.fcst=forecast,stringsAsFactors=FALSE),quote=F,row.names = FALSE,file='forecast.csv')

# create a plot with actual power production and forecast
png(filename='plot.png',type='cairo',width=1200,height=700,antialias='default')

minDate <- as.POSIXct(hist$date[1],"%Y-%m-%d %H:%M:%S",tz='GMT')
maxDate <- as.POSIXct(bench$date[nrow(bench)],"%Y-%m-%d %H:%M:%S",tz='GMT')
maxVal <- max(c(DF$power.prod,forecast))
actualColor <- '#0000CDCC'
forecastColor <- '#2E8B57CC'
plot(x=c(minDate,maxDate),y=c(0,maxVal),type='n',ylab='Power',xlab='Time',main='Netherlands Power Production')
lines(x=as.POSIXct(DF$date,"%Y-%m-%d %H:%M:%S",tz='GMT'),y=DF$power.prod,col=actualColor)
lines(x=as.POSIXct(bench$date,"%Y-%m-%d %H:%M:%S",tz='GMT'),y=forecast,type='l',col=forecastColor)
legend('bottomright',inset=0.02,legend=c('actual','forecast'),fill=c(actualColor,forecastColor))

dev.off()


