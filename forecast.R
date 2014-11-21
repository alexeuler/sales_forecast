source("run.R",chdir=T)

ts_to_frame = function(series,name="values") {
  frame=data.frame(time=time(series))
  frame[[name]]=c(series)
  return(frame)
}

cust_weekly_filtered = cust_weekly[cust_weekly$week!=53,]
data = list()
data$training = data.frame(actual = cust_weekly_filtered[cust_weekly_filtered$year<2014,]$count,
                           date = cust_weekly_filtered[cust_weekly_filtered$year<2014,]$date)
data$training$time=time(ts(data$training$actual,frequency=52))
data$test=data.frame(actual = cust_weekly_filtered[cust_weekly_filtered$year==2014,]$count,
                     date = cust_weekly_filtered[cust_weekly_filtered$year==2014,]$date)
data$test$time=time(ts(data$test$actual,frequency=52))
fit <- HoltWinters(ts(data$training$actual, frequency = 52))
fcast=forecast(fit,h=length(data$test$actual))
fitted_frame=ts_to_frame(fit$fitted[,"xhat"], name = "fitted")
data$training = merge(data$training, fitted_frame, by="time", all=T)
data$test$fitted = c(fcast$mean)

#data$training$fitted=c(fit$fitted[,"xhat"])
#data$test$fitted=c(fcast$mean)
ggplot(data$training)+geom_line(aes(y=actual))