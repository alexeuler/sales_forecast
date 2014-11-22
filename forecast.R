source("run.R",chdir=T)

ts_to_frame = function(series,name="values", frequency = 1) {
  frame=data.frame(time=round((time(series)-1)*frequency))
  frame[[name]]=c(series)
  return(frame)
}

HWModel = function(series) {
  result = list()
  result$fit=HoltWinters(series)
  result$fitted=result$fit$fitted[,"xhat"]
  return(result)
}

STLModel = function(series) {
  result = list()
  result$fit=stlm(series)
  result$fitted=result$fit$model$fitted
  return(result)
}


cust_weekly_filtered = cust_weekly[cust_weekly$week!=53,]
data = list()

data$training = data.frame(actual = cust_weekly_filtered[cust_weekly_filtered$year<2014,]$count,
                           date = cust_weekly_filtered[cust_weekly_filtered$year<2014,]$date)
data$training$time=time(ts(data$training$actual))

data$test=data.frame(actual = cust_weekly_filtered[cust_weekly_filtered$year==2014,]$count,
                     date = cust_weekly_filtered[cust_weekly_filtered$year==2014,]$date)
data$test$time=seq(from=max(data$training$time)+1, length.out=length(data$test$actual))

model = STLModel(ts(data$training$actual, frequency = 52))
fit = model$fit
fitted_frame=ts_to_frame(model$fitted, name = "fitted", frequency = 52)
#fit <- HoltWinters(ts(data$training$actual, frequency = 52))
#fit <- stlm(ts(data$training$actual, frequency = 52))
#fitted_frame=ts_to_frame(fit$fitted[,"xhat"], name = "fitted", frequency = 52)
#fitted_frame=ts_to_frame(fit$model$fitted, name = "fitted", frequency = 52)
data$training = merge(data$training, fitted_frame, by="time", all=T)

fcast=forecast(fit,h=length(data$test$actual))
data$test$fitted = c(fcast$mean)

ggplot(data$training, aes(x=time)) + 
         geom_line(aes(y=actual)) + 
         geom_line(aes(y=fitted), colour = "red") +
         geom_vline(xintercept = max(data$training$time), linetype="dashed") +
         geom_line(data = data$test, aes(y=actual)) +
         geom_line(data = data$test, aes(y=fitted), colour = "red")
         