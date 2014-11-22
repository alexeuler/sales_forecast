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

BATSModel = function(series) {
  result = list()
  result$fit=bats(series)
  result$fitted=result$fit$fitted.values
  return(result)
}

plot_model=function (model_name, start, middle, end) {
  start = as.Date(start)
  middle = as.Date(middle)
  end = as.Date(end)
  data = list()
  weekly_training = cust_weekly[cust_weekly$date<middle,]
  weekly_training = weekly_training[weekly_training$date>=start,]
  data$training = data.frame(actual = weekly_training$count,
                             date = weekly_training$date)
  data$training$time=time(ts(data$training$actual))
  weekly_test = cust_weekly[cust_weekly$date>=middle,]
  weekly_test = weekly_test[weekly_test$date<=end,]
  
  data$test=data.frame(actual = weekly_test$count,
                       date = weekly_test$date)
  data$test$time=seq(from=max(data$training$time)+1, length.out=length(data$test$actual))
  model = do.call(model_name,args=list(ts(data$training$actual, frequency = 52)))
  fit = model$fit
  fitted_frame=ts_to_frame(model$fitted, name = "fitted", frequency = 52)
  data$training = merge(data$training, fitted_frame, by="time", all=T)
  fcast=forecast(fit,h=length(data$test$actual))
  data$test$fitted = c(fcast$mean)
  
  plot = ggplot(data$training, aes(x=time)) + 
           geom_line(aes(y=actual)) + 
           geom_line(aes(y=fitted), colour = "red") +
           geom_vline(xintercept = max(data$training$time), linetype="dashed") +
           geom_line(data = data$test, aes(y=actual)) +
           geom_line(data = data$test, aes(y=fitted), colour = "red")
  return(plot)
}


plot_model("HWModel", "2008-01-01", "2014-07-01", "2014-10-01")