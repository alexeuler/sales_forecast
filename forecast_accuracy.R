source("run.R",chdir=T)

model_accuracy = function(model_name, data, training_start, training_end, frequency, nforecast) {
  training_start = as.Date(training_start)
  training_end = as.Date(training_end)
  data = data[data$date<=training_end,]
  data = data[data$date>=training_start,]
  fit = do.call(model_name,args=list(ts(data$count, frequency = frequency)))
  fcast_fit = forecast(fit, h=nforecast)
  fcast = c(fcast_fit$mean)
  test = cust_weekly_filtered[cust_weekly_filtered$date>training_end,]
  test = test[1:nforecast,]
  diff = test$count - fcast
  diff = diff %*% diff
  return(diff[1,1])
}

make_test = function(model_name, data, dates, frequency, nforecast) {
  
  error = 0
  for (date in dates) {
    error = error + model_accuracy(model_name,data,"2008-01-01",date, frequency, nforecast)
  }
  return(error)
}

weekly_params = function() {
  result = list(frequency = 52, nforecast = 13, data = cust_weekly,
                dates = c("2013-01-01","2013-04-01","2013-07-01","2013-10-01","2014-01-01","2014-04-01", "2014-07-01"))
  return(result)
}

params = weekly_params()
print(make_test("stlm", params$data, params$dates, params$frequency, params$nforecast))
print(make_test("HoltWinters", params$data, params$dates, params$frequency, params$nforecast))
print(make_test("bats", params$data, params$dates, params$frequency, params$nforecast))
#print(model_accuracy("HoltWinters", "2008-01-01", "2014-07-01", 12))
#print(model_accuracy("auto.arima", "2008-01-01", "2014-07-01", 12))