source("run.R",chdir=T)

model_accuracy = function(model_name, data, field,training_start, training_end, frequency, nforecast) {
  training_start = as.Date(training_start)
  training_end = as.Date(training_end)
  data_tr = data[data$date<=training_end,]
  data_tr = data_tr[data_tr$date>=training_start,]
  fit = do.call(model_name,args=list(ts(data_tr[[field]], frequency = frequency)))
  fcast_fit = forecast(fit, h=nforecast)
  fcast = c(fcast_fit$mean)
  test = data[data$date>training_end,]
  test = test[1:nforecast,]
  diff = test[[field]] - fcast
  diff = diff %*% diff
  return(diff[1,1])
}

make_test = function(model_name, data, field, dates, frequency, nforecast) {
  
  error = 0
  for (date in dates) {
    error = error + model_accuracy(model_name,data,field,"2008-01-01",date, frequency, nforecast)
  }
  return(error)
}

weekly_params = function() {
  result = list(frequency = 52, nforecast = 13, data = cust_weekly,
                dates = c("2013-01-01","2013-04-01","2013-07-01","2013-10-01","2014-01-01","2014-04-01", "2014-07-01"))
  return(result)
}

daily_params = function() {
  result = list(frequency = 6, nforecast = 14, data = cust,
                dates = c("2014-01-01","2014-02-13","2014-02-28","2014-04-10","2014-05-01","2014-06-25", "2014-07-01", "2014-07-15", "2014-08-07","2014-09-21","2014-10-01"))
  return(result)
}


params = weekly_params()
#params = daily_params()
print(make_test("stlm", params$data, "spend",params$dates, params$frequency, params$nforecast))
print(make_test("HoltWinters", params$data, "spend", params$dates, params$frequency, params$nforecast))
print(make_test("bats", params$data, "spend", params$dates, params$frequency, params$nforecast))
#print(make_test("stlm", params$data, "count",params$dates, params$frequency, params$nforecast))
#print(make_test("HoltWinters", params$data, "count", params$dates, params$frequency, params$nforecast))
#print(make_test("bats", params$data, "count", params$dates, params$frequency, params$nforecast))

#print(model_accuracy("HoltWinters", "2008-01-01", "2014-07-01", 12))
#print(model_accuracy("auto.arima", "2008-01-01", "2014-07-01", 12))