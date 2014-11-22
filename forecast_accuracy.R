source("run.R",chdir=T)
cust_weekly_filtered = cust_weekly[cust_weekly$week!=53,]

model_accuracy = function(model_name, training_start, training_end, nforecast) {
  training_start = as.Date(training_start)
  training_end = as.Date(training_end)
  data = cust_weekly_filtered[cust_weekly_filtered$date<=training_end,]
  data = data[data$date>=training_start,]
  fit = do.call(model_name,args=list(ts(data$count, frequency = 52)))
  fcast_fit = forecast(fit, h=nforecast)
  fcast = c(fcast_fit$mean)
  test = cust_weekly_filtered[cust_weekly_filtered$date>training_end,]
  test = test[1:nforecast,]
  diff = test$count - fcast
  diff = diff %*% diff
  return(diff[1,1])
}

make_test = function(model_name) {
  dates = c("2013-01-01","2013-04-01","2013-07-01","2013-10-01","2014-01-01","2014-04-01", "2014-07-01")
  error = 0
  for (date in dates) {
    error = error + model_accuracy(model_name,"2008-01-01",date,13)
  }
  return(error)
}


print(make_test("stlm"))
print(make_test("HoltWinters"))
print(make_test("bats"))
#print(model_accuracy("HoltWinters", "2008-01-01", "2014-07-01", 12))
#print(model_accuracy("auto.arima", "2008-01-01", "2014-07-01", 12))