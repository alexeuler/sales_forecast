source("init.R")

HW = function (source_data) {
  series = ts(source_data, start = c(1,1), end = c(7,26), frequency = 52)
  fit <- HoltWinters(series)
  par(mfrow=c(2,1))
  print(accuracy(fit))
  plot(fit)
  forecast = forecast(fit, 26)$mean
  
  plot(forecast(fit, 26))
}

HWA = function (source_data) {
  series = ts(source_data, frequency = 13)
  fit <- ets(series)
  par(mfrow=c(2,1))
  print(accuracy(fit))
  plot(fit)
  plot(forecast(fit, 13))
}



AR = function (source_data, p, d, q) {
  series = ts(source_data, frequency = 52)
  fit <- arima(series, order=c(p, d, q))
  par(mfrow=c(2,1))
  print(accuracy(fit))
  #plot(fit)
  plot(forecast(fit, 13))
}

ARA = function (source_data) {
  series = ts(source_data, frequency = 52)
  fit <- auto.arima(series)
  par(mfrow=c(2,1))
  print(fit)
  print(accuracy(fit))
  #plot(fit)
  plot(forecast(fit, 13))
}


#plot_cust(weekly_cust_filtered)
#acf(weekly_cust_filtered$Cust...TOTAL, lag.max = 60)
#pacf(weekly_cust_filtered$Cust...TOTAL, lag.max = 60)
#x <- ts(weekly_cust_filtered$Cust...TOTAL, frequency=365/7)
#fit <- tbats(x)
#seasonal <- !is.null(fit$seasonal)
#plot(ets(weekly_cust_filtered$Cust...TOTAL))
#seasonplot(weekly_cust_filtered$Cust...TOTAL)
#series = ts(weekly_cust_filtered$Cust...TOTAL, frequency = 52)
#fit <- stl(series, s.window="period")
#fit <- HoltWinters(series)
#par(mfrow=c(2,1))
#plot(fit)
#plot(forecast(fit, 52))

series = ts(weekly_cust_filtered$Cust...TOTAL, start = c(1,1), end = c(7,26), frequency = 52)
fit <- HoltWinters(series)
#par(mfrow=c(2,1))
#print(accuracy(fit))
#plot(fit)
#forecast = forecast(fit, 26)$mean
library(reshape2)
library(zoo)
fc = melt(forecast(fit, 26)$mean)$value
ggplot(fc) 

