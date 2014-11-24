forecast_daily = function (data, field, method) {
  #preparing current_week_data frame
  last_day = tail(data, 1)$date
  last_uk_week = tail(data, 1)$uk_week
  last_uk_weekyear = tail(data, 1)$uk_weekyear
  last_week_data = data[data$uk_weekyear==last_uk_weekyear,]
  first_day_of_the_last_week = head(last_week_data$date,1) 
  current_week_data = data.frame(date=seq(first_day_of_the_last_week, length.out=6, by=1))
  current_week_data = merge(current_week_data, data, by="date", all.x=T, all.y=F)
  current_week_data = current_week_data[,c("date",field)]
  names(current_week_data)[names(current_week_data)==field] = "actual"
  #making initial forecast for the current week
  initial_data = data[data$date<first_day_of_the_last_week,]
  ts_daily = ts(initial_data[[field]], frequency = 6)  
  fit_daily = do.call(method, list(ts_daily))
  fcast_daily = forecast(fit_daily, 6)
  current_week_data$initial_forecast = round(c(fcast_daily$mean))
  #making forecast
  ts_daily = ts(data[[field]], frequency = 6)  
  fit_daily = do.call(method, list(ts_daily))
  fcast_daily = forecast(fit_daily, 12) # note that forecast excludes sundays
  dates_frame = data.frame(date = seq(last_day+1, length.out=20,by=1))
  dates_frame$weekday = strftime(dates_frame$date,"%w")
  dates_frame=dates_frame[dates_frame$weekday!=0,]
  two_week_fcast=data.frame(
    date = dates_frame$date[1:12],
    forecast = c(round(fcast_daily$mean)))
  current_week_data = merge(current_week_data, two_week_fcast, by="date", all.x=T)
  first_day_of_the_next_week = tail(current_week_data$date,1)+2
  next_week_data = data.frame(date = seq(first_day_of_the_next_week, length.out = 6, by = 1))
  next_week_data = merge(next_week_data, two_week_fcast, by="date", all.x=T)
  result = list(currentf=current_week_data, nextf = next_week_data)
  return(result)
}