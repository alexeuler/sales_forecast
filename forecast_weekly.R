forecast_weekly = function(data, field, method) {
  last_week = as.numeric(tail(data, 1)$week)
  last_year = as.numeric(tail(data, 1)$year)
  last_weekyear = tail(data, 1)$weekyear
  first_week_of_the_last_quarter = floor(last_week / 13) * 13 + 1
  current_quarter_data = data.frame(
    week = seq(first_week_of_the_last_quarter, length.out = 13, by = 1),
    year = rep(last_year, 13))
  current_quarter_data$weekyear = paste(current_quarter_data$year, current_quarter_data$week, sep="-")
  current_quarter_data = merge(current_quarter_data, data, by = "weekyear", all.x = T)
  names(current_quarter_data)[names(current_quarter_data)=="week.x"] = "week"
  names(current_quarter_data)[names(current_quarter_data)=="year.x"] = "year"
  current_quarter_data = current_quarter_data[,c("week","year", "weekyear", field)]
  names(current_quarter_data)[names(current_quarter_data)==field] = "actual"
  data$weekyear = factor(data$weekyear, ordered = T)
  initial_data = data[data$weekyear<paste(last_year,first_week_of_the_last_quarter, sep="-"),]
  #making initial forecast
  ts_weekly = ts(initial_data[[field]], frequency = 52)
  fit_weekly = do.call(method, list(ts_weekly))
  fcast_weekly = forecast(fit_weekly, 13)
  current_quarter_data$initial_forecast = round(c(fcast_weekly$mean))
  current_quarter_data$date = as.Date(paste(current_quarter_data$year,"01", "01", sep="-")) + (current_quarter_data$week - 1) * 7
  
  #making forecast
  data = data[1:(nrow(data)-1),] #removing last week as it is not full
  ts_weekly = ts(data[[field]], frequency = 52)
  fit_weekly = do.call(method, list(ts_weekly))
  fcast_weekly = forecast(fit_weekly, 13*2)
  
  dates_frame = data.frame(week = seq(last_week, length.out=26 ,by=1))
  dates_frame$year = last_year + ifelse(dates_frame$week>52,1,0)
  dates_frame$week = (dates_frame$week - 1) %% 52 + 1
  dates_frame$weekyear = paste(dates_frame$year, sprintf("%02d",dates_frame$week), sep="-")
  
  two_quarter_fcast=data.frame(
    weekyear = dates_frame$weekyear[1:26],
    forecast = c(round(fcast_weekly$mean)))
  
  current_quarter_data = merge(current_quarter_data, two_quarter_fcast, by = "weekyear", all.x=T)
  
  next_quarter_data = data.frame(week = seq(first_week_of_the_last_quarter + 13, length = 13),
                                  year = last_year)
  next_quarter_data$year = last_year + ifelse(next_quarter_data$week>52,1,0)
  next_quarter_data$week = (next_quarter_data$week - 1) %% 52 + 1
  next_quarter_data$weekyear = paste(next_quarter_data$year, sprintf("%02d",next_quarter_data$week), sep="-")
  next_quarter_data$date = as.Date(paste(next_quarter_data$year,"01", "01", sep="-")) + (next_quarter_data$week - 1) * 7
  next_quarter_data = merge(next_quarter_data, two_quarter_fcast, by="weekyear")
  
  current_quarter_data = current_quarter_data[ , -which(names(current_quarter_data) %in% c("week","year", "weekyear"))]
  next_quarter_data = next_quarter_data[ , -which(names(next_quarter_data) %in% c("week","year", "weekyear"))]
  
  #current_quarter_data = subset(current_quarter_data, select=-c("week", "year", "weekyear"))
  #next_quarter_data = subset(next_quarter_data, select=-c("week", "year", "weekyear"))
  
  result = list(currentf=current_quarter_data, nextf = next_quarter_data)
  return(result)
}
