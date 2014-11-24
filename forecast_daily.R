source("run.R",chdir=T)

close_forecast = function (data, field, method) {
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
  result = list(current_week=current_week_data, next_week = next_week_data)
  return(result)
}

weekly_plot = function(data, title = element_blank()) {
  data.m = melt(data,id="date")
  p = ggplot(data.m, aes(x = strftime(date,"%Y-%m-%d"), y=value, label = value)) + 
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
    ggtitle(title)
  return(p)
}

daily_forecast_count = close_forecast(cust, "count", "HoltWinters")
daily_forecast_spend = close_forecast(cust, "spend", "HoltWinters")
daily_forecast_revenue = list()
daily_forecast_revenue$current_week = data.frame(
  date = daily_forecast_spend$current_week$date,
  actual = daily_forecast_spend$current_week$actual * daily_forecast_count$current_week$actual,
  initial_forecast = daily_forecast_spend$current_week$initial_forecast * daily_forecast_count$current_week$initial_forecast,
  forecast = daily_forecast_spend$current_week$forecast * daily_forecast_count$current_week$forecast
  )
daily_forecast_revenue$next_week = data.frame(
  date = daily_forecast_spend$next_week$date,
  forecast = daily_forecast_spend$next_week$forecast * daily_forecast_count$next_week$forecast
)

make_slide = function(data) {
  multiplot(data$count, data$spend, data$revenue, cols = 2)
}

current_week_plot = list(
  count = weekly_plot(daily_forecast_count$current_week, "Customers"),
  spend = weekly_plot(daily_forecast_spend$current_week, "Average ticket"),
  revenue = weekly_plot(daily_forecast_revenue$current_week, "Total income"))

next_week_plot = list(
  count = weekly_plot(daily_forecast_count$next_week, "Customers"),
  spend = weekly_plot(daily_forecast_spend$next_week, "Average ticket"),
  revenue = weekly_plot(daily_forecast_revenue$next_week, "Total income"))


make_slide(current_week_plot)
multiplot(current_week_plot$count, current_week_plot$spend, current_week_plot$revenue, cols = 2)

#current_week_customers_plot = weekly_plot(weekly_forecast$current_week, "Customers")
#next_week_customers_plot = weekly_plot(weekly_forecast$next_week, "Customers")
#multiplot(current_week_customers_plot)
#multiplot(weekly_plot(weekly_forecast$current_week, "Customers"),
#          weekly_plot(weekly_forecast$next_week))