source("run.R",chdir=T)
ts_daily = ts(cust$count, frequency = 6)
last_day = tail(cust, 1)$date
last_uk_week = tail(cust, 1)$uk_week
last_uk_weekyear = tail(cust, 1)$uk_weekyear
last_week_cust = cust[cust$uk_weekyear==last_uk_weekyear,]
first_day_of_the_last_week = head(last_week_cust$date,1) 
current_week_data = data.frame(date=seq(first_day_of_the_last_week, length.out=6, by=1))
current_week_data = merge(current_week_data, cust, by="date", all.x=T, all.y=F)
current_week_data = current_week_data[,c("date","count")]
names(current_week_data)[names(current_week_data)=="count"] = "actual"
fit_daily = bats(ts_daily)
fcast_daily = forecast(fit_daily, 12)
# note that forecast excludes sundays
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