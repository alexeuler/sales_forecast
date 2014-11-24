source("run.R",chdir=T)

weekly_plot = function(data, title = element_blank()) {
  data.m = melt(data,id="date")
  p = ggplot(data.m, aes(x = strftime(date,"%Y-%m-%d"), y=value, label = value)) + 
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
    ggtitle(title) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
}

make_slide = function(data) {
  multiplot(data$count, data$spend, data$revenue, cols = 2)
}

make_forecast = function (data, time, period, model) {
  forecast_count = do.call(paste("forecast",period, sep="_"), args = list(data, "count", model))
  forecast_spend = do.call(paste("forecast",period, sep="_"), args = list(data, "spend", model))
  forecast_revenue = list()
  forecast_revenue$currentf = data.frame(
    date = forecast_spend$currentf$date,
    actual = forecast_spend$currentf$actual * forecast_count$currentf$actual,
    initial_forecast = forecast_spend$currentf$initial_forecast * forecast_count$currentf$initial_forecast,
    forecast = forecast_spend$currentf$forecast * forecast_count$currentf$forecast
    )
  forecast_revenue$nextf = data.frame(
    date = forecast_spend$nextf$date,
    forecast = forecast_spend$nextf$forecast * forecast_count$nextf$forecast
  )
  return(list(
    count = weekly_plot(forecast_count[[time]], "Customers"),
    spend = weekly_plot(forecast_spend[[time]], "Average ticket"),
    revenue = weekly_plot(forecast_revenue[[time]], "Total income")))
}


#forecast_spend = do.call(paste("forecast","weekly", sep="_"), args = list(cust_weekly, "spend", "HoltWinters"))
#forecast_count = do.call(paste("forecast","weekly", sep="_"), args = list(cust_weekly, "count", "HoltWinters"))

current_week_plot = make_forecast(cust_weekly, "currentf", "weekly", "HoltWinters")
#next_week_plot = make_forecast(cust_weekly, "nextf", "weekly", "HoltWinters")

make_slide(current_week_plot)
#multiplot(current_week_plot$count, current_week_plot$spend, current_week_plot$revenue, cols = 2)

#current_week_customers_plot = weekly_plot(weekly_forecast$current_week, "Customers")
#next_week_customers_plot = weekly_plot(weekly_forecast$next_week, "Customers")
#multiplot(current_week_customers_plot)
#multiplot(weekly_plot(weekly_forecast$current_week, "Customers"),
#          weekly_plot(weekly_forecast$next_week))