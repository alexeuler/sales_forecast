source("run.R",chdir=T)

report_plot = function(data, title = element_blank()) {
  data.m = melt(data,id="date")
  p = ggplot(data.m, aes(x = strftime(date,"%Y-%m-%d"), y=value, label = value)) + 
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
    ggtitle(title) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
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
    count = report_plot(forecast_count[[time]], "Customers"),
    spend = report_plot(forecast_spend[[time]], "Average ticket"),
    revenue = report_plot(forecast_revenue[[time]], "Total income")))
}

make_slide = function(data, title) {

  df = data.frame()
  title = ggplot(df) + geom_point() + 
    xlim(0, 10) + 
    ylim(0, 10) + 
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(vjust=0.5)) +
    annotate("text", x = 5, y = 5, label = title, size=15)
  
  grid.arrange(title, arrangeGrob(data$count, data$spend, ncol=2), data$revenue, heights = c(1,4,4))
 
}


current_week_plot = make_forecast(cust, "currentf", "daily", MODEL)
next_week_plot = make_forecast(cust, "nextf", "daily", MODEL)

current_quarter_plot = make_forecast(cust_weekly, "currentf", "weekly", MODEL)
next_quarter_plot = make_forecast(cust_weekly, "nextf", "weekly", MODEL)

pdf("plots.pdf", onefile = TRUE, width = 16, height = 16 / 297 * 210)
make_slide(current_week_plot, "Current Week")
make_slide(next_week_plot, "Next Week")
make_slide(current_quarter_plot, "Current Quarter")
make_slide(next_quarter_plot, "Next Quarter")
dev.off()