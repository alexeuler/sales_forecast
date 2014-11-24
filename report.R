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
    revenue = report_plot(forecast_revenue[[time]], "Total income"),
    fcount = forecast_count,
    fspend = forecast_spend,
    frevenue = forecast_revenue))
}

make_slide = function(data, title, subtitle) {

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
          panel.background = element_blank()) +
    annotate("text", x = 5, y = 5, label = title, size=15) + 
    annotate("text", x = 5, y = 1, label = subtitle, size=7)
  
  grid.arrange(title, arrangeGrob(data$count, data$spend, ncol=2), data$revenue, heights = c(1,3,3))
 
}


current_week_plot = make_forecast(cust, "currentf", "daily", MODEL)
next_week_plot = make_forecast(cust, "nextf", "daily", MODEL)
current_quarter_plot = make_forecast(cust_weekly, "currentf", "weekly", MODEL)
next_quarter_plot = make_forecast(cust_weekly, "nextf", "weekly", MODEL)

current_week_subtitle = paste("(", min(current_week_plot$fcount$currentf$date), " - ", max(current_week_plot$fcount$currentf$date), ")", sep="")
next_week_subtitle = paste("(", min(next_week_plot$fcount$nextf$date), " - ", max(next_week_plot$fcount$nextf$date), ")", sep="")
current_quarter_subtitle = paste("(", min(current_quarter_plot$fcount$currentf$date), " - ", max(current_quarter_plot$fcount$currentf$date) + 6, ")", sep="")
next_quarter_subtitle = paste("(", min(next_quarter_plot$fcount$nextf$date), " - ", max(next_quarter_plot$fcount$nextf$date) + 6, ")", sep="")


time = gsub("[:]","-",Sys.time())
time = gsub("[ ]",", ",time)
name = paste(getwd(),"/Reports/Report ",time, ".pdf", sep="")
pdf(name, onefile = TRUE, width = 16, height = 16 / 297 * 210)
make_slide(current_week_plot, "Current Week", current_week_subtitle)
make_slide(next_week_plot, "Next Week", next_week_subtitle)
make_slide(current_quarter_plot, "Current Quarter", current_quarter_subtitle)
make_slide(next_quarter_plot, "Next Quarter", next_quarter_subtitle)
dev.off()