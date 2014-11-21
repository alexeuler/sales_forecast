plot_cust = function (source_data) {
  p1 = ggplot(source_data, aes(x = X..of.the.week, y=Cust...TOTAL, group=factor(Year), colour=factor(Year))) + geom_line()  + theme_bw()
  
  p2 = ggplot(source_data, aes(x = factor(X..of.the.week), y=Cust...TOTAL)) + 
    geom_boxplot() + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  p3 = ggplot(source_data, aes(x=weekYear, y=Cust...TOTAL, group=1)) +
    geom_line() +
    theme_bw() +
    stat_smooth() + 
    scale_x_discrete(breaks=source_data$weekYear[seq(1,length(source_data$weekYear), 52)]) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  multiplot(p1,p2,p3,cols=2)
}