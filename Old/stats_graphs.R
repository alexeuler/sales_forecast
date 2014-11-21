stats_graphs = function (source_data) {
  par(mfrow=c(2,2))
  p1 = acf(weekly_cust_filtered$Cust...TOTAL, lag.max = 60)
  p2 = pacf(weekly_cust_filtered$Cust...TOTAL, lag.max = 60)
  
  #multiplot(p1,p2,cols=2)
  
}