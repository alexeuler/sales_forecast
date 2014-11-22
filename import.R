df = readWorksheetFromFile(file = "Shop Figures v8.xlsx", sheet = "Takings", endCol = 13)
df$Date = as.Date(df$Date + 3600) #This is weird - the date from excel is read as previous day 23:00, to fix that we add 3 600 sec
df$Weekday = strftime(df$Date,'%w')
df$Year = strftime(df$Date,'%Y')
#Year 2007 is incomplete
df=df[df$Year>2007,]

#Remove future zeros
non_zeros = df[df$Cust...TOTAL!=0,]
last_date = tail(non_zeros, n = 1)$Date
df=df[df$Date<=last_date,]

#adding week number starting the first day of the year
df_by_year=split(df,df$Year)
for (year in names(df_by_year)) {
  x = seq(nrow(df_by_year[[year]]))
  df_by_year[[year]]$Week = sprintf("%02d",ceiling(x/7))
}
df_new = do.call("rbind",df_by_year)

#modifying columns
df_new$Year = as.numeric(df_new$Year)
df_new$weekYear = paste(df_new$Year,df_new$Week,sep="-")

#assigning NA where daily sales are 0

#df_new$Cust...TOTAL[df_new$Cust...TOTAL==0] = NA #if you like to use NA instead of 0

#summing days to weekly data
cust = data.frame(date = df_new$Date, year = df_new$Year, week = df_new$Week, day = df_new$Weekday, weekyear = df_new$weekYear, count = df_new$Cust...TOTAL)
cust_agg1 = aggregate(formula = count ~ weekyear, data = cust, FUN="sum", na.action=na.omit) #na.pass if you like to NA the whole week
cust_agg2 = aggregate(formula = cbind(week,year, date) ~ weekyear, data = cust,FUN="max")
cust_weekly = merge(cust_agg1,cust_agg2, by="weekyear")
cust_weekly$date=as.Date(cust_weekly$date)


#removing na values
#cust_weekly = cust_weekly[!is.na(cust_weekly$count),]