df = readWorksheetFromFile(file = "Shop Figures v8.xlsx", sheet = "Takings", endCol = 13)
df$Date = as.Date(df$Date) + 1 #This is weird - the date from excel is read as previous day 23:00, to fix that we add 3 600 sec
df$Weekday = strftime(df$Date,'%w')
df$Year = strftime(df$Date,'%Y')
#Year 2007 is incomplete
df=df[df$Year>2007,]

#Remove future zeros
non_zeros = df[df$Cust...TOTAL!=0,]
last_date = tail(non_zeros, n = 1)$Date - 3
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

#making clean data frame, removing Sundays as they contain totals for a week
cust = data.frame(date = df_new$Date, 
                  year = df_new$Year, 
                  week = df_new$Week, 
                  day = df_new$Weekday, 
                  weekyear = df_new$weekYear, 
                  count = df_new$Cust...TOTAL,
                  revenue = df_new$Cash + as.numeric(gsub("[$]","",df_new$EFTPOS))
                  )
cust$spend=cust$revenue / cust$count
cust = cust[cust$day!=0,]

#adding UK week, e.g. first monday of the year is the first week
cust$uk_week = strftime(cust$date, "%W")
cust$uk_weekyear = paste(cust$year,cust$uk_week,sep="-")

#filling na's with means

mean_count = mean(cust$count, na.rm=TRUE)
mean_revenue = mean(cust$revenue, na.rm=TRUE)
cust$count[is.na(cust$count)]=mean_count
cust$revenue[is.na(cust$revenue)]=mean_revenue
cust$spend = cust$revenue / cust$count
cust$spend[is.na(cust$spend)]=mean_revenue / mean_count
cust$spend[is.infinite(cust$spend)]=mean_revenue / mean_count

#summing days to weekly data
cust_agg1 = aggregate(formula = cbind(count, revenue) ~ weekyear, data = cust, FUN="sum", na.action=na.omit) #na.pass if you like to NA the whole week
cust_agg2 = aggregate(formula = cbind(week,year, date) ~ weekyear, data = cust,FUN="max")
cust_weekly = merge(cust_agg1,cust_agg2, by="weekyear")
cust_weekly$spend = cust_weekly$revenue / cust_weekly$count
cust_weekly$date=as.Date(cust_weekly$date)
cust_weekly=cust_weekly[cust_weekly$week!=53,]

#filling zeros and na's with means
#empirically this is better than removing and keeping these values
mean_count = mean(cust_weekly$count)
mean_revenue = mean(cust_weekly$revenue)
cust_weekly$count[cust_weekly$count==0]=mean_count
cust_weekly$count[is.na(cust_weekly$count)]=mean_count
cust_weekly$revenue[cust_weekly$revenue==0]=mean_revenue
cust_weekly$revenue[is.na(cust_weekly$revenue)]=mean_revenue
cust_weekly$spend = cust_weekly$revenue / cust_weekly$count



#cleaning up
rm(df_by_year, non_zeros, cust_agg1, cust_agg2, mean_count, mean_revenue)