options(java.parameters = "-Xmx1024m")
df = readWorksheetFromFile(file = "Shop Figures v8.xlsx", sheet = "Takings", endCol = 13)
df$Weekday = strftime(df$Date,'%w')
df$Week = strftime(df$Date,'%W')
df$Year = strftime(df$Date,'%Y')
df_weekly = subset(df, Weekday == 0)
cust_weekly = data.frame(date = df_weekly$Date, year = df_weekly$Year, week = df_weekly$Week,  count = df_weekly$Cust...TOTAL)

#removing future days
non_zeros = subset(cust_weekly, cust_weekly$count!=0)
last = as.numeric(row.names(tail(non_zeros, n = 1))) - 1 #last week is not full, e.g. only mon and tue stats available
cust_weekly =cust_weekly[1:last,]

#filling zeros with averages
cust_weekly$count[cust_weekly$count==0]=mean(cust_weekly$count)

#removing first and last week - to avoid 53 - 1 week inconsistencies

cust_weekly=cust_weekly[as.numeric(cust_weekly$week)!=1,]
cust_weekly=cust_weekly[as.numeric(cust_weekly$week)!=52,]
cust_weekly=cust_weekly[as.numeric(cust_weekly$week)!=53,]
