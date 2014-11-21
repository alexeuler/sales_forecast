source("init.R", chdir=T)
source("import.R", chdir=T)
source("graphs.R", chdir=T)
#general_graphs(cust_weekly)

#ggplot(cust_weekly, aes(x = factor(week), y=count, group=year, colour=year)) + geom_line()  + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggplot(cust_weekly, aes(x = factor(week), y=count)) + geom_boxplot() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))      
#ggplot(cust_weekly, aes(x=date, y=count)) + geom_line() + theme_bw() + stat_smooth()      
