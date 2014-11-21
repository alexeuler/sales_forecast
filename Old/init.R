if (!is.element("ggplot2", installed.packages()[,1]))
  install.packages("ggplot2", dep = TRUE)
library("ggplot2")
if (!is.element("forecast", installed.packages()[,1]))
  install.packages("forecast", dep = TRUE)
library("forecast")

source("multiplot.R", chdir=T)
source("plot_cust.R", chdir=T)
source("stats_graphs.R", chdir=T)

df = read.table("Takings-UTF8.csv", header = TRUE, sep=";", comment.char="", quote="")
weekly = subset(df, df$Day.of.the.week=="Sun")
daily = subset(df, df$Day.of.the.week!="Sun")
weekly_cust = subset(weekly, select = c('Cust...TOTAL','X..of.the.week','Year'))
weekly_cust_filtered = subset(weekly_cust, weekly_cust$Cust...TOTAL>0)
weekly_cust_filtered$weekYear = as.factor(paste(weekly_cust_filtered$Year,weekly_cust_filtered$X..of.the.week , sep="_"))


