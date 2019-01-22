setwd("~/documents/assignments/database/project/")

#importing libraries
library(plyr)
library(RMySQL)

#reading in datasets
d1 <- read.csv("datasets/BUAN6320-DataSet1.txt",header = T,stringsAsFactors = F,sep = "\t")
d2 <- read.csv("datasets/BUAN6320-DataSet2.txt",header = T,stringsAsFactors = F,sep = "\t")
d3 <- read.csv("datasets/BUAN6320-DataSet3_pytmp.txt",header = T,stringsAsFactors = F,sep = "\t",fileEncoding="utf-8")
d4 <- read.csv("datasets/BUAN6320-DataSet4.txt",header = T,stringsAsFactors = F,sep = "\t")

#displaying column names for easier subsetting
colnames(d1)
colnames(d2)
colnames(d3)
colnames(d4)

#subsetting columns to form individual tables and manipulating rows to clean data
brand <- d3[c(12,2,9)]
brand <- unique(brand)
customer <- d1[c(2,3,5,6,8,10,12,14)]
customer <- unique(customer)
department <- d4[c(12,13,14,15,16)]
department <- unique(department)
employee <- d4[c(1,2,3,4,5,6,7,8,12)]
employee <- unique(employee)
invoice <- d1[c(13,16,2,9,15)]
invoice <- unique(invoice)
line1 <- d1[c(13,11,7,4,1)]
line2 <- d2[c(12,13,14,15,16)]
line <- rbind(line1,line2)
line <- unique(line)
product <- d3[c(1,4,5,17,7,11,14,16,12)]
product <- unique(product)
salary_history <- d4[c(1,9,10,11)]
salary_history <- unique(salary_history)
supplies <- d3[c(1,13)]
supplies <- unique(supplies)
vendor <- d3[c(13,6,3,10,8,15)]
vendor <- unique(vendor)

#removing duplicated invoice numbers
invoice <- invoice[!duplicated(invoice$INV_NUM),]
#removing empty vendor names
vendor <- vendor[which(vendor$VEND_NAME != ""),]
#changing employee_id to emp_num in invoices table
colnames(invoice)[5] <- "EMP_NUM"
#reformatting dates into the proper YYYY-MM-DD format
employee$EMP_HIREDATE <- as.Date(employee$EMP_HIREDATE, origin = "1900-01-01")
invoice$INV_DATE <- as.Date(invoice$INV_DATE, origin = "1900-01-01")
salary_history$SAL_FROM <- as.Date(salary_history$SAL_FROM, origin = "1900-01-01")
#setting empty sal_end dates to null in salary_history table
salary_history$SAL_END[which(salary_history$SAL_END == " - ")] <- NA
salary_history$SAL_END <- as.integer(salary_history$SAL_END)
salary_history$SAL_END <- as.Date(salary_history$SAL_END, origin = "1900-01-01")

#pushing data to mysql
con <- dbConnect(MySQL(), user='root', password='n', dbname='project', host='localhost')
# 
# # dbWriteTable(con, name='brand', brand, overwrite=T, row.names = F)
# # dbWriteTable(con, name='customer', customer, overwrite=T, row.names = F)
# # dbWriteTable(con, name='department', department, overwrite=T, row.names = F)
# # dbWriteTable(con, name='employee', employee, overwrite=T, row.names = F)
# # dbWriteTable(con, name='invoice', invoice, overwrite=T, row.names = F)
# # dbWriteTable(con, name='line', line, overwrite=T, row.names = F)
# # dbWriteTable(con, name='product', product, overwrite=T, row.names = F)
# # dbWriteTable(con, name='salary_history', salary_history, overwrite=T, row.names = F)
# # dbWriteTable(con, name='supplies', supplies, overwrite=T, row.names = F)
# # dbWriteTable(con, name='vendor', vendor, overwrite=T, row.names = F)
# dbWriteTable(con, name='brand', brand, append=T, row.names = F)
# dbWriteTable(con, name='customer', customer, append=T, row.names = F)
# dbWriteTable(con, name='department', department, append=T, row.names = F)
# dbWriteTable(con, name='employee', employee, append=T, row.names = F)
# dbWriteTable(con, name='invoice', invoice, append=T, row.names = F)
# dbWriteTable(con, name='line', line, append=T, row.names = F)
# dbWriteTable(con, name='product', product, append=T, row.names = F)
# dbWriteTable(con, name='salary_history', salary_history, append=T, row.names = F)
# dbWriteTable(con, name='supplies', supplies, append=T, row.names = F)
# dbWriteTable(con, name='vendor', vendor, append=T, row.names = F)



#time series analysis
library(ggplot2)
library(tseries)

#subsetting invoice table into an analytical dataset
ts <- invoice[c(2,4)]
ts <- ts[order(ts$INV_DATE),]
colnames(ts) <- c("inv_date","inv_total")
#aggregating the invoice totals on a day level
ts_ <- aggregate(data=ts,FUN=sum,inv_total~inv_date)
#talking a cumulative sum of invoice totals on a day level to generate the running revenue field
ts_$revenue <- cumsum(ts_$inv_total)

#testing revenue data to check where it is stationary
kpss.test(ts_$revenue,null="Level")
kpss.test(ts_$revenue,null="Trend")
kpss.test(diff(ts_$revenue),null="Level")
kpss.test(diff(ts_$revenue),null="Trend")
kpss.test(diff(diff(ts_$revenue)),null="Level")
kpss.test(diff(diff(ts_$revenue)),null="Trend")

#making a time series plot to check revenue data being stationary
plt_rev <- ggplot(ts_, aes(x=inv_date, y=revenue, group=1)) + 
  geom_line() + 
  xlab('Invoice Date') + 
  ylab('Revenue (in $)') + 
  theme_bw() + 
  ggtitle('Revenue Plot') +
  scale_x_date(date_breaks = "2 months") +
  scale_y_continuous(labels = scales::comma, breaks=c(0,50000,100000,150000,200000,250000,300000))
plt_rev

revdiffs <- diff(diff(ts_$revenue))
dates <- ts_$inv_date[3:275]

df1 <- as.data.frame(cbind(dates,revdiffs))
df1$dates <- as.Date(df1$dates,origin = '1970-01-01')

plt_rev_diff <- ggplot(data = df1,aes(x=dates, y=revdiffs, group=1)) + 
  geom_line() + 
  xlab('Invoice Date') + 
  ylab('Revenue Diffs (in $)') + 
  theme_bw() + 
  ggtitle('Revenue Second Differences vs Time') +
  scale_x_date(date_breaks = "2 months") +
  scale_y_continuous(labels = scales::comma, breaks=c(0,50000,100000,150000,200000,250000,300000))
plt_rev_diff

#creating an ARIMA
library(forecast)
auto.arima(ts_$revenue)
model <- arima(ts_$revenue,c(0,2,1))
#setting prediction steps for 365 days
steps <- 365
#predicting the next 365 days of revenue
future <- forecast(model,h=steps)
#plot for predicted revenue
plot(future)
fcast <- as.data.frame(cbind(future$lower,future$upper))
fcast$date <- seq.Date(from = max(ts_$inv_date)+1,to = max(ts_$inv_date)+365,by = "day")
fcast <- fcast[c(5,1,3,2,4)]
fcast$`future$lower.80%` <- round(fcast$`future$lower.80%`,2)
fcast$`future$upper.80%` <- round(fcast$`future$upper.80%`,2)
fcast$`future$lower.95%` <- round(fcast$`future$lower.95%`,2)
fcast$`future$upper.95%` <- round(fcast$`future$upper.95%`,2)
colnames(fcast) <- c("Prediction Date","Lower 80% Prediction","Higher 80% Prediction","Lower 95% Prediction", "Higher 95% Prediction")
dbWriteTable(con, name='forecasts', fcast, append=T, row.names = F)
fcst <- fcast
fcst$`Prediction Date` <- as.character(fcst$`Prediction Date`)
fcst <- fcst[which(substr(fcst$`Prediction Date`,9,10) == '01'),]
fcst$avrev <- (fcst$`Lower 95% Prediction`+fcst$`Higher 95% Prediction`)/2
fcst$diff <- 0
fcst$diff[2:12] <- diff(fcst$avrev)
fcst$pinc <- fcst$diff/fcst$avrev*100
mean(fcst$pinc)
