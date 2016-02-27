
#author narendra
#date 02/25/16
#description time series pridication of balance 



#install.packages("RWeka")
install.packages("xts")
install.packages("forecast")
install.packages("xlsx")
install.packages("date")
install.packages("readxl")
library(xts)
library(RWeka)
WPM("refresh-cache") # refresing chace 
WPM("list-packages", "installed") # list of installed packages 
WPM("list-packages", "available") # list of available packages  
WPM("install-package", "timeseriesForecasting") # installing time series forcasting package 


library(xlsx)
library(RWeka)
library(readxl)
setwd("/Users/naru/Documents/R_workshop/personalFinanceSaving/data") # setting path to data directory
data <- read.xlsx2("Sample data set 5 Final.xlsx", sheetName = 2,header=TRUE) # reading xlsx file sheet 2 as the fromat defined 

#data <- read.table("Sample data set 4 Final.xlsx",2)
#data <- read_excel("Sample data set 6 Final.xlsx", 2) # reading xlsx file sheet 2 as the fromat defined 
newdata = data[!apply(is.na(data) | data == "", 1, all), ]  # removing empty rows from excell sheet 
newdata

newdata = aggregate(newdata$Balance,by=list(date=newdata$Post.Date), FUN = tail, n = 1)  # getting balance of last day of each day 
newdata

newdata$date <- as.Date(as.numeric(as.character(newdata$date)), origin="1899-12-30") # converting into date 
class(newdata$date)
newdata


alldates = seq(min(newdata$date), max(newdata$date), 1) # seq of dates from start to end with one day gape



dates0 = alldates[!(alldates %in% newdata$date)]   # filter out timestamps that are already present
data0 = data.frame(date = dates0, x = NA_real_)  # construct a `data.frame` to append with missing values:


newdata = rbind(newdata, data0) # append this `data.frame` and resort in time:
newdata = newdata[order(newdata$date),]

newdata$x <-as.character(newdata$x)


# fill the values 
current = NA_real_
newdata$x = sapply(newdata$x, function(x) { 
  current <<- ifelse(is.na(x), current, x); current })


write.csv(newdata, file="balance.csv")
newdata


plot(newdata$date, newdata$x, type = "l")


source("/Users/naru/Documents/R_workshop/personalFinanceSaving/forecast.R")  ## see code file in section 5
result.arima <- forecastArima(newdata, n.ahead = 30)

#xts dataframe

xtsdata <- xts(as.numeric(newdata$x), newdata$date)
xtsdata
 
xtsdata["01"]
mAvg <- apply.monthly(xtsdata, function(x) apply(x, 2, mean))  # daily mean
dAvg <- apply.daily(xtsdata, function(x) apply(x, 2, mean))  # daily mean

to.period(xtsdata,'months')
to.monthly(xtsdata)
str(attributes(xtsdata))

xtsdata[xts:::startof(xtsdata, "months")]
apply.monthly(xtsdata,head,1)

st <- as.Date(newdata$date[1])
en <- as.Date(newdata$date[length(newdata$date)])
ll <- seq(newdata$date[length(newdata$date)], newdata$date[1], by = "-1 months")
ll
rev(ll[ll >= st & ll <= en])



## find all 7th of the month between two dates, the last being a 7th.
st <- as.Date("1998-12-17")
en <- as.Date("2000-1-7")
ll <- seq(en, st, by = "-1 month")
rev(ll[ll > st & ll < en])
