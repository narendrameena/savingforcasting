
#author narendra
#date 02/25/16
#description time series pridication of balance 



#install.packages("RWeka")
#install.packages("xts")
#install.packages("forecast")
#install.packages("xlsx")
#install.packages("date")
#install.packages("readxl")
library(xts)
library(RWeka)
#WPM("refresh-cache") # refresing chace 
#WPM("list-packages", "installed") # list of installed packages 
#WPM("list-packages", "available") # list of available packages  
#WPM("install-package", "timeseriesForecasting") # installing time series forcasting package 


library(xlsx)
library(RWeka)
library(readxl)
setwd("/Users/naru/Documents/R_workshop/personalFinanceSaving/data") # setting path to data directory
data <- read.xlsx2("Sample data set 5 Final.xlsx", sheetName = 2,header=TRUE) # reading xlsx file sheet 2 as the fromat defined 

#data <- read.table("Sample data set 4 Final.xlsx",2)
#data <- read_excel("Sample data set 6 Final.xlsx", 2) # reading xlsx file sheet 2 as the fromat defined 


###### applying all three assumptions 
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


#######end

#######getting output into CSV file
write.csv(newdata, file="balance.csv")
newdata

######end

####### ploting data to see patterns 
plot(newdata$date, newdata$x, type = "l")

#######end


########Normalized Data BAlance columun
normdata<-newdata
x <-as.numeric(newdata$x)
normalized = (x-min(x))/(max(x)-min(x))

normdata$x <- normalized

########end


#######xts dataframe (data in time series dataframe )

xtsdata <- xts(as.numeric(newdata$x), newdata$date)
newdata

#normlized data
normxtsdata <- xts(as.numeric(normdata$x), normdata$date)
normxtsdata


#######end

########pridiction using Neural network autoregression
par(mfrow=c(1,2))


xtsdata
fit <- nnetar(xtsdata)
f <- forecast(fit,h=30)
plot(f)
summary(f)

#normlized normxtsdata
fit <- nnetar(normxtsdata)
f <- forecast(fit,h=30)
plot(f)
summary(f)

#####end


#######foriour transformation of data into frequecy 
library(ggplot2)
f <- data.frame(coef = fft(normdata$x), freqindex = c(1:91))   # norm data
#f <- data.frame(coef = fft(as.numeric(newdata$x)), freqindex = c(1:91))   # original data
plot(f)
qplot(freqindex, Mod(coef), data = f[1:91,], geom = "line")


f[Mod(f$coef) > 3 & f$freqindex < 53, "freqindex"] - 1


peaks <- Mod(f$coef) > 3
ffilt <- f
ffilt[!peaks, "coef"] <- 0
ffilt <- data.frame(index=ffilt$freqindex, value=Re(fft(ffilt$coef, inverse=TRUE))/91, type=rep("filtered", times=91))
ffilt <- rbind(ffilt, data.frame(index=seq(1:91), value=newdata$x, type=rep("original", times=91)))


##########
midindex <- ceiling((length(f$coef)-1)/ 2) + 1
peakind <- f[abs(f$coef) > 3 & f$freqindex > 1 & f$freqindex < midindex,]

lindex <- length(f$coef)

lowerind <- 1

subsignals <- lapply(c(peakind$freqindex, midindex+1), function(x){
  upperind <- x
  fsub <- f
  notnullind <- ((fsub$freqindex >= lowerind
                  & fsub$freqindex < upperind)
                 |
                   (fsub$freqindex >  (lindex - upperind + 2)
                    & fsub$freqindex <= (lindex - lowerind + 2)))
  fsub[!notnullind,"coef"] <- 0
  lowerind <<- upperind
  Re(fft(fsub$coef, inverse=TRUE)/length(fsub$coef))
})




########
library(grid)

grid.newpage()
pushViewport(viewport(layout=grid.layout(4,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)

psig <- function(x, y, z){
  h <- data.frame(index = c(1:length(subsignals[[x]])),
                  orders = subsignals[[x]])
  lab <- paste("Subseries ", as.character(x), sep="")
  print(qplot(index, orders, data = h, geom = "line", main=lab), vp = vplayout(y,z))
  TRUE
}

par(mfrow=c(1,6))
psig(1,1,1); 
psig(2,1,2);
psig(3,2,1); 
psig(4,2,2); 
psig(5,3,1); 
psig(6,3,2)


#number of hidden neurons

#nn.sizes <- c(4,2,3,3,3,2,2,2)
nn.sizes <- c(5,3,4,4,4,3,3,3)
######
numofsubs <- length(subsignals)
twindow <- 4

offsettedsubdfs <- lapply(1:numofsubs, function(x){
  singleoffsets <- lapply(0:(twindow-1), function(y){
    subsignals[[x]][(twindow-y):(length(subsignals[[x]])-y-1)]
  })
  a <- Reduce(cbind, singleoffsets)
  names <- lapply(1:twindow, function(y){paste("TS", as.character(x), "_", as.character(y), sep = "")})
  b <- as.data.frame(a)
  colnames(b) <- names
  b
})

####
sample.number <- length(offsettedsubdfs[[1]][,1])

######
#install.packages("nnet")
library(nnet)
#the neural networks

nns <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  nn <- nnet(offsettedsubdfs[[i]][1:(sample.number),], #the training samples
             
             subsignals[[i]][(twindow+1):(length(subsignals[[i]]))], #the output
             
             #corresponding to the training samples
             
             size=nn.sizes[i], #number of neurons
             
             maxit = 1000, #number of maximum iteration
             
             linout = TRUE) #the neuron in the output layer should be linear
  
  #the result of the trained networks should be plotted
  
  plot(subsignals[[i]][(twindow+1):(length(subsignals[[i]]))], type="l")
  
  lines(nn$fitted.values,type="l",col="red")
  
  nn
  
})


#pridiction 

number.of.predict <- 30



#long term prediction

long.predictions <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  prediction <- vector(length=number.of.predict, mode="numeric")
  
  #initial input
  
  input <- offsettedsubdfs[[i]][sample.number,]
  
  for (j in 1 : number.of.predict)
    
  {
    
    prediction[j] <- predict(nns[[i]], input)
    
    input <- c(prediction[j],input[1:(length(input)-1)])
    
  }
  
  #we want to plot the prediction
  
  plot(c(nns[[i]]$fitted.values,prediction), type="l",col="red")
  
  lines(subsignals[[i]][(twindow+1):length(subsignals[[i]])])
  
  prediction

})

long.predictions


normPridiction<- rowSums(data.frame(long.predictions))


finalpridiction <-  (normPridiction *(max(x)-min(x)) ) +min(x)


#########end 




###########average method for forcasting 


#data by months 
library(dplyr) 
library(reshape2)
newdata$month <- factor(format(newdata$date, "%B"),  levels = month.name) # getting moths name from data
newdata$month <- as.character(newdata$month)
 # geting each months
months <- unique(newdata$month)
month1 <- as.numeric(subset(newdata$x,newdata$month == as.character(months[1])))  # first month 
month2 <- as.numeric(subset(newdata$x,newdata$month == as.character(months[2])))  #second month
month3 <- as.numeric(subset(newdata$x,newdata$month == as.character(months[3])))  # third month

#list all months 
monthdata <- list(month1,month2,month3)
                    
#maximum number of rows
max.rows <- max(length(month1), length(month2), length(month3))
# Stack shorter columns with NA at the end
monthdata <- sapply( monthdata , function(x) c( x , rep( NA , max.rows - length(x) ) ) ) 

#setting a column name at run time 
monthdata<- setNames( do.call( data.frame , list(monthdata) ) , paste0("month" , 1:3 ) )


monthdata$average <- rowMeans(monthdata)


############end 

####### average of diffrence of change


monthdata$change1 <- monthdata$month2 - monthdata$month1 
monthdata$change2 <- monthdata$month3 - monthdata$month2 


monthdata$changeAverage <- (monthdata$change1+monthdata$change2)/2


##########end 


########pridiction using average method 

monthdata$pridiction <- monthdata$changeAverage + monthdata$average

plot(monthdata)
#######end  


######final results 
finalaveragedata<- monthdata[complete.cases(monthdata),]

balancePridiction <- data.frame(averageMethod=finalaveragedata$pridiction,mlpPridiction = finalpridiction )


balancePridiction$average <- rowMeans(balancePridiction)

