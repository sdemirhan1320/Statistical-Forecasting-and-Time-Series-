#Question 1
set.seed(330)
X<-rnorm(75 , 1,sqrt(3))
X

a<-exp(X)
a

a1<-mean(a)
a1

b=t.test(a,conf.level = 0.95)
b

#Question 2
getwd()
setwd("C:/Users/Sinan/Desktop/Assignment 1 360")
cardata <- read.table(file="cars.txt", header=TRUE)
cardata

plot(cardata,xlab="speed",ylab="Distance",main=" The Relationship between Speed and Distance")
cor(cardata$speed,cardata$dist)

#Question 3
getwd()
setwd("C:/Users/Sinan/Desktop")
netdata <- read.table(file="electricity.txt", header=FALSE)
netdata

netdatats<-ts(netdata,frequency = 12,start=c(1956,1))
netdatats

plot(netdatats, main="internet usage")

acf(netdata,main="Autocorrolation Plot")


#Question 4
netdatats_dec<-decompose(netdatats,type = "multiplicative")
plot(netdatats_dec)


desea_data<-netdatats/netdatats_dec$seasonal
plot(desea_data, main="Deseasonalized Data")
acf(desea_data,main="Autocorrolation Plot")

detrend_and_desea_data<-desea_data/netdatats_dec$trend
plot(detrend_and_desea_data,main="Deseasonalized & Detrend Data")
acf(detrend_and_desea_data,na.action=na.pass,main="Autocorrelation Plot")
