getwd()
setwd("C:/Users/Sinan/Desktop/dersler/ie 360/Homework 3")
res<-read.csv("UKPlasticPrices.csv",header = TRUE)

x<-ts(res$Price.Index,freq=12,start=1996)
library(forecast)
library(fpp2)

plot(x)
plot(decompose(x))

acf(x,lag.max = 35)
pacf(x,lag.max = 35)

tsdisplay(x)

diffx<-diff(x)
acf(diffx)
pacf(diffx)

Arimax<-Arima(x,order = c(0,1,0),include.drift = TRUE)
Arimax

forecast12<-forecast(Arimax,level = c (95),h=12)
plot(forecast12)
forecast12

plot(res$Price.Index[20:30],type="l")

fix(res)
newres<-data.frame.na()
newres<-res$Month[25:183]
newres<-data.frame(newres)
newres$Price.Index<-res$Price.Index[25:183]
fix(newres)

newx<-ts(newres$Price.Index,start = 1998,freq=12)
newx
tsdisplay(newx)

diffnewx<-diff(newx)
acf(diffnewx,lag.max = 40)
pacf(diffnewx,lag.max = 40)
plot(decompose(diffnewx))

Arimanewx<-Arima(newx,order=c(0,1,0),seasonal = c(0,0,0),include.drift = TRUE)
ArimanewxwithAR<-Arima(newx,order=c(0,1,0),seasonal = c(1,0,0))
ArimanewxwithARdrift<-Arima(newx,order=c(0,1,0),seasonal = c(1,0,0),include.drift = TRUE)
ArimanewxwithMAdrift<-Arima(newx,order=c(0,1,0),seasonal = c(0,0,1),include.drift = TRUE)
ArimanewxwithMA<-Arima(newx,order=c(0,1,0),seasonal = c(0,0,1))
Arimanewx
ArimanewxwithAR
ArimanewxwithARdrift
ArimanewxwithMAdrift
ArimanewxwithMA

best<-auto.arima(newx,seasonal=TRUE,trace = TRUE)

forecastnew12<-forecast(ArimanewxwithMA,level = c (95),h=12)
forecastnew12

plot(forecastnew12)

