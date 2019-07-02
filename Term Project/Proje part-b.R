getwd()
setwd("C:/Users/Sinan/Desktop/dersler/ie 360/Term Project")
data<-read.csv("EP.csv",header = TRUE)
fix(data)
library(ggplot2)
library(basicTrendline)
library(fpp2)
library(forecast)
datats<-ts(data$SALES[1:156],start = 1988,frequency = 12)

plot(datats)

acf(datats,lag.max = 40)

#METHOD A: FORECASTING WITH REGRESSION




#METHOD B: FORECASTING WITH TIME SERIES ANALYSIS

plot(log(data$SALES),type = "l")

transformed_data<-data.frame(log(data$SALES[1:156]))
newsales<-transformed_data$log.data.SALES.
acf(newsales,lag.max = 50)
pacf(newsales,lag.max = 50)

differenced_log_data<-diff(newsales,differences = 1)

acf(differenced_log_data,lag.max = 50)
pacf(differenced_log_data,lag.max = 50)


seadiff <- diff(newsales, lag = 12, differences = 1)
acf(seadiff,lag.max = 50)
pacf(seadiff,lag.max = 50)


model_time_serie<-ts(transformed_data$log.data.SALES.1.156..,start = 1988,frequency = 12)

Arima_initial<-Arima(model_time_serie,order=c(2,1,2),seasonal=c(1,1,1))
Arima1<-Arima(model_time_serie,order=c(1,1,2),seasonal=c(1,1,1))
Arima2<-Arima(model_time_serie,order=c(1,1,1),seasonal=c(1,1,1))
Arima3<-Arima(model_time_serie,order=c(1,1,3),seasonal=c(1,1,1))
Arima4<-Arima(model_time_serie,order=c(1,1,2),seasonal=c(0,1,1))
Arima5<-Arima(model_time_serie,order=c(1,1,2),seasonal=c(1,1,0))
Arima6<-Arima(model_time_serie,order=c(1,1,2),seasonal=c(1,1,2))
Arima7<-Arima(model_time_serie,order=c(1,1,2),seasonal=c(2,1,1))
Arima8<-Arima(model_time_serie,order=c(1,1,2),seasonal=c(1,1,2))
Arima9<-Arima(model_time_serie,order=c(2,1,1),seasonal=c(1,1,1))
Arima10<-Arima(model_time_serie,order=c(2,1,1),seasonal=c(1,1,0))
Arima11<-Arima(model_time_serie,order=c(2,1,1),seasonal=c(0,1,1))
Arima12<-Arima(model_time_serie,order=c(3,1,1),seasonal=c(1,1,1))
Arima13<-Arima(model_time_serie,order=c(2,1,1),seasonal=c(1,1,2))
Arima14<-Arima(model_time_serie,order=c(2,1,1),seasonal=c(2,1,1))
Arima15<-Arima(model_time_serie,order=c(2,1,1),seasonal=c(2,1,2))
Arima15<-Arima(model_time_serie,order=c(3,1,2),seasonal=c(1,1,1))
Arima16<-Arima(model_time_serie,order=c(3,1,1),seasonal=c(1,1,1))
Arima17<-Arima(model_time_serie,order=c(3,1,3),seasonal=c(1,1,1))
Arima18<-Arima(model_time_serie,order=c(3,1,2),seasonal=c(0,1,1))
Arima19<-Arima(model_time_serie,order=c(3,1,2),seasonal=c(1,1,0))
Arima20<-Arima(model_time_serie,order=c(3,1,2),seasonal=c(0,1,0))
Arima21<-Arima(model_time_serie,order=c(3,1,2),seasonal=c(2,1,2))
Arima22<-Arima(model_time_serie,order=c(3,1,2),seasonal=c(2,1,1))
Arima23<-Arima(model_time_serie,order=c(3,1,2),seasonal=c(1,1,2))
Arima24<-Arima(model_time_serie,order=c(1,1,3),seasonal=c(1,1,1))
Arima25<-Arima(model_time_serie,order=c(2,1,3),seasonal=c(1,1,1))
Arima26<-Arima(model_time_serie,order=c(3,1,3),seasonal=c(1,1,1))
Arima27<-Arima(model_time_serie,order=c(2,1,3),seasonal=c(0,1,1))
Arima28<-Arima(model_time_serie,order=c(2,1,3),seasonal=c(1,1,0))
Arima29<-Arima(model_time_serie,order=c(2,1,3),seasonal=c(2,1,1))
Arima30<-Arima(model_time_serie,order=c(2,1,3),seasonal=c(1,1,2))
Arima31<-Arima(model_time_serie,order=c(2,1,2),seasonal=c(0,1,2))
Arima32<-Arima(model_time_serie,order=c(2,1,2),seasonal=c(2,1,0))
Arima33<-Arima(model_time_serie,order=c(0,1,2),seasonal=c(1,1,0))
Arima_initial$aic
Arima1$aic
Arima2$aic
Arima3$aic
Arima4$aic
Arima5$aic
Arima6$aic
Arima7$aic
Arima8$aic
Arima9$aic
Arima10$aic
Arima11$aic
Arima12$aic
Arima13$aic
Arima14$aic
Arima15$aic
Arima16$aic
Arima17$aic
Arima18$aic
Arima19$aic
Arima20$aic
Arima21$aic
Arima22$aic
Arima23$aic
Arima24$aic
Arima25$aic
Arima26$aic
Arima27$aic
Arima28$aic
Arima29$aic
Arima30$aic
Arima31$aic
Arima32$aic


Arima5
Arima1
Arima10
Arima29
Arima7
Arima4
Arima12
library(stats)
require(stats)
par(mar=c(1,1,1,1))
tsdisplay(Arima5$residuals)
tsdisplay(Arima1$residuals)
tsdisplay(Arima10$residuals)
tsdisplay(Arima29$residuals)
tsdisplay(Arima7$residuals)
tsdisplay(Arima4$residuals)


qqnorm(Arima5$residuals)
qqline(Arima5$residuals)
checkresiduals(Arima5$residuals)


forecast_arima<-forecast(Arima5,h=12*1)
autoplot((forecast_arima))

print(exp(summary(forecast_arima)))
checkresiduals(forecast_arima)
k<-as.numeric(forecast_arima$mean)
exp(k)
forecasted<-0
forecasted<-exp(k)
plot(forecasted)
initial_model<-Arima(model_time_serie,order = c(0,1,0),seasonal = c(0,1,0),include.constant=TRUE)

acf(initial_model,lag =15)

print(summary(initial_model))











  
  
  
