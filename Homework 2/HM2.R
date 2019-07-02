getwd()
setwd("C:/Users/Sinan/Desktop/Assignment 2")
salesdata<-read.table("salesdata.txt",header = TRUE)
salesperson<-read.table("salesperson.txt",header = TRUE)

library(corrplot)
library(car)
library(gvlma)

#Question 1
x<-scan()
186 119 180 137 152 171 169 131 137 105 191 195 118 117 150 186 183
176 160 138 109 108 160 150 127 193 182 114 118 129 170 175 115 166
136 144 145 185 146 132 117 148 136 154 157 183 189 133 180 141

y<-scan()
2397 156 1572 339 600 1364 1317 265 288 121 2101 2038 214 132 601 1729
1986 1738 757 280 106 112 637 559 195 2227 1689 93 144 273 849 1364 159
953 322 357 391 2273 516 229 162 571 338 594 614 2102 2635 278 1960 374

plot(x,y)
reg<-lm(y~x)
gvlma(reg)
summary(reg)
abline(reg,col="red")


plot(x,log(y))

logreg<-lm(log(y)~x)
summary(logreg)
par(mfrow=c(2,2))
plot(reg)
gvlma(logreg)
plot(logreg)

abline(logreg,col="blue")

coefficients(logreg)
confint(logreg,level = 0.95)
confint(predict(reg,data.frame(x=150)),level = 0.95)
predict(reg,data.frame(x=125),interval = "confidence")
predict(reg,data.frame(x=250))

exp(predict(logreg,data.frame(x=125),interval = "prediction"))
exp(predict(logreg,data.frame(x=250),interval = "prediction"))

exp(predict(logreg,data.frame(x=150),interval="confidence"))
exp(predict(logreg,data.frame(x=150),interval="prediction"))


#Question 2

cor(salesperson)

pairs(salesperson)
corrplot(cor(salesperson))

corrplot(cor(salesperson),order = "AOE",method = "color",addCoef.col = "gray" )

reg1<-lm(SALES~AGE,data =salesperson)
summary(reg1)





summary.aov(lm(SALES~AGE+APT,data =salesperson))
summary.aov(lm(SALES~AGE+ANX,data =salesperson))
summary.aov(lm(SALES~AGE+EXP,data =salesperson))
summary.aov(lm(SALES~AGE+GPA,data =salesperson))

currentmodel<-lm(SALES~AGE+APT,data =salesperson)
reducedmodel<-lm(SALES~APT,data =salesperson)
summary.aov(currentmodel)
summary.aov(reducedmodel)

summary.aov(lm(SALES~AGE+APT+ANX,data =salesperson))
summary.aov(lm(SALES~AGE+APT+GPA,data =salesperson))
summary.aov(lm(SALES~AGE+APT+EXP,data =salesperson))

#by using step function

Fitall<-lm(SALES~.,data = salesperson)
Fitstart<-lm(SALES~AGE,data = salesperson)
step(Fitstart,direction = "forward",scope = formula(Fitall))



reglast<-lm(SALES~AGE+APT,data =salesperson)
summary(reglast)

summary(lm(SALES~GPA,data = salesperson))




#Question 3


salesdata
sales<-lm(PROFIT~SALES,data=salesdata)
plot(salesdata$SALES,salesdata$PROFIT)
abline(sales,col="red")
summary(sales)

len<-length(salesdata[,1])
len
newsales<-data.frame(PROFIT=salesdata$PROFIT,SALES=salesdata$SALES,TIME=1:len)
newsales[1:4,]
regsales<-lm(PROFIT~SALES+TIME,data=newsales)
summary(regsales)
Sales_coeff<-9.6060923+0.2936245*salesdata$SALES+0.1099769+newsales$TIME

plot(Sales_coeff,newsales$PROFIT)
par(mfrow=c(2,2))
plot(regsales)
summary(regsales)
gvlma(regsales)
predict(regsales,data.frame(SALES=30,TIME=101),interval = "prediction")


salests<-ts(salesdata,freq=4,start=1988)
plot(salests)
salests

len<-length(salesdata[,1])
len

x1<-1:len%%4 ==1
x2<-1:len%%4 ==2
x3<-1:len%%4 ==3
x4<-1:len%%4 ==0
powerdata<-data.frame(PROFIT=salesdata$PROFIT,SALES=salesdata$SALES,TIME=1:len,S1=x1*1,S2=x2*1,S3=x3*1,S4=x4*1)
powerdata[1:4,]

fit<-lm(PROFIT~SALES+TIME+S1+S2+S3+S4,data=powerdata)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
gvlma(fit)

predict(fit,data.frame(SALES=30,TIME=101,S1=1,S2=0,S3=0,S4=0),interval = "prediction")

