library(itsmr)
#install.packages('fGarch')
library(fGarch)
library(forecast)
#Q1
ibm<-read.table("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW6\\IBM2012.txt", header=FALSE)$V1
View(ibm)
plot(ibm, type = 'l')
#data doesn't seem to have any trend or seasonality
#acf(ibm, lag.max=NULL, type="correlation",plot=TRUE, main='Sample ACF', ylim=NULL, na.action=na.contiguous)
plota(ibm, h = 40)
acf(ibm)
acf(ibm^2, main="IBM squared")
pacf(ibm^2, main = 'IBM squared')
#M = c("diff",1)
#e = Resid(ibm,M)

#Perform Box-Ljung test
Box.test(ibm^2,lag=6,type="Ljung")
Box.test(ibm^2,lag=12,type="Ljung")

auto.arima(ibm,c(5,1,5)) 
a = Arima(ibm, order=c(1,0,1))
res_ibm= a$residuals
Box.test(res_ibm^2,lag=12,type="Ljung")

m3=garchFit(~garch(3,0),data=ibm, trace=F)
summary(m3)
#AICc(m3)
m1=garchFit(~garch(1,0),data=ibm,trace=F)
summary(m1)
plot(m3)
plot(m1)
#QQ plot suggests that the sample quantile > theo quantile --> heavier tails --> try t-dist

m3e=garchFit(~garch(3,0),data=ibm, cond.dist ='std', trace=F)
summary(m3e)
#AICc(m3)
m1e=garchFit(~garch(1,0),data=ibm,cond.dist ='std', trace=F)
summary(m1e)
plot(m3e)
plot(m1e)
predict(m3e,n.ahead=1)
predict(m3e,n.ahead=5)

#Q3
dj<-read.table("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW6\\djao2-v1.txt", header=FALSE)$V1
plot(dj, type = 'l')
#data does seem to have some trend
M = c("diff",1)
e = Resid(dj,M)

plota(e, h = 40)
acf(e)
#(ibm^2, main="IBM squared")
#pacf(ibm^2, main = 'IBM squared')

Box.test(e^2,lag=12,type="Ljung")

m41=garchFit(~garch(4,0),data=e, trace=F)
res<-residuals(m41, standardize = T)

Box.test(res^2,lag=12,type="Ljung")
summary(m41)

predict(m41,n.ahead=4)

#Q4
m51=garchFit(~garch(1,2),data=ibm, trace=F)
res_ibm<-residuals(m51, standardize = T)

Box.test(res_ibm^2,lag=12,type="Ljung")
summary(m51)
predict(m51,n.ahead=4)