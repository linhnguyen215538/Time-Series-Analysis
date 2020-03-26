library(itsmr)
install.packages('aTSA')
library(forecast)
#Q2
data2 <- read.delim("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW5\\HW_5_Attachments_export\\HW 5 Data Sets\\lake.txt", header = FALSE)$V1
plot.ts(data2)
View(data2)
#the plot shows a downward trend --> need to difference the data to remove the trend. 
M2 = c("diff",1)
e2 = Resid(data2,M2)
plot(e2, type='l')

acf(e2)
pacf(e2)
#the plot shows the possible range of p is up to 3.
autofit(e2,p=0:3,q=0:10)
model <- arima(e2, order = c(1, 0, 2))
test(model$residuals)

# fit <- auto.arima(data2,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = FALSE,ic = 'aicc')
# model <- Arima(data2[,1], order = c(2, 1, 1))
# model2 <- Arima(data2[,1], order = c(1, 1, 2))
# model3 <- Arima(data2[,1], order = c(2, 1, 2))
# model4 <- Arima(data2[,1], order = c(1, 1, 1))
# model[["aicc"]]
# model2[["aicc"]]
# model3[["aicc"]]
# model4[["aicc"]]
Box.test(model[["residuals"]])


#Q3:
library(tseries)

mod_ar1 <- ar(data2, order.max = 1)
mod_ar2 <- ar(data2, order.max = 2)
adf.test(data2, k = 0)
adf.test(data2, k = 1)

#Q4
data<-read.table('C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW5\\HW_5_Attachments_export\\HW 5 Data Sets\\airpass.txt',col.names = 'airpass')
attach(data)
airpass<-as.matrix(airpass)
str(airpass)
ashort<-airpass[c(1:(length(airpass)-12))]
#View(airpass)
#View(airshort)
log_ashort<-log(ashort)

plotc(log_ashort)
M2 = c("diff",1)
e2 = Resid(log_ashort,M2)
plot(e2, type='l')

acf(e2)
pacf(e2)
#the plot shows the possible range of p is up to 3.
autofit(e2,p=0:10,q=0:10)
model4 <- arima(e2, order = c(4, 0, 10))
test(model$residuals)

M2 = c("diff",1)
e2 = Resid(log_ashort,M2)
#plot(e2,type='l')
ee2= Resid(e2,M2)
#ee2= ee2-mean(ee2)

