library(itsmr)
install.packages('ltsa')
library(ltsa)
data<-read.table('C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW 4\\sunspots.txt',col.names = 'sunspots')
attach(data)
yw(sunspots,2)

r<-(acf(sunspots, lag.max=3,plot=FALSE)$acf)[-1]
DLAcfToAR(r,0,0)