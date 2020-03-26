library(itsmr)
gnpdata <- read.table("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW3\\q-gnp-4711.txt",header=TRUE)
attach(gnpdata)
View(gnpdata)

loggnp = log(gnp)
M = c("diff",1)

e = Resid(loggnp,M)
year = gnpdata$year[1:259]
plot(year,e, type = 'l',xlab='Year',ylab= 'Differenced Log GNP')

a=arma(e,p=3,q=0)
check(a)
#Q2
strikesdata <- read.table("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW3\\Strikes.txt",col.names=c("strikes"))
attach(strikesdata)
View(strikesdata)
M2 = c("diff",1)
e2 = Resid(strikes,M2)
#plot(e2,type='l')
ee2= Resid(e2,M2)
#ee2= ee2-mean(ee2)
plot(ee2,type = 'l',ylab='Differenced Strikes data')
a2=arma(ee2,p=2,q=0)
View(ee2)

check(a2)
#theoretical
#plota(ee2, v = NULL, h = 40)
plota(a2, h = 40)
rho <- ARMAacf(ar=a2$phi, lag.max=40, pacf=F)
View(rho)
#sample ACF
acf(ee2, lag.max=NULL, type="correlation", 
    plot=TRUE, main='Sample ACF', ylim=NULL, na.action=na.contiguous)
#mean(ee2)
#ee2<-ee2-mean(strikes)

View(ee2)