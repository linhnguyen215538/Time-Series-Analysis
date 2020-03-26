library(forecast)
library(itsmr)
library(ltsa)
library(fGarch)

intel = read.table("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW7\\m-INTC7308.txt", header = TRUE)
log_intel = log(1+intel$rtn)
plot(log_intel)

intel_arima = auto.arima(log_intel)

resid = residuals(intel_arima)
test(resid)

# Box-ljung test to check resid^2
Box.test(resid^2, lag = 10, type = "Ljung")

mod = garchFit(~garch(1,1), resid, trace = F)
summary(mod)

mod_resid = residuals(mod, standardize = TRUE)
Box.test(mod_resid^2, lag = 10, type = "Ljung")

#b
x = fitted(mod)
plot(x[1:(length(x)-1)], x[2:length(x)])

spec = garchSpec(model = list(mu = -0.00093610, omega = 0.00093329, alpha1 = 0.07520361, beta1 = 0.86316001))

# c)
y = garchSim(spec, n = 10000)
plot(y[1:(length(y)-1)], y[2:length(y)])