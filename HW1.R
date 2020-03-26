#load("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW1\\d-axp3dx-0111.txt")
data <- read.table("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Time Series\\HW1\\d-axp3dx-0111.txt", header = TRUE, sep = ",")

View(data)
attach(data)

library(fBasics)
basicStats(data$axp)
data1 <- log(data+1)
basicStats(data1)

qt(c(.025, .975), df=2534)
qt(c(.025, .975), df=860)