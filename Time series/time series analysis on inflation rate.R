#import libraries
library(tseries)
library(fpp2)
library(forecast)

#read data
inflation_ts <- read.csv("Inflation.csv", header = T)
View(inflation_ts)

#convert data to time series format
inflation_ts <- ts(inflation_ts[,2], start = c(2010, 1), frequency = 12)

#plot the time series dataset to observe it component
plot(inflation_ts, ylab = "inflation rate")
decompose_data <- decompose(inflation_ts, type = "additive")
decompose_data <- decompose(inflation_ts, type = "multiplicative")
plot(decompose_data)

#test of stationarity
adf.test(inflation_ts)

#fit the model
fit <- auto.arima(inflation_ts)
summary(fit)

#forecast the model
pred1 <- predict(fit, n.ahead = 24)
pred1 <- forecast(fit)
plot(pred1)

#test for adequacy of the model
Box.test(pred1)

