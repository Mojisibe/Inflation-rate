#import libraries
library(tseries)
library(fpp2)
library(forecast)

#read data
inflation_ts <- read.csv("Inflation.csv", header = T) #inflation_ts is the name assigned to the dataset
View(inflation_ts) # View the dataset

#convert data to time series format in other to for r to see it as a time series data
inflation_ts <- ts(inflation_ts[,2], start = c(2010, 1), frequency = 12) # inflation_ts[,2] is used to choose the second column since it is the column that contain the dataset
# start = c(2001,1) depicts that the data that is used for the analysis is starting from january, 2001
# frequency = 12 depict that data has a frequency of 12 month

#plot the time series dataset to observe it component
plot(inflation_ts, ylab = "inflation rate") # This shows the component of time series in the data
decompose_data <- decompose(inflation_ts, type = "additive") # This decompose the dataset to remove seasonality and irregularities in the data
decompose_data <- decompose(inflation_ts, type = "multiplicative")
plot(decompose_data) # This plot the data to shows the trend in the data

#test of stationarity
adf.test(inflation_ts) # This is will observe the stationarity in the data

#fit the model
fit <- auto.arima(inflation_ts)
summary(fit)

#forecast the model
pred1 <- predict(fit, n.ahead = 24)
pred1 <- forecast(fit)
plot(pred1)

#test for adequacy of the model
Box.test(pred1)

