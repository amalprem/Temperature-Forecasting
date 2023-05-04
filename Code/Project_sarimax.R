library(forecast)
library(vars)

D <- read.csv("weatherdata.csv")
summary(D)

# Reading Data
temperature= D$temperature
dewpoint = D$dewpoint
humidity = D$humidity
wind = D$wind
precipitation = D$precipitation
n= length(temperature)

date_string <- paste(D$year, D$month, D$day, sep = "-")
D$date <- as.Date(date_string)
date=D$date
# Standardizing the data 

temperature = (temperature - mean(temperature))
#temperature = ts(temperature, start=1, end=n)

dewpoint = (dewpoint - mean(dewpoint))
#dewpoint = ts(dewpoint, start=1, end=n)

humidity = (humidity - mean(humidity))
#humidity = ts(humidity, start=1, end=n)

wind = (wind - mean(wind))
#wind = ts(wind, start=1, end=n)

precipitation = (precipitation - mean(precipitation))
#precipitation = ts(precipitation, start=1, end=n)

n= nrow(D) 
Time= 1:n

s1 = cos(2*pi*Time*(1/400))
s2 = sin(2*pi*Time*(1/400))

plot(s1)
plot(temperature)
t2= Time*Time

temperature_lag1= filter(temperature,c(0,1),sides=1)
temperature_lag2=filter(temperature_lag1,c(0,1),sides=1)
humidity_lag1= filter(humidity,c(0,1),sides=1)
humidity_lag2=filter(humidity_lag1,c(0,1),sides=1)
dewpoint_lag1=filter(dewpoint,c(0,1),sides=1)
dewpoint_lag2=filter(dewpoint_lag1,c(0,1),sides=1)
wind_lag1= filter(wind,c(0,1),sides=1)
wind_lag2=filter(wind_lag1,c(0,1),sides=1)

data_arima_1= cbind(temperature_lag1,temperature_lag2,
                    humidity_lag1,humidity_lag2, dewpoint_lag1,dewpoint_lag2,
                    wind_lag1,wind_lag2,s1,s2)
data_arima_2= cbind(temperature_lag1,temperature_lag2,
                    humidity_lag1,humidity_lag2, dewpoint_lag1,dewpoint_lag2,
                    wind_lag1,wind_lag2,s1,s2,t2,Time)
data_arima_2

library(forecast)
fit <- auto.arima(temperature[3:750], xreg = data_arima_2[3:750,],
                  seasonal = TRUE, stepwise = FALSE, approximation = FALSE
                  ,max.p = 3, max.d = 0, max.q = 3)


pr=predict(fit,newxreg=data_arima_2[751:784,])

ts.plot(temperature[750:783], pr$pred, lty=1:2, col=c("blue", "red"),
        ylab='Temperature',main='Predicting Temperature')


errors <- temperature[750:783] - pr$pred
errors
# calculate the metrics
mae <- mean(abs(errors))
mse <- mean(errors^2)
rmse <- sqrt(mse)

# print the results
cat("MAE:", round(mae, 2), "\n")
cat("MSE:", round(mse, 2), "\n")
cat("RMSE:", round(rmse, 2), "\n")


