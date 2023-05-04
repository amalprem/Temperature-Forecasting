library(forecast)
library(vars)

D <- read.csv("weatherdata.csv")
D
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

spectrum(temperature,log='no',xlab='Frequencies',
         ylab='Power',
         main='Periodogram Temperature')


#Plotting timeseries
plot(temperature, xlab="datapoints", ylab = "temperature", main = "Temperature Time series plot",type="l")

plot(humidity, xlab="datapoints", ylab = "humidity", main = "Humidity Time series plot",type="l")

plot(dewpoint, xlab="datapoints", ylab = "dewpoint", main = "Dewpoint Time series plot",type="l")

plot(wind, xlab="datapoints", ylab = "wind", main = "Wind Time series plot",type="l")

plot(precipitation, xlab="datapoints", ylab = "precipitation", main = "Precipitation Time series plot",type="l")


library(tseries)


# Temperature
adf.test(temperature)

kpss.test(temperature)

# Humidity
adf.test(humidity)

# Dewpoint
adf.test(dewpoint)

kpss.test(dewpoint)

#Wind
adf.test(wind)

#Precipitation
adf.test(precipitation)

# Differencing the data
diff_temperature <- diff(temperature, differences = 1, lag = 365)

plot(diff_temperature, xlab="datapoints", ylab = "temperature", main = "Temperature Time series plot",type='l')

adf.test(diff_temperature)

diff_temperature[1]
temperature[1]
temperature[366]

diff_dewpoint <- diff(dewpoint, differences = 1, lag = 365)

plot(diff_dewpoint, xlab="datapoints", ylab = "dewpoint", main = "Dewpoint Time series plot",type='l')

adf.test(diff_dewpoint)


diff_humidity= diff(humidity, differences = 1, lag = 365)
diff_wind= diff(wind, differences = 1, lag = 365)
diff_precipitation= diff(precipitation, differences = 1, lag = 365)



acf(diff_temperature,main='Temperature')
acf(diff_humidity,main='Humidity')
acf(diff_dewpoint,main='Dewpoint')
acf(diff_wind,main='Wind')
acf(diff_precipitation, main='Precipitation')

pacf(diff_temperature,main='Temperature')
pacf(diff_humidity,main='Humidity')
pacf(diff_dewpoint,main='Dewpoint')
pacf(diff_wind,main='Wind')
pacf(diff_precipitation, main='Precipitation')


ccf(diff_temperature, diff_humidity,main='CCF Temperature and Humidity')
ccf(diff_temperature, diff_dewpoint,main='CCF Temperature and Dewpoint')
ccf(diff_temperature, diff_wind,main='CCF Temperature and Wind')
ccf(diff_temperature, diff_precipitation,main='CCF Temperature and Precipitation')

spectrum(diff_temperature,log='no',xlab='Frequencies',
         ylab='Power',
         main='Periodogram Temperature')


spectrum(diff_humidity,log='no',xlab='Frequencies',
         ylab='Power',
         main='Periodogram Humidity')

spectrum(diff_dewpoint,log='no',xlab='Frequencies',
         ylab='Power',
         main='Periodogram Dewpoint')

spectrum(diff_wind,log='no',xlab='Frequencies',
         ylab='Power',
         main='Periodogram Wind')


spectrum(diff_precipitation,log='no',xlab='Frequencies',
         ylab='Power',
         main='Periodogram Precipitation')

# Based on ACF AND CCF,creating features using lag values

temperature_lag1= filter(diff_temperature,c(0,1),sides=1)
temperature_lag2=filter(temperature_lag1,c(0,1),sides=1)
temperature_lag3=filter(temperature_lag2,c(0,1),sides=1)

dewpoint_lag1=filter(diff_dewpoint,c(0,1),sides=1)
dewpoint_lag2=filter(dewpoint_lag1,c(0,1),sides=1)
dewpoint_lag3=filter(dewpoint_lag2,c(0,1),sides=1)

wind_lag1=filter(diff_wind,c(0,1),sides=1)
wind_lag2= filter(wind_lag1,c(0,1),sides=1)
wind_lag3= filter(wind_lag2,c(0,1),sides=1)

humidity_lag1= filter(diff_humidity,c(0,1),sides=1)
humidity_lag2= filter(humidity_lag1,c(0,1),sides=1)
humidity_lag3= filter(humidity_lag2,c(0,1),sides=1)


n=length(diff_temperature)
n
diff_temperature=diff_temperature[4:n]
temperature_lag1=temperature_lag1[4:n]
temperature_lag2=temperature_lag2[4:n]
temperature_lag3=temperature_lag3[4:n]

diff_humidity=diff_humidity[4:n]
humidity_lag1=humidity_lag1[4:n]
humidity_lag2=humidity_lag2[4:n]
humidity_lag3=humidity_lag3[4:n]

diff_dewpoint=diff_dewpoint[4:n]
dewpoint_lag1=dewpoint_lag1[4:n]
dewpoint_lag2=dewpoint_lag2[4:n]
dewpoint_lag3=dewpoint_lag3[4:n]

diff_wind=diff_wind[4:n]
wind_lag1=wind_lag1[4:n]
wind_lag2=wind_lag2[4:n]
wind_lag3=wind_lag3[4:n]





#--------------------------------------------------------------
data_arima_0= cbind(temperature_lag1,temperature_lag2,temperature_lag3,
                    humidity_lag1,humidity_lag2,humidity_lag3, dewpoint_lag1,dewpoint_lag2,dewpoint_lag3
                    ,wind_lag1,wind_lag2,wind_lag3)


data_arima_1= cbind(temperature_lag1,temperature_lag2,
                    humidity_lag1,humidity_lag2, dewpoint_lag1,dewpoint_lag2,
                  wind_lag1,wind_lag2)

data_arima_2 = cbind(temperature_lag1,
                     humidity_lag1, dewpoint_lag1,
                     wind_lag1)

for(i in 1:6) {
  for (j in 1:6){
    if ((i == 4 & j == 4) || (i==6 & j==3)) {
      break
    }
    complex_model_3lag=arima(diff_temperature[0:750], xreg=data_arima_0[0:750,], order=c(i,0,j))
    pr=predict(complex_model_3lag,newxreg=data_arima_0[751:784,])
    
    ts.plot(diff_temperature[751:784], pr$pred, lty=1:2, col=c("blue", "red"))
    
    errors <- diff_temperature[751:784] - pr$pred
    
    mae <- mean(abs(errors))
    mse <- mean(errors^2)
    rmse <- sqrt(mse)
    cat()
    cat(AIC(complex_model_3lag),BIC(complex_model_3lag),mae,mse,rmse,i,j,"\n")
    
    cat()
    
    
  }
}
## 5,2 gives the minimum rmse


for(i in 1:6) {
  for (j in 1:6){
    if ((i == 4 & j == 4) || (i==6 & j==3)) {
      break
    }
    complex_model_2lag=arima(diff_temperature[0:750], xreg=data_arima_1[0:750,], order=c(i,0,j))
    pr=predict(complex_model_2lag,newxreg=data_arima_1[751:784,])
    
    ts.plot(diff_temperature[751:784], pr$pred, lty=1:2, col=c("blue", "red"))
    
    errors <- diff_temperature[751:784] - pr$pred
    
    mae <- mean(abs(errors))
    mse <- mean(errors^2)
    rmse <- sqrt(mse)
    cat()
    cat(AIC(complex_model_2lag),BIC(complex_model_2lag),mae,mse,rmse,i,j,"\n")
    cat()
    
    
  }
}
## 3,1



for(i in 1:6) {
  for (j in 1:6){
    if ((i == 4 & j == 4) || (i==6 & j==3)) {
      break
    }
    average_model_1lag= arima(diff_temperature[0:750], xreg=data_arima_2[0:750,], order=c(i,0,j))
    pr=predict(average_model_1lag,newxreg=data_arima_2[751:784,])
    
    ts.plot(diff_temperature[751:784], pr$pred, lty=1:2, col=c("blue", "red"))
    
    errors <- diff_temperature[751:784] - pr$pred
    
    mae <- mean(abs(errors))
    mse <- mean(errors^2)
    rmse <- sqrt(mse)
    cat()
    cat(AIC(average_model_1lag),BIC(average_model_1lag),mae,mse,rmse,i,j,"\n")
    cat()
    
    
  }
}
# 5,5

for(i in 1:6) {
  for (j in 1:6){
    if ((i == 4 & j == 4) || (i==6 & j==3)) {
      break
    }
    simple_model= arima(diff_temperature[0:700], order=c(i,0,j))
   
    cat(AIC(simple_model),BIC(simple_model),i,j,"\n")
    cat()
    
    
  }
}

#1,1



## Have used for loop to try different values of order of AR and MA
# After that identified different models have different order of AR,MA where we get mininmum AIC, BIC
# Creating models with best combination of AIC, BIC

complex_model_3lag=arima(diff_temperature[0:750], xreg=data_arima_0[0:750,], order=c(5,0,2))
complex_model_2lag=arima(diff_temperature[0:750], xreg=data_arima_1[0:750,], order=c(3,0,1))
average_model_1lag= arima(diff_temperature[0:750], xreg=data_arima_2[0:750,], order=c(5,0,5))
simple_model= arima(diff_temperature[0:700], order=c(1,0,1))




AIC(complex_model_3lag)
BIC(complex_model_3lag)
AIC(complex_model_2lag)
BIC(complex_model_2lag)
AIC(average_model_1lag)
BIC(average_model_1lag)
AIC(simple_model)
BIC(simple_model)


res_complex=residuals(complex_model_2lag)
Box.test(res_complex,lag=5)

res_average=  residuals(average_model_1lag)
Box.test(res_average,lag=5)



plot(res_complex, ylab='Residuals',main='Residual plot for complex model')

adf.test(res_complex)

acf(res_complex, 10, main="Acf Residuals")
spectrum(res_complex,log='no')
## The spectral analysis shows random spikes
spectrum(res_complex,spans=c(10,10))



pr=predict(complex_model_2lag,newxreg=data_arima_1[751:784,])

ts.plot(diff_temperature[750:783], pr$pred, lty=1:2, col=c("blue", "red"),
        ylab='Temperature Difference', main='Predicting Difference in Temperature')

errors <- diff_temperature[750:783] - pr$pred
errors
# calculate the metrics
mae <- mean(abs(errors))
mse <- mean(errors^2)
rmse <- sqrt(mse)

# print the results
cat("MAE:", round(mae, 2), "\n")
cat("MSE:", round(mse, 2), "\n")
cat("RMSE:", round(rmse, 2), "\n")




## Actual values
predicted_actual <- numeric(length(pr$pred))
actual_value <- numeric(length(pr$pred))
for(i in 1:length(pr$pred)) {
  predicted_actual[i] <- pr$pred[i] + temperature[753+i]
  actual_value[i]<- diff_temperature[749+i]+ temperature[753+i]
}


actual_value = ts(actual_value,start=1,end=34)
ts.plot(actual_value, predicted_actual, lty=1:2, col=c("blue", "red"),
        ylab='Temperature', main='Predicting Temperature')

#temperature[753]
#temperature[1118]
#diff_temperature[750]
#pr$pred[1]
#temperature[754]
#temperature[1119]
#diff_temperature[751]
#pr$pred[2]
#--------------------------------------------------------------


#-------------------------------------------------------------

data_var= cbind(diff_temperature,diff_humidity,diff_dewpoint,diff_wind)
train_data <- data_var[1:750, ]
test_data <- data_var[751:784, ]

# Fit a VAR model with p = 2 and a constant term
library(vars)
var_model <- VAR(train_data, p = 50, type = "const")

# Forecast the next 90 time points
var_forecast <- predict(var_model, n.ahead = nrow(test_data), ci = 0.95)

# Extract the forecasted temperature values
temp_forecast <- var_forecast$fcst$diff_temperature

predicted_var=temp_forecast[1:34]
predicted_var
actual_var=test_data[, "diff_temperature"]

error_var= actual_var-predicted_var
error_var

mae_var <- mean(abs(error_var))
mse_var <- mean(error_var^2)
rmse_var <- sqrt(mse_var)

# print the results
cat("MAE:", round(mae_var, 2), "\n")
cat("MSE:", round(mse_var, 2), "\n")
cat("RMSE:", round(rmse_var, 2), "\n")

ts.plot(ts(test_data[, "diff_temperature"], start=1, end=34),
        var_forecast$fcst$diff_temperature[1:34], lty=1:2,
        col=c("blue", "red"),
        ylab='Temperature',main='Predicting Temperature')


