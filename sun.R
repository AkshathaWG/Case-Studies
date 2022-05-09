
#read the data
Sunes <- ts(MagdeburgWeatherData[,7],start = c(1989,1),end = c(2015,12),frequency = 12)

Sunhs <- ts(MagdeburgWeatherData[,7],start = c(2016,1),end = c(2019,5),frequency = 12)
length(Sunes)

ggAcf(Sunes)
ggPacf(Sunes)
nsdiffs(Sunes)

Diff3 <-diff(Sunes,lag=12)
ggAcf(Diff3)
ggPacf(Diff3)

Sarima1 <- arima(Sunes,order = c(1,0,1), seasonal = c(1,1,1))
checkresiduals(Sarima1)
summary(Sarima1) #not good

Sarima2 <- arima(Sunes,order = c(1,0,1), seasonal = c(1,1,2))
checkresiduals(Sarima2)
summary(Sarima2) #doesn't work

Sarima3 <- arima(Sunes,order = c(2,0,1), seasonal = c(1,1,0))
checkresiduals(Sarima3)
summary(Sarima3) #works Rmse=0.8265921 ; aic=816.81

Sarima4 <- arima(Sunes,order = c(1,0,1), seasonal = c(2,1,1))
checkresiduals(Sarima4)
summary(Sarima4) #a bit better than best solution

Sarima5 <- arima(Sunes,order = c(1,0,0), seasonal = c(2,1,0))
checkresiduals(Sarima5)
summary(Sarima5)

Auto_Arima <- auto.arima(Sunes,stepwise = FALSE,approximation = FALSE)
summary(Auto_Arima)

Sarima9 <- arima(Sunes,order = c(2,0,2), seasonal = c(2,1,0))
checkresiduals(Sarima9)
summary(Sarima9) # not best solution

