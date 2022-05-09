
#read the data
Raines <- ts(MagdeburgWeatherData[,6],start = c(1989,1),end = c(2015,12),frequency = 12)

Rainhs <- ts(MagdeburgWeatherData[,6],start = c(2016,1),end = c(2019,5),frequency = 12)
length(Tempes)

ggAcf(Raines)
ggPacf(Raines)

Diff2 <-diff(Raines,lag=12)
ggAcf(Diff2)
ggPacf(Diff2)
nsdiffs(Raines)

Rarima1 <- arima(Raines,order = c(1,0,1), seasonal = c(1,1,1))
checkresiduals(Rarima1)
summary(Rarima1) #not good

Rarima2 <- arima(Raines,order = c(1,0,1), seasonal = c(1,0,0))
checkresiduals(Rarima2)
summary(Rarima2) #doesn't work

Rarima3 <- arima(Raines,order = c(2,0,1), seasonal = c(2,1,1))
checkresiduals(Rarima3)
summary(Rarima3) #works Rmse=0.8265921 ; aic=816.81

Rarima4 <- arima(Raines,order = c(1,0,1), seasonal = c(2,1,1))
checkresiduals(Rarima4)
summary(Rarima4) #a bit better than best solution

Rarima5 <- arima(Raines,order = c(2,0,1), seasonal = c(1,0,1))
checkresiduals(Rarima5)
summary(Rarima5) #not good

Auto_Arima <- auto.arima(Raines,stepwise = FALSE,approximation = FALSE)
summary(Auto_Arima)

Rarima9 <- arima(Raines,order = c(1,0,0), seasonal = c(0,0,1))
checkresiduals(Rarima9)
summary(Rarima9) #RMSE = 0.8956338

Rarima10 <- arima(Raines,order = c(0,0,0), seasonal = c(0,0,1))
checkresiduals(Rarima9)
summary(Rarima10) #RMSE = 0.8981013 - best solution

