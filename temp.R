#set-up
library("forecast");library("ggplot2");library("readxl");

#read the data
MagdeburgWeatherData <- read_excel("E:/SS19/Business Forecasting/POBF2eDataFiles/MagdeburgWeather.xlsx")
head(MagdeburgWeatherData)
MagdeburgWeather <- ts(MagdeburgWeatherData[,2],start=c(1989,1),end = c(2019,5),frequency = 12)
autoplot(MagdeburgWeather)
Tempes <- ts(MagdeburgWeatherData[,2],start = c(1989,1),end = c(2015,12),frequency = 12)

Temphs <- ts(MagdeburgWeatherData[,2],start = c(2016,1),end = c(2019,5),frequency = 12)
length(Tempes)

ggAcf(Tempes)
ggPacf(Tempes)

Diff1 <-diff(Tempes,lag=12)
ggAcf(Diff1)
ggPacf(Diff1)

arima1 <- arima(Tempes,order = c(1,0,1), seasonal = c(1,1,1))
checkresiduals(arima1)
summary(arima1) #best solution

arima2 <- arima(Tempes,order = c(1,0,1), seasonal = c(1,1,0))
checkresiduals(arima2)
summary(arima2) #doesn't work

arima3 <- arima(Tempes,order = c(2,0,1), seasonal = c(2,1,1))
checkresiduals(arima3)
summary(arima3) #works but RMSE value high

arima4 <- arima(Tempes,order = c(1,0,1), seasonal = c(2,1,1))
checkresiduals(arima4)
summary(arima4) #a bit better than best solution

arima5 <- arima(Tempes,order = c(1,0,1), seasonal = c(1,1,2))
checkresiduals(arima5)
summary(arima5) #not good

arima6 <- arima(Tempes,order = c(1,0,1), seasonal = c(2,1,2))
checkresiduals(arima6)
summary(arima6) #not better

arima7 <- arima(Tempes,order = c(1,0,2), seasonal = c(1,1,1))
checkresiduals(arima7)
summary(arima7) #aic be bit high

arima8 <- arima(Tempes,order = c(2,0,1), seasonal = c(1,1,2))
checkresiduals(arima8)
summary(arima8) #aic value be high

Auto_Arima <- auto.arima(Tempes)
summary(Auto_Arima)

arima9 <- arima(Tempes,order = c(1,0,1), seasonal = c(1,1,0))
checkresiduals(arima9)
summary(arima9) #def not best solution
