Tempes <- ts(MagdeburgWeatherData[,2],start = c(1989,1),end = c(2015,12),frequency = 12)
length(Tempes)
Temphs <- ts(MagdeburgWeatherdata[,2],start = c(2016,1),end = c(2019,5),frequency = 12)

#Mean Method
bm1 <-meanf(Tempes)
summary(bm1)
#RMSE= 6.664984

#Naive Method
bm2 <-rwf(Tempes)
summary(bm2)
#RMSE=4.099782

#Seasonal Method
bm3 <-snaive(Tempes)
summary(bm3)
#RMSE=4.087647

#Drift Method
bm4 <- rwf(Tempes,drift = TRUE)
summary(bm4)
#RMSE= 4.087624