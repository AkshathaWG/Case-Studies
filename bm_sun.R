Sunes <- ts(MagdeburgWeatherdata[,7],start = c(1989,1),end = c(2015,12),frequency = 12)

Sunhs <- ts(MagdeburgWeatherdata[,7],start = c(2016,1),end = c(2019,5),frequency = 12)

#Mean Method
bm9 <-meanf(Sunes)
summary(bm9)
#RMSE= 2.512327

#Naive Method
bm10 <-rwf(Sunes)
summary(bm10)
#RMSE=2.017086

#Seasonal Method
bm11 <-snaive(Sunes)
summary(bm11)
#RMSE=1.759562

#Drift Method
bm12 <- rwf(Sunes,drift = TRUE)
summary(bm12)
#RMSE= 2.017084