Raines <- ts(MagdeburgWeatherdata[,6],start = c(1989,1),end = c(2015,12),frequency = 12)
autoplot(Raines)
Rainhs <- ts(MagdeburgWeatherdata[,6],start = c(2016,1),end = c(2019,5),frequency = 12)

#Mean Method
bm5 <-meanf(Raines)
summary(bm5)
#RMSE= 0.9023452

#Naive Method
bm6 <-rwf(Raines)
summary(bm6)
#RMSE=1.219596

#Seasonal Method
bm7 <-snaive(Raines)
summary(bm7)
#RMSE=1.218276

#Drift Method
bm8 <- rwf(Raines,drift = TRUE)
summary(bm8)
#RMSE= 1.219596