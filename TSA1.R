shampoo_data <- shampoo
#
sh
head(sh)
tail(sh)
#time series
shampoo_datats <- ts(sh[,2],start=2000,freq=12)
shampoo_datats
#plot
plot(shampoo_datats)
plot(decompose(shampoo_datats))


library(forecast)
ggseasonplot(shampoo_datats)


#differencinng
d.shampoo_datats<-diff(log(shampoo_datats))
d.shampoo_datats
plot(d.shampoo_datats)

#Second order difference
d2.sha<-diff(d.sha)
plot(d2.sha)

#comparing
summary(shampoo_datats)
summary(d.shampoo_datats)
summary(d2.sha)

library(tseries)
#adf test
adf.test(shampoo_datats, alternative="stationary",k=3)
#p-value is high, so we accept the null and reject the alternate. So, data is non-stationary

#Augmented Dickey-Fuller test - with additional lags
adf.test(shampoo_datats,alternative="stationary")

#with lag
adf.test(d.shampoo_datats)
adf.test(d2.sha)

#
acf(shampoo_datats)

pacf(shampoo_datats)

#Run for first differences variable
acf(d.shampoo_datats)
pacf(d.shampoo_datats)

library("forecast")

#Automatic function to find best fit
ar1<-auto.arima(shampoo_datats)
ar1

#Automatic function to find best fit
auto.arima(d.shampoo_datats)

#Model Forecasting
fit<-arima(sha, order = c(1, 1, 1))
predict(fit,12)

#train tes
train <- ts(log(shampoo_datats[1:30]),start = 2000, frequency = 12)
mfit <- arima(train, order = c(1,1,1))
pred <- predict(mfit,6)$pred
exp(pred )
shampoo_datats
#foracasting
af<-forecast(ar1,h =12, level =95)
af
summary(af)
accuracy(af)