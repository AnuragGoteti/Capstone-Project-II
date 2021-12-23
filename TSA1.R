sh <- shampoo
#
sh
head(sh)
tail(sh)
#time series
sha <- ts(sh[,2],start=2000,freq=12)
sha
#plot
plot(sha)
plot(decompose(sha))


library(forecast)
ggseasonplot(sha)

#differencinng
d.sha<-diff(log(sha))
d.sha
plot(d.sha)

#Second order difference
d2.sha<-diff(d.sha)
plot(d2.sha)

#comparing
summary(sha)
summary(d.sha)
summary(d2.sha)

library(tseries)
#adf test
adf.test(sha, alternative="stationary",k=3)
#p-value is high, so we accept the null and reject the alternate. So, data is non-stationary

#Augmented Dickey-Fuller test - with additional lags
adf.test(sha,alternative="stationary")

#with lag
adf.test(d.sha)
adf.test(d2.sha)

#
acf(sha)

pacf(sha)

#Run for first differences variable
acf(d.sha)
pacf(d.sha)

library("forecast")

#Automatic function to find best fit
auto.arima(sha)

#Automatic function to find best fit
auto.arima(d.sha)

#Model Forecasting
fit<-arima(sha, order = c(1, 1, 1))
predict(fit,12)

#train tes
train <- ts(log(sha[1:30]),start = 2000, frequency = 12)
mfit <- arima(train, order = c(1,1,1))
pred <- predict(mfit,6)$pred
exp(pred )

