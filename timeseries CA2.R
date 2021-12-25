library(fpp2)
library(tseries)
library(readxl)

lifeexpectancy <- read_excel("C:/Users/anany/Desktop/STATS CA2/lifeexpectancy.xlsx")

#creating a time series object
tlife <- ts(lifeexpectancy$Hungary, start=c(1960,1), frequency=1) 
tlife

#plotting the time series
plot(tlife)


#there is a clear trend hence using holts model
fcast1 <- holt(tlife, h=3)
fcast1
#Evaluating Model Fit
qqnorm(fcast1$residuals)
qqline(fcast1$residuals)
Box.test(fcast1$residuals, type="Ljung-Box")
checkresiduals(fcast1)
accuracy(fcast1)

#############################################################################
#Check the order of differencing required
ndiffs(tlife)

#Plot the differenced Nile Time Series
dlife <-  diff(tlife, differences=1)
plot(dlife)

#Assess stationarity of the differenced series
adf.test(dlife)

#ACF/PACF plots. Choosing p and q
Acf(dlife)
Pacf(dlife)

#Fitting an ARIMA model
fit1 <- arima(dlife, order=c(1,1,1))
fit1

#Evaluating Model Fit
qqnorm(fit1$residuals)
qqline(fit1$residuals)
Box.test(fit1$residuals, type="Ljung-Box")
checkresiduals(fit1)
accuracy(fit1)

#auto ARIMA function
plot(dlife)
fit2<-auto.arima(dlife)
fit2
accuracy(fit2)


#Evaluating Model Fit
qqnorm(fit2$residuals)
qqline(fit2$residuals)
Box.test(fit2$residuals, type="Ljung-Box")
checkresiduals(fit2)
accuracy(fit2)

#Forecasting with the fitted model
forecast(fit2, 3)
plot(forecast(fit1, 3), xlab="Year", ylab="Life expectancy")

