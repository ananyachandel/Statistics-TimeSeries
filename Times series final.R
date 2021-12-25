library(fpp2)
library(tseries)
library(readxl)

#read the excel file 
energy <- read_excel("C:/Users/anany/Desktop/STATS CA2/arima.xlsx")
#convert the data into timeseries data object
tenergy <- ts(energy$Estonia, start=1990, frequency=1)
tenergy

#plotting the time series
plot(tenergy)

#there is a trend in time series and no seasonality hence holts model is suitable
fit1 <- holt(tenergy, h=3)
plot(fit1)
fit1
#Evaluating Model Fit
qqnorm(fit1$residuals)
qqline(fit1$residuals)
Box.test(fit1$residuals, type="Ljung-Box")
#checkresiduals(fit1)
accuracy(fit1)

########################################################################################

#auto ARIMA function
plot(tenergy)
fit2<-auto.arima(tenergy)

fit2


#Evaluating Model Fit
qqnorm(fit2$residuals)
qqline(fit2$residuals)
Box.test(fit2$residuals, type="Ljung-Box")
checkresiduals(fit2)
accuracy(fit2)


#Forecasting with the fitted model
forecast(fit2, 3)
plot(forecast(fit2, 3), xlab="Year", ylab="Energy Consumption")

#######################################################################################
#Check the order of differencing required
ndiffs(tenergy)

#Plot the differenced Nile Time Series

#senergy<-ma(tenergy,4)
#plot(senergy)
#ndiffs(senergy)

denergy <-  diff(tenergy, differences=2)
plot(denergy)

#Assess stationarity of the differenced series
adf.test(denergy)

#ACF/PACF plots. Choosing p and q
Acf(denergy)
Pacf(denergy)

#Fitting an ARIMA(p,d,q) model
fit3 <- arima(tenergy, order=c(1,2,1))
fit3

#Evaluating Model Fit
qqnorm(fit3$residuals)
qqline(fit3$residuals)
Box.test(fit3$residuals, type="Ljung-Box")
checkresiduals(fit3)
accuracy(fit3)

#Forecasting with the fitted ARIMA(1,2,1)model
forecast(fit3, 3)
plot(forecast(fit3, 3), xlab="Year", ylab="Energy Consumption")

