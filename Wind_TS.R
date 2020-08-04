#prepare
install.packages("ISLR")
install.packages("astsa ")
install.packages("forecast ")
install.packages("aTSA")
install.packages("xts")
install.packages('forecast', dependencies = TRUE)
install.packages("tseries")
install.packages("FitAR")
install.packages("TSA")
library(FitAR)
library(ISLR)
library(astsa)
library(forecast)
library(aTSA)
library(xts)
library(tseries)
library(TSA)

#importing and exploring
wind=read.csv("/Users/AmberZhao/Desktop/wind.csv")
head(wind)
dim(wind)
windts=ts(wind[,2],frequency=12,start=c(2006,1))
windts
plot.ts(windts, main="Monthly Maximum Wind Speed 2006 - 2018", ylab = "Wind Speed mph", )
acf(windts, main="ACF of Monthly Maximum Wind Speed 2006 - 2018",lag=length(windts)-1,)
pacf(windts,main="PACF of Monthly Maximum Wind Speed 2006 - 2018",,lag=length(windts)-1,)
acf(windts, main="ACF of Monthly Maximum Wind Speed 2006 - 2018, Lag =20",lag.max=20)
pacf(windts,main="PACF of Monthly Maximum Wind Speed 2006 - 2018, Lag = 20",lag.max=20)

#decomposition 
windtscomponents = decompose(windts)
windtscomponents$seasonal
plot(windtscomponents)

#stationary test
adf_windts = adf.test(windts,alternative="stationary")
adf_windts

#seasonal adjustment 
windts_seasonadjusted = windts - windtscomponents$seasonal
plot(windts_seasonadjusted, main="Seasonal Adjusted Monthly Maximum Wind Speed 2006 - 2018",
     ylab = "Wind Speed mph")

acf(windts_seasonadjusted, main="ACF of Seasonal Adjusted Monthly Maximum Wind Speed 2006 - 2018",lag=length(windts)-1,)
pacf(windts_seasonadjusted,main="PACF of Seasonal Adjusted Monthly Maximum Wind Speed 2006 - 2018",,lag=length(windts)-1,)

adf_windts_adjusted = adf.test(windts_seasonadjusted,alternative="stationary")
adf_windts_adjusted

#model fitting
eacf(windts)

BoxCox.lambda(windts)

auto.arima(windts,d=0)

#eacf model check 



#diagnostic measurements
fitarima1=arima(windts,order=c(1,0,0),seasonal = list(order=c(1,0,1), period=12),method='ML')
library(lmtest)
fitarima1
coeftest(fitarima1)
checkresiduals(fitarima1)
tsdiag(fitarima1)

sarima(fitarima1,order=c(1,0,0),seasonal = list(order=c(1,0,1), period=12))

fitarima2=arima(windts,order=c(0,0,0),seasonal = list(order=c(1,0,0), period=12),method='ML')
coeftest(fitarima2)
acf(fitarima2$residuals, main="ACF of Fitted ARIMA Residuals")
qqnorm(fitarima2$residuals, main="Normal Q-Q Plot of Fitted ARIMA Residuals")
qqline(fitarima2$residuals)
tsdiag(fitarima2)



#forecasting
windfuture = forecast(fitarima1,10)
plot(forecast(fitarima1,10))

ARMA001=arima(windts,order=c(0,0,1),method='ML')
ARMA0011=arima(windts,order=c(0,0,11),method='ML')
ARMA101=arima(windts,order=c(1,0,1),method='ML')
ARMA200=arima(windts,order=c(2,0,0),method='ML')
ARMA201=arima(windts,order=c(2,0,1),method='ML')
ARMA300=arima(windts,order=c(3,0,0),method='ML')
ARMA301=arima(windts,order=c(3,0,1),method='ML')
ARMA402=arima(windts,order=c(4,0,2),method='ML')
ARMA500=arima(windts,order=c(5,0,0),method='ML')
ARMA600=arima(windts,order=c(6,0,0),method='ML')
ARMA601=arima(windts,order=c(6,0,1),method='ML')
ARMA700=arima(windts,order=c(7,0,0),method='ML')
ARMA701=arima(windts,order=c(7,0,1),method='ML')
