library(readxl)
library(tseries)
library(forecast)
library(ggplot2)
library(stats)
library(lmtest)

Consumption=read_excel("D:/Data for Research/Revised_Time Series Project/Power Consumption from 1973 to 2015.xlsx")
powerconsumptionts=ts(Consumption$`Total Power Consumption`,frequency=1,start=1973,end=2020)
ts.plot(powerconsumptionts, xlab="Year", ylab="Gigawatt Hours", main="Total Power Consumption in the Philippines ", lwd=2, col="blue")

powerconsmodelbuilding <- read_excel("D:/Data for Research/Revised_Time Series Project/Power Consumption from 1973 to 2015.xlsx", range="A1:I44")
ts1=ts(powerconsmodelbuilding$`Total Power Consumption`,frequency=1,start=1973,end=2015)
ts.plot(ts1,xlab="Year",ylab="Gigawatt hours", main="Total Power Consumption from 1973 to 2015",lwd = 2,col="blue")
acf(ts1, main="ACF Plot of the Series")

#ADF test for the total consumption
adf.test(ts1) #non-stationary (p=0.9824)

#BoxCox lambda=0

newts1=BoxCox(ts1,0)
plot(newts1,xlab="Year",ylab="Gigawatt Hours", main="Log-transformed Time Series", lwd=2,col="red")
acf(newts1,main="ACF Plot of the Log-Transformed Series")
pacf(newts1,main="PACF Plot of the Log-Transformed Series")

#ADF test for the new time series data
adf.test(newts1) #non-stationary (p=0.2757)

#1st differencing to transformed series
d1=diff(newts1,1)
plot(d1,xlab="Year",ylab="Gigawatt Hours", main="First Differenced Time Series", lwd=2,col="red")

#ADF test on the first-differenced series
adf.test(d1) #non-stationary (p=0.08978)

#ACF and PACF of the first-differenced series
acf(d1,main="ACF Plot of the First-Differenced Series")
pacf(d1,main="PACF Plot of the First-Transformed Series")

#Second Differencing
d2=diff(diff(newts1,1),1)
plot(d2, xlab="Year", ylab="Gigawatt Hours", main="Second Differenced Time Series", lwd=2, col="red")

#ADF test on the second-differenced series
adf.test(d2) #stationary (p<0.01)

#ACF and PACF of the second-differenced series
acf(d2, main="ACF Plot of the Second-Transformed Series")
pacf(d2, main="PACF Plot of the Second-Transformed Series")

#Tentative Models #p=0,1,2 d=2, q=0,1,2
model1=Arima(ts1, order = c(0,2,0), lambda = 0)
model2=Arima(ts1, order = c(0,2,1), lambda = 0)
model3=Arima(ts1, order = c(0,2,2), lambda = 0)
model4=Arima(ts1, order = c(1,2,0), lambda = 0)
model5=Arima(ts1, order = c(1,2,1), lambda = 0)
model6=Arima(ts1, order = c(1,2,2), lambda = 0)
model7=Arima(ts1, order = c(2,2,0), lambda = 0)
model8=Arima(ts1, order = c(2,2,1), lambda = 0)
model9=Arima(ts1, order = c(2,2,2), lambda = 0)

#AIC
model1$aic #-116.4855
model2$aic #-136.6084 *
model3$aic #-134.8391
model4$aic #-124.3067
model5$aic #-134.8495
model6$aic #-133.2334
model7$aic #-125.7119
model8$aic #-132.8622
model9$aic #-130.8495

#SUMMARY
                  #MAPE         #MASE
summary(model1)   #4.456948     #0.8591447
summary(model2)   #3.352819     #0.6323103
summary(model3)   #3.32292      #0.630539
summary(model4)   #3.961352     #0.7430818
summary(model5)   #3.322324 *   #0.630511
summary(model6)   #3.235572     #0.6095918 *
summary(model7)   #3.685615     #0.6832056
summary(model8)   #3.32372      #0.6302085
summary(model9)   #3.322393     #0.6304735

#model estimation
model=model2
coeftest(model) #MA(1) is significant

#Diagnostic checking
plot(model$residuals,type="p",ylab="Residuals",main="Plot of Residuals vs. Time")
abline(0,0)

plot(as.numeric(model$fitted),model$residuals,xlab="Fitted", ylab="Residuals", main="Plot of Residuals vs. Fitted Value")
abline(0,0)

qqnorm(model$residuals, main="Normal Q-Q Plot of Residuals")
qqline(model$residuals)

acf(model$residuals, main="ACF Plot of Residuals")
pacf(model$residuals, main="PACF Plot of Residuals")

shapiro.test(model$residuals) #The Residuals are normal (p=0.7652)

#Ljung-Box test
Box.test(model$residuals,type = "Lj", lag=10) #The model does not show lack of fit (p= 0.8249)

#FORECAST EVALUATION
validation <- read_excel("D:/Data for Research/Revised_Time Series Project/Power Consumption from 2016 to 2020.xlsx")
ts2=ts(validation$`Total Power Consumption`,frequency=1,start=2016,end=2020)
newmodel=Arima(c(ts1,ts2),model=model,lambda = 0)
onestep=fitted(newmodel)[44:48]


#actual and forecasted plot
ts3=ts(onestep,frequency = 1,start=2016,end=2020)
ts.plot(ts2,ts3,xlab="Year",ylab="Gigawatt hours",col=c("blue","red"),lwd=2)+legend("topleft",bty="o",lty=c(1,1),col=c("blue","red"),legend=c("Actual","Forecasted"),cex=0.7,inset=0.025,lwd=2)

#forecast errors
FE=validation-onestep


#ACF and PACF of forecast errors
acf(FE$`Total Power Consumption`,main="ACF of Forecast Errors")
pacf(FE$`Total Power Consumption`,main="PACF of Forecast Errors")
qqnorm(FE$`Total Power Consumption`)
qqline(FE$`Total Power Consumption`)

shapiro.test(FE$`Total Power Consumption`) #The Forecast Errors are normally distributed (p=0.2733)

evaluation=cbind(ts2,onestep,FE$`Total Power Consumption`)
colnames(evaluation)=c("Actual","Forecast","Forecast Error")
print(evaluation)

#forecast
forecast=forecast(c(ts1,ts2),model=model,h=5)
print(forecast)
plot(forecast)
