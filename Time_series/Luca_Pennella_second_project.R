library(tseries)  ## for significance tests
library(forecast) ## for auto.arima
library(TSA)      ## includes some datasets

# set margins of graphics
par(mar=c(4.1,4.1,3,2))
#par(mar=c(5.1,4.1,4.1,2.1))  ## default values

## list all datasets included in the 'TSA' package
#data(package="TSA")

# load data
data(beersales)
?beersales ## documentation
summary(beersales)
#beersales 
y <- beersales  ## Monthly beer sales in millions of barrels, 01/1975 - 12/1990.
y

###  try break the series  ###

y1 <- window(beersales, start=1976, end=1980)
y <- window(beersales, start=1981, end=1991)
y1
y

# original data
par(mfrow=c(2,1))
plot(y)  #evident seasonal patterns

###  check stationarity  ###

plot(y)       ## evident non-stationarity and seasonality
Acf(y)        ## slow decay
par(mfrow=c(1,1))

# seasonal differencing
y1 <- diff(y, 12)
par(mfrow=c(2,1))
plot(y1);abline(h=mean(y1)) ## still non-stationary
Acf(y1, lag.max=48)         ## fast decay at seasonal lags
par(mfrow=c(1,1))
adf.test(y1)  ## ADF:  rejected (stationary)
kpss.test(y1) ## KPSS: boderline

# non-seasonal differencing
y2 <- diff(y1)
par(mfrow=c(2,1))
plot(y2);abline(h=mean(y2)) ## seems stationary
Acf(y2, lag.max=48)         ## reasonably fast decay, SMA 2
par(mfrow=c(1,1))
adf.test(y2)  ## ADF: rejected (ok)
kpss.test(y2) ## KPSS: not rejected (ok)
## --> try to fit an arima with d=1 and D=1

###  arima model  ###

# acf/pacf of differenced data
par(mfrow=c(2,1))
Acf(y2, lag.max=48)
Pacf(y2, lag.max=48)
par(mfrow=c(1,1))
## it seems  ar: 1 or 2 / ma: 1
##          sar: 1 / sma: 1, 2 or 3

# automated identification
m1 <- auto.arima(y, d=1, D=1, seasonal=T, ic="aicc", stepwise=F)
m1  ## -> ARIMA(0,1,1)(0,1,2)[12] without drift
m1$coef/sqrt(diag(m1$var.coef))  ## z statistics

# model diagnostic
f1 <- fitted(m1)      ## fitted values
r1 <- residuals(m1)   ## residuals
Box.test(r1, lag=4)   ## box-pierce test on residuals: ok
Box.test(r1^2, lag=4) ## box-pierce test on squared residuals: sig.
#
par(mfrow=c(2,2))
plot(r1);abline(h=0) ## no evident pattern: ok
Acf(r1)              ## ok
Acf(r1^2)            ## several spikes at early lags
plot(c(f1), c(r1), xlab="fitted", ylab="residuals");abline(h=0)
## no evident pattern: ok
par(mfrow=c(1,1))

###  arma model with deterministic trend  ###

time <- 1:length(y)
cubic <- cbind(time,time2=time^2,time3=time^3)  ## terms of cubic trend
m0 <- Arima(y, xreg=cubic, order=c(0,0,0))
m0  ## estimates
m0$coef/sqrt(diag(m0$var.coef))  
## linear and cubic quadratic terms not sig.
## cubic term not sig. 

# cross-validation forecast error at up to one year (h=1,...,12)
#   data are % index numbers: interpret RMSE and MAE in %
err1 <- tsCV(y, function(x,h){
  forecast(Arima(x, order=c(0,1,1), seasonal=c(0,1,2),
                 include.constant=F), h=h)
}, h=12)
#
tab1 <- cbind(rmse=apply(err1, 2, function(x){sqrt(mean(x^2,na.rm=T))}),
              mae=apply(err1, 2, function(x){mean(abs(x),na.rm=T)})
)
#
plot(tab1[,1], xlab="h", ylab="Forecast error", ylim=range(tab1))
lines(tab1[,1]);points(tab1[,2]);lines(tab1[,2], lty=2)
legend("left", legend=c("RMSE","MAE"), lty=1:2, bty="n")

# display forecasts at up to one year (h=1,...,12)
pred1 <- forecast(m1, h=12*5)
plot(pred1, showgap=F)        ## forecasts
lines(f1, col="blue", lty=2)  ## fitted values

