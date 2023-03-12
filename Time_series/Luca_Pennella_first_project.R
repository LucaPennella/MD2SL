library(tseries)  ## for significance tests
library(forecast) ## for auto.arima
library(TSA)      ## includes some datasets

# set margins of graphics
par(mar=c(4.1,4.1,3,2))
#par(mar=c(5.1,4.1,4.1,2.1))  ## default values

## list all datasets included in the 'TSA' package
#data(package="TSA")

# load data
data(gold)
?gold ## documentation
summary(gold)
#gold 
y <- gold  ## Gold Price 
#Daily price of gold (in $ per troy ounce) for the 252 trading days of 2005

# original data
par(mfrow=c(2,1))
plot(y)

abline(v=c(154),col="blue") ## structural breaks?


###  check stationarity  ###

plot(y)       ## evident non-stationarity
Acf(y)        ## slow decay
par(mfrow=c(1,1))
adf.test(y)   ## ADF: not rejected (unit root)
kpss.test(y)  ## KPSS: rejected (non-stationary)

# differenced data
dy <- diff(y)
par(mfrow=c(2,1))
plot(dy);abline(h=mean(dy)) ## doubts about stationary in variance
Acf(dy)                     ## fast decay: ok

par(mfrow=c(1,1))
adf.test(dy)  ## ADF: rejected (ok)
kpss.test(dy) ## KPSS: not rejected (stationary)


###  arima model  ###

# acf/pacf of differenced data
par(mfrow=c(2,1))
Acf(dy); Pacf(dy)  ## seems ar: 2 / ma: 2
par(mfrow=c(1,1))

# automated identification
m1 <- auto.arima(y, d=1, seasonal=F, ic="aicc", stepwise=F)

m1  ## ARIMA(2,1,2) without drift

# model diagnostic
f1 <- fitted(m1)      ## fitted values
r1 <- residuals(m1)   ## residuals
Box.test(r1, lag=5)   ## box-pierce test on residuals: ok
Box.test(r1^2, lag=5) ## box-pierce test on squared residuals: ok
par(mfrow=c(2,2))
plot(r1);abline(h=0)  ## apparent change in variance at half 1960
Acf(r1)               ## ok
Acf(r1^2)             ## ok
plot(c(f1), c(r1), xlab="fitted", ylab="residuals");abline(h=0)
## no evident pattern
par(mfrow=c(1,1))

###  try break the series  ###

tbreak <- 154  ## da impostare
y1 <- window(gold, start=1, end=tbreak)
y2 <- window(gold, start=tbreak+1, end=length(gold))


# breaking data
par(mfrow=c(2,1))
plot(y2)

###  check stationarity  ###

plot(y2)       ## evident non-stationarity
Acf(y2)        ## slow decay
par(mfrow=c(1,1))
adf.test(y2)   ## ADF: not rejected (unit root)
kpss.test(y2)  ## KPSS: rejected (non-stationary)

# differenced data
dy2 <- diff(y2)
par(mfrow=c(2,1))
plot(dy2);abline(h=mean(dy2)) ## doubts about stationary in variance
Acf(dy2)                     ## fast decay: ok

par(mfrow=c(1,1))
adf.test(dy2)  ## ADF: rejected (ok)
kpss.test(dy2) ## KPSS: rejected (ok)

##  arima model  ###

# acf/pacf of differenced data
par(mfrow=c(2,1))
Acf(dy2); Pacf(dy2)  ## seems ar: 2 / ma: 2
par(mfrow=c(1,1))

# automated identification
m1 <- auto.arima(y2, d=1, seasonal=F, ic="aicc", stepwise=F)

m1  ## ARIMA(0,1,1) without drift

# model diagnostic
f1 <- fitted(m1)      ## fitted values
r1 <- residuals(m1)   ## residuals
Box.test(r1, lag=5)   ## box-pierce test on residuals: ok
Box.test(r1^2, lag=5) ## box-pierce test on squared residuals: ok
par(mfrow=c(2,2))
plot(r1);abline(h=0)  ## apparent change in variance at half 1960
Acf(r1)               ## ok
Acf(r1^2)             ## ok
plot(c(f1), c(r1), xlab="fitted", ylab="residuals");abline(h=0)
## no evident pattern
par(mfrow=c(1,1))

###  fit and remove linear trend  ###

# fit linear trend
time <- 1:length(y2)
m0 <- Arima(y2, xreg=time, order=c(0,0,0))
m0  ## estimates
m0$coef/sqrt(diag(m0$var.coef))  ## linear trend is significant

# detrended series
y0 <- residuals(m0)
par(mfrow=c(2,1))
plot(y0, type="l");abline(h=0)  ## seems stationary
Acf(y0)                         ##  ## decay is not so fast, maybe due
                                ##   to a slight stochastic trend
adf.test(y0)   ## ADF: (not ok)
kpss.test(y0)  ## KPSS: not rejected (ok)
## --> try to fit an arma with linear trend

###  arma model with deterministic trend  ###

# acf/pacf of detrended data
par(mfrow=c(2,1))
Acf(y0); Pacf(y0)  ## it seems ar: 6 / ma: 1
par(mfrow=c(1,1))

# fit arma with linear trend
m2 <- auto.arima(y2, d=0, xreg=time,
                 seasonal=F, ic="aicc", stepwise=F)
m2  ## -> ARMA(2,0) with linear trend
m2$coef/sqrt(diag(m2$var.coef))  ## linear trend is significant

# model diagnostic
f2 <- fitted(m2)      ## fitted values
r2 <- residuals(m2)   ## residuals
Box.test(r2, lag=5)   ## box-pierce test on residuals: ok
Box.test(r2^2, lag=5) ## box-pierce test on squared residuals: ok
#
par(mfrow=c(2,2))
plot(r2);abline(h=0)  ## no evident pattern
Acf(r2)               ## ok
Acf(r2^2)             ## ok
plot(c(f2), c(r2), xlab="fitted", ylab="residuals");abline(h=0)
## no evident pattern
par(mfrow=c(1,1))


###  model comparison  ###

err1 <- tsCV(y2, function(x,h) {
  forecast(Arima(x, order=c(0,1,1), include.constant=F), h=h)
}, h=10)
err2 <- tsCV(y2, function(x,h,xreg,newxreg) {
  forecast(Arima(x, order=c(2,0,0), include.constant=T), xreg=newxreg, h=h)
}, h=10, xreg=time)

rmseFun <- function(x){sqrt(mean(x^2,na.rm=T))}
maeFun <- function(x){mean(abs(x),na.rm=T)}
mapeFun <- function(x,actual){100*mean(abs(x/actual),na.rm=T)}

tab1 <- cbind(rmse=apply(err1, 2, rmseFun),
              mae=apply(err1, 2, maeFun),
              mape=apply(err1, 2, mapeFun, actual=y2)
)
tab2 <- cbind(rmse=apply(err2, 2, rmseFun),
              mae=apply(err2, 2, maeFun),
              mape=apply(err2, 2, mapeFun, actual=y2)
)

# -> m2 forecasts are worse and deteriorate rapidly as h increases
plot(tab1[,1], xlab="h", ylab="RMSE", ylim=range(tab1[,1],tab2[,1]))
lines(tab1[,1]);
points(tab1[,1], col=1); lines(tab1[,1], col=1)
points(tab2[,1], col=2); lines(tab2[,1], col=2)
legend("topleft", legend=c("ARIMA no drift","ARMA w/lin. trend"), lty=1, col=1:2, bty="n")

# display forecasts at up to 10 years (h=1,...,10)
pred1 <- forecast(m1, h=10, level=95)
pred2 <- forecast(m2, xreg=length(y)+1:10, level=95)
library(ggplot2)
autoplot(y) +
  autolayer(pred1, series="ARIMA no drift", showgap=F, alpha=0.5) +
  autolayer(f1, series="ARIMA no drift", lty=2) +
  autolayer(pred2, series="ARMA w/lin. trend", showgap=F, alpha=0.5) +
  autolayer(f2, series="ARMA w/lin. trend", lty=2) +
  xlab("Date") + ylab("gold price") +
  guides(colour=guide_legend(title=""), lty=2)