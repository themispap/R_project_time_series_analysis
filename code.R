# Loading required packages
library(knitr)
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(tseries)

# Loading data
data=read.csv('data.csv')
data$date=as.Date(data$date,format="%d/%m/%y")
data=data.frame('date'=data$date,'value'=data$value)
r=range(data$value)[2]-range(data$value)[1]

#Time series
f=25
a=1
b=1
value_ts=ts(data$value,frequency = f,start = a)

#Plot
plot.ts(value_ts, main="Time series plot",ylab='value',col='#69b3a2',bty='n')

par(mfrow=c(1,2))
value_dec=decompose(value_ts)
plot(value_dec, yax.flip = TRUE,col='#69b3a2',bty='n')

lambda=BoxCox.lambda(value_ts)
log_ts=BoxCox(value_ts,lambda)
par(mfrow=c(1,2))
value_dec=decompose(log_ts)
plot(value_dec, yax.flip = TRUE,col='#69b3a2',bty='n')

n=4
par(mfrow=c(2,2))
t=1:length(value_ts)
for (i in 1:n){
  fit_poln=lm(value_ts ~ poly(t,i))
  print(paste('Degree of polynomial: ',i))
  print(paste('adj R squared: ',summary(fit_poln)$adj.r.squared))
  print(paste('AIC: ',AIC(fit_poln)))
  print(paste('BIC: ',BIC(fit_poln)))
  plot(value_ts,ylab='value',col='#69b3a2',bty='n')
  lines(ts(fit_poln$fitted.values,frequency = f,start = a),type="l",col="red",lwd = 1,pch = 10)
}

par(mfrow=c(1,1))
fit_poln=lm(value_ts ~ poly(t,3)) #Selection of the degree of the polynomial
value_pol=ts(value_ts-fit_poln$fitted.values,frequency = f,start = a)
plot.ts(value_pol,col='orange',main='Residuals',ylab='res',bty='n',ylim=c(-r/2,r/2))

adf.test(value_ts)  # p<0.05

kpss.test(value_ts)  # p>0.05

l=1 # Elimination of seasonality
d=ndiffs(value_ts, test = "adf") # Elimination of trend
value_dif=diff(value_ts,lag=l,differences = d)
plot.ts(value_dif,main='Residuals',ylab='res',col='orange',ylim=c(-r/2,r/2),bty='n')

par(mfrow=c(1,2))
plot(acf(value_ts,plot = F),main = 'Auto-Correlation Function Estimation',col='orange',bty='n')
q=11
plot(pacf(value_ts,plot = F),main = 'Partial Auto-Correlation Function Estimation',col='orange',bty='n')

p=1

par(mfrow=c(1,2))
fit_ar=arima(value_ts,order = c(p,0,0),method = 'CSS')
value_ar=fitted(fit_ar)
plot.ts(value_ts,ylab='value',main='AR(1) model',col='#69b3a2',bty='n')
lines(value_ar,col='red')
legend('bottomright',legend=c('observed','fitted'), col=c('#69b3a2','red'),pch=c('-','-'),bty='n')
plot.ts(value_ts-value_ar, yax.flip = TRUE,col='orange',main='Residuals',ylim=c(-r/2,r/2),ylab='res',bty='n')

par(mfrow=c(1,2))
fit_ma=arima(value_ts,order = c(0,0,q),method = 'CSS')
value_ma=fitted(fit_ma)
plot.ts(value_ts,ylab='value',main='MA(11) model',col='#69b3a2',bty='n')
lines(value_ma,col='red')
legend('bottomright',legend=c('observed','fitted'), col=c('#69b3a2','red'),pch=c('-','-'),bty='n')
plot.ts(value_ts-value_ma, yax.flip = TRUE,col='orange',main='Residuals',ylab='res',ylim=c(-r/2,r/2),bty='n')

par(mfrow=c(1,2))
fit_arima=arima(value_ts,order = c(p,d,q),method = 'CSS')
value_arima=fitted(fit_arima)
plot.ts(value_ts,ylab='value',main='ARIMA(1,1,11) model',col='#69b3a2',bty='n')
lines(value_arima,col='red')
legend('bottomright',legend=c('observed','fitted'), col=c('#69b3a2','red'),pch=c('-','-'),bty='n')
plot.ts(value_ts-value_arima, yax.flip = TRUE,col='orange',main='Residuals',ylab='res',ylim=c(-r/2,r/2),bty='n')

checkresiduals(fit_arima)  #h0 independence (p>0.05)

frcast=forecast(fit_arima, h = 7)
kable(frcast)

plot(frcast,main="Prediction",ylab='value',col='#69b3a2',bty='n')

kable(accuracy(frcast$fitted, data$value))