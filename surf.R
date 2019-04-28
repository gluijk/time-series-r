# Windsurf vs Kitesurf. Series temporales con R
# www.datosimagensonido.com


# FUNCIÓN AUXILIAR
decomposedts.plot=function(decomposedts, legend=T,
        x='topright', ylab='', ...){
    plot(decomposedts$x, ylab=ylab, ...)
    lines(decomposedts$trend, col='green')
    lines(decomposedts$seasonal, col='blue')
    lines(decomposedts$random, col='red')
    abline(h=0)
    if (legend) legend(x=x, c('observed', 'trend', 'seasonal', 'random'),
        col=c('black', 'green', 'blue', 'red'), lty=c(1,1,1,1))
}


# DESCOMPOSICIÓN SERIES TEMPORALES

# Series temporales mensuales desde ene-04
surf=read.table("surf.csv", sep=',', header=T)  # CSV de Google
windsurfts=ts(surf$Windsurf, frequency=12, start=c(2004,1))
kitesurfts=ts(surf$Kitesurf, frequency=12, start=c(2004,1))

# Corrección logarítmica
logwindsurfts=log(windsurfts)
logkitesurfts=log(kitesurfts)

# Descomposición
logwindsurftscomps=decompose(logwindsurfts)  # type='additive'
logkitesurftscomps=decompose(logkitesurfts)  # type='additive'
plot(logwindsurftscomps)
plot(logkitesurftscomps)
decomposedts.plot(logwindsurftscomps, ylim=c(-0.5, 5), main='Windsurf')
decomposedts.plot(logkitesurftscomps, ylim=c(-0.5, 5), main='Kitesurf')

# Peso de las estacionalidades (seasonal/trend)
plot(logwindsurftscomps$figure/mean(logwindsurftscomps$trend, na.rm=T),
     type='o', col='blue', xaxt='n',
     main='Peso estacionalidades (seasonal/trend)', xlab='', ylab='')
lines(logkitesurftscomps$figure/mean(logkitesurftscomps$trend, na.rm=T),
     type='o', col='red')
abline(h=0)
axis(1, at=1:12, labels=months(ISOdate(2000,1:12,1)), las=2)
legend('topleft', c('Windsurf', 'Kitesurf'), col=c('blue', 'red'), lty=c(1,1))


# FORECASTING ARIMA(p,d,q)(P,D,Q)[m]

library(forecast)  # auto.arima() y forecast()

training=read.table("surftraining.csv", sep=',', header=T)  # ene-04 a dic-14
valid=read.table("surfvalid.csv", sep=',', header=T)  # ene-15 a ago-18

windsurfts=ts(training$Windsurf, frequency=12, start=c(2004,1))
kitesurfts=ts(training$Kitesurf, frequency=12, start=c(2004,1))
windsurfvalidts=ts(valid$Windsurf, frequency=12, start=c(2015,1))
kitesurfvalidts=ts(valid$Kitesurf, frequency=12, start=c(2015,1))

# Corrección logarítmica
logwindsurfts=log(windsurfts)
logkitesurfts=log(kitesurfts)


# Windsurf (2 métodos)
fit=auto.arima(logwindsurfts, trace=T)  # Best model: ARIMA(1,1,1)(0,1,1)[12]
fit=arima(logwindsurfts, order=c(1,1,1), seasonal=list(order=c(0,1,1)))

plot(forecast(fit, h=12*7), col='blue', fcol='blue', flty=3,  # Proy. 7 años
    main='Windsurf', xlab='Time', shadecols=c('gray90','gray75'))
legend('topright', c('Actual', 'Forecast'), col=c('blue','blue'), lty=c(1,3))

forewind=predict(fit, n.ahead=12*7)  # Proy. 7 años
U=forewind$pred+2*forewind$se
L=forewind$pred-2*forewind$se
ts.plot(logwindsurfts, forewind$pred, U, L,
    col=c('blue','blue','green','green'), lty=c(1,3,3,3), main='Windsurf')
legend('bottomleft', c('Actual', 'Forecast', 'Err bounds (95% confidence)'),
    col=c('blue','blue','green'), lty=c(1,3,3))


# Kitesurf (2 métodos)
fit=auto.arima(logkitesurfts, trace=T)  # Best model: ARIMA(0,1,1)(1,1,1)[12]
fit=arima(logkitesurfts, order=c(0,1,1), seasonal=list(order=c(1,1,1)))

plot(forecast(fit, h=12*7), col='red', fcol='red', flty=3,  # Proy. 7 años
    main='Kitesurf', xlab='Time', shadecols=c('gray90','gray75'))
legend('topright', c('Actual', 'Forecast'), col=c('red','red'), lty=c(1,3))

forekite=predict(fit, n.ahead=12*7)  # Proy. 7 años
U=forekite$pred+2*forekite$se
L=forekite$pred-2*forekite$se
ts.plot(logkitesurfts, forekite$pred, U, L,
    col=c('red','red','green','green'), lty=c(1,3,3,3), main='Kitesurf')
legend('bottomleft', c('Actual', 'Forecast', 'Err bounds (95% confidence)'),
    col=c('red','red','green'), lty=c(1,3,3))


# Forecasts desglose
ts.plot(exp(logwindsurfts), exp(forewind$pred),  # exp() deshace logaritmo
    exp(logkitesurfts), exp(forekite$pred),
    windsurfvalidts, kitesurfvalidts,
    col=c('blue','blue','red','red','green','green'),
    lty = c(1,3,1,3,1,1), ylim=c(0,100))
abline(h=0, col='gray')
legend('topright', c('Windsurf', 'Kitesurf', 'Validation', 'Forecast'),
    col=c('blue', 'red', 'green', 'black'), lty=c(1,1,1,3))
