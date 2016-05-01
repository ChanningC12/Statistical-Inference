# Time Series: Trend, Seasonality, Cyclical, Random
# Simple plot
chem = c(
    17,16.6,16.3,16.1,17.1,16.9,16.8,17.4,17.1,17,16.7,17.4,17.2,17.4,17.4,17,17.3,
    17.2,17.4,16.8,17.1,17.4,17.4,17.5,17.4,17.6,17.4,17.3,17,17.8,17.5,18.1,17.5,
    17.4,17.4,17.1,17.6,17.7,17.4,17.8,17.6,17.5,16.5,17.8,17.3,17.3,17.1,17.4,16.9,
    17.3,17.6,16.9,16.7,16.8,16.8,17.2,16.8,17.6,17.2,16.6,17.1,16.9,16.6,18,17.2,
    17.3,17,16.9,17.3,16.8,17.3,17.4,17.7,16.8,16.9,17,16.9,17,16.6,16.7,16.8,16.7,
    16.4,16.5,16.4,16.6,16.5,16.7,16.4,16.4,16.2,16.4,16.3,16.4,17,16.9,17.1,17.1,
    16.7,16.9,16.5,17.2,16.4,17,17,16.7,16.2,16.6,16.9,16.5,16.6,16.6,17,17.1,17.1,
    16.7,16.8,16.3,16.6,16.8,16.9,17.1,16.8,17,17.2,17.3,17.2,17.3,17.2,17.2,17.5,
    16.9,16.9,16.9,17,16.5,16.7,16.8,16.7,16.7,16.6,16.5,17,16.7,16.7,16.9,17.4,
    17.1,17,16.8,17.2,17.2,17.4,17.2,16.9,16.8,17,17.4,17.2,17.2,17.1,17.1,17.1,
    17.4,17.2,16.9,16.9,17,16.7,16.9,17.3,17.8,17.8,17.6,17.5,17,16.9,17.1,17.2,
    17.4,17.5,17.9,17,17,17,17.2,17.3,17.4,17.4,17,18,18.2,17.6,17.8,17.7,17.2,17.4)
y=ts(chem)
summary(y)
par(mfrow=c(1,1))
plot(y)

MA = function(y,m,k=20){
    # m = MA window length
    # k = prediction horizon
    n=length(y)
    plot(y,xlim=c(0,n+k))
    title(paste("m= ",m,sep=""))
    MA=filter(y,filter=rep(1/m,m),
              method="convolution",sides=1)
    yhat=rep(MA[n],k)
    lines(c(MA,yhat),col="red")
    invisible(MA)
}
MA(y,20)
MA(y,5)
MA(y,1)

# EWMA: give more weights to recent observations
# Lt is the weighted average of yt and the forecast Lt-1 of the current level
EWMA = function(y, alpha=.05, k=20)
{
    n=length(y)
    plot(y, xlim=c(0,n+k))
    title(paste("alpha =", alpha))
    L = alpha*filter(y, filter=1-alpha,
                     method="recursive", sides=1,
                     init=y[1]/alpha)
    yhat = rep(L[n], k)
    lines(c(L, yhat), col="red")
}

EWMA(y, .05)
EWMA(y, .2)
EWMA(y, 1)

## Holt-Winter will find the optimal alpha
fit = HoltWinters(y,beta=F,gamma=F)
fit
yhat = predict(fit,n.ahead=20,prediction.interval = T)
plot(fit,yhat,type="l")

# HoltWinter does not model seasonality
# Decomposition
fit = decompose(trade,type="additive")
names(fit)
plot(fit,type="b")

# Forecaset from a decomposition model
yhat = fit$trend + fit$seasonal
plot(trade,type="b")
lines(yhat,col="red")







