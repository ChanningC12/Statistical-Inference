# Smoothing, smoothers summarizes nonlinear relationships
# Bin
# Wave Example
set.seed(1234567)
n=30
x=runif(n)*3
y=sin(x*pi/2)+rnorm(n)/4
dat=data.frame(x=round(x,3),y=round(y,3))

xx=seq(0,3,.05)
plot(dat$x,dat$y,pch=16,xlab="x",ylab="y")
lines(xx,sin(xx*pi/2))

# Bin smoother with three bins
dat$bin = cut(dat$x,0:3)
fit = lm(y~bin,dat)
summary(fit)

test = data.frame(x=runif(10000)*3)
test$y = sin(test$x*pi/2) + rnorm(10000)/4

dobin = function(dat, k, b){
    dat$bin = cut(dat$x, b);
    test$bin = cut(test$x, b);
    fit = lm(y ~ bin, dat);
    c(k, mean((fit$residuals)^2),
      mean((test$y-predict(fit, test))^2));
}


# Pricewise - Linear Fit to Wave
plot(dat$x,dat$y,pch=16,xlab="x",ylab="y")
lines(xx,sin(xx*pi/2),lwd=2)
fit = lm(y~x+I((x-1)*(x>1))+I((x-2)*(x>2)),dat)
lines(xx, predict(fit, data.frame(x=xx)), lty=2, col=2, lwd=2)
points(1:2, predict(fit, data.frame(x=1:2)), col=2, cex=2)
abline(v=c(1,2))

# cubic model
fit = lm(y ~ x+I(x^2)+I(x^3)+I((x>1)*(x-1)^3)+I((x>2)*(x-2)^3), dat)
summary(fit)
plot(dat[,1:2])
x=seq(0,3,length=100)
lines(x,sin(x*pi/2))
lines(x,predict(fit,data.frame(x=x)),col=2)
abline(v=c(1,2))

# splines
library(splines)
fit = lm(y~bs(x),dat)
drop1(fit,test="F")
fit = lm(y~bs(x,Boundary.knots = c(0,3)),dat)
plot(dat[,1:2],pch=16)
lines(x, predict(fit, data.frame(x=x)))
lines(x, sin(x*pi/2), col=2) # true function

fit = lm(y ~ bs(x, df=3, Boundary.knots=c(0,3)), dat)
lines(x, predict(fit, data.frame(x=x)), col=2)
c(mean(fit$residuals^2), mean((test$y-predict(fit, test))^2))

fit = lm(y ~ bs(x, df=5, Boundary.knots=c(0,3)), dat)
lines(x, predict(fit, data.frame(x=x)), col=3)

fit = lm(y ~ bs(x, df=11, Boundary.knots=c(0,3)), dat)
lines(x, predict(fit, data.frame(x=x)), col=4)

fit = lm(y ~ bs(x, df=17, Boundary.knots=c(0,3)), dat)
lines(x, predict(fit, data.frame(x=x)), col=5)
legend(.5,0, paste("df=",c(3,5,11,17)),col=2:5,lty=1)


# Test Scores Predicting GPA
library(gam)
gpa = read.csv("Desktop/gpa.csv")
fit = gam(GPA~s(Verbal)+s(Math),data=gpa)
summary(fit)
par(mfrow=c(1,2))
plot.gam(fit, se=T, scale=2.5)
mean(fit$residuals^2) # mean squared residuals
1-fit$deviance/fit$null.deviance # R-square







