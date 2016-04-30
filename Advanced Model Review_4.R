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







