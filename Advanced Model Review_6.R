# Regression Tree
library(tree)
fit = tree(y~x,dat)
partition.tree(fit,main="Fitted Function")
points(dat$x,dat$y,pch=16)

plot(fit)
text(fit,cex=.8)
deviance(fit) # gives SSE

# SSE of NULL Model
sum((dat$y - mean(dat$y))^2)
fit # node, split, n, deviance, yval

# gpa
par(mfrow=c(1,1))
fit = tree(GPA~Verbal+Math,gpa,mindev=1e-6)
plot(fit)
text(fit,cex=.8)
fit
partition.tree(fit, cex=.8)
fit2 = prune.tree(fit,best=5) # backward selection
plot(fit2)
text(fit2,cex=.8)
fit2
partition.tree(fit2, cex=.8)

# Regression Tree: numerical dependent variable
# Classification Tree: categorical dependent variable (factor in R)
# CART: Classification and Regression Tree
# CHAID: Chi-square automatic interaction detector
# C4.5: like CART
gmatgpa$status = factor(gmatgpa$admit,1:3,c("Admit","Deny","Wait"))
fit = tree(status~GMAT+GPA,gmatgpa)
plot(fit,type="uniform")
text(fit,cex=0.8)
fit
with(gmatgpa, plot(GPA, GMAT, col=admit, pch=admit))
partition.tree(fit,add=T,cex=.7)

# Your Turn
dat1 = expand.grid(x3=0:1, x2=0:1, x1=0:1)
dat2 = rbind(dat1,dat1)
dat2$y = c(rep(0,8),rep(1,8))
dat2$n = c(8,10,92,88,85,88,11,8,92,90,8,12,15,12,89,92)
fit = tree(y~x1+x2+x3,dat2,weight=n,minsize=5, mindev=.0001)
fit
plot(fit)
text(fit,cex=0.6)

# If you fit a tree on a different sample from the same population, the tree will likely be very different
# Bootstrap aggregation (bagging) exploits this variation to produce a better estimate
# Repeat many times, B=25
n=100
nbag=20
set.seed(1234567)
wave = data.frame(x=runif(n)*3)
wave$y = sin(wave$x*pi/2) + rnorm(n)/4
grid = seq(0,3,.05)
ans = double(length(grid))

plot(wave$x,wave$y,pch=16,xlab="x",ylab="y",cex=.7)
lines(grid,sin(grid*pi/2),lwd=4)
for(i in 1:nbag){
    train = sample(1:n, n, T)
    fit = tree(y ~ x, wave[train,])
    if(i<=6) lines(grid, predict(fit, data.frame(x=grid)), col=4)
    ans = ans + predict(fit, data.frame(x=grid))
}
ans = ans/nbag
lines(grid, ans, lwd=4, col=2)

# Test score predicting GPA
library(gam)
library(randomForest)
fit = randomForest(GPA~Verbal+Math,data=gpa,importance=T)
fit
importance(fit)
par(mfrow=c(1,2))
partialPlot(fit,gpa,"Verbal")
partialPlot(fit,gpa,"Math")





