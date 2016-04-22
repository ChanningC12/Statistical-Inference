# Choose good predictors
# Selection: forward/backward/stepwise/lasso
# Shrinkage / Regularization: ridge, lasso or dimensionality reduction models

# Interative Model Selection
# Forward Selection, begin with no variable, add variable that yields greatest significant improvement in SSE, until no significant improvement in SSE
# Backward elimination
# Stepwise selection

click$fair = as.numeric(click$eff==2)
click$good = as.numeric(click$eff==3)
click$outstand = as.numeric(click$eff==4)
fit = lm(sales~1,click) # null model
summary(fit)
fit2 = step(fit,scope=~ad+reps+fair+good+outstand,test="F") # stepwise regression

# backward selection
fit = lm(sales~ad+reps+fair+good+outstand,click)
step(fit)

fit = lm(defect~.,quality)
step(fit)

# Shrinkage, deal with multicollinearity
# Ridge Regression, include a penalty term in the least squares
bodyfat = data.frame(
    x1=c(19.5,24.7,30.7,29.8,19.1,25.6,31.4,27.9,22.1,25.5,31.1,30.4,
         18.7,19.7,14.6,29.5,27.7,30.2,22.7,25.2),
    x2=c(43.1,49.8,51.9,54.3,42.2,53.9,58.5,52.1,49.9,53.5,56.6,56.7,
         46.5,44.2,42.7,54.4,55.3,58.6,48.2,51.0),
    x3=c(29.1,28.2,37.0,31.1,30.9,23.7,27.6,30.6,23.2,24.8,30.0,28.3,
         23.0,28.6,21.3,30.1,25.7,24.6,27.1,27.5),
    y=c(11.9,22.8,18.7,20.1,12.9,21.7,27.1,25.4,21.3,19.3,25.4,27.2,
        11.7,17.8,12.8,23.9,22.6,25.4,14.8,21.1)
)

plot(bodyfat)
round(cor(bodyfat),2)
fit = lm(y~.,bodyfat)
vif(fit)
library(MASS)
Zbodyfat = data.frame(scale(bodyfat)) # standardize data
round(cor(Zbodyfat),2)
fit2 = lm.ridge(y~x1+x2+x3-1,Zbodyfat,lambda=seq(0,0.4,length=41)) # ridge regression
plot(fit2)
abline(h=0)
round(coef(fit2),4)

# The Lasso

# Test set, train set, K-fold
# test set, draw a sample of obs, set them in a safe while build the model using training set (should be large enough to estimate reliable estimates)
# k-fold: split available data into K roughly equal-sized parts

# test set in R
set.seed(12345)
View(employee)
employee$train = runif(nrow(employee))>0.5 # assign to test/train set
dim(employee)
table(employee$train)
fit = lm(Income~Age + Years, subset(employee,train==T))
summary(fit)
anova(fit)
sum(fit$residuals^2) # training SSE
deviance(fit) # or deviance
mean(fit$residuals^2) # training MSE
yhat = predict(fit,subset(employee,train==F))
length(yhat)
mean((employee$Income[!employee$train]-yhat)^2) #test MSE
