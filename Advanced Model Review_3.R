# gains table in R
library(gains)
gains(actual = Auto$displacement, predicted = Auto$displacement + runif(nrow(Auto)))

# Logistic Regression, yes-no response variable
# log-odds ratio or logit: pi/(1-pi)
# logistic regression: logit(pi) = log(pi/(1-pi)) = a + bx + e
# pi = (1+exp(-(a+bx)))^(-1)

bottle = data.frame(n=rep(500,6),deposit=c(2,5,10,20,25,30),y=c(72,103,170,296,406,449))
plot(bottle$deposit,bottle$y/bottle$n)
bot2 = data.frame(x=rep(bottle$deposit,2),y=c(rep(0,6),rep(1,6)),count=c(500-bottle$y,bottle$y))
fit = glm(y~x,bot2,family=binomial,weight=count)
points(bottle$deposit,fit$fitted.values[1:6],pch=2,col=2)
x = seq(2,30,length=100)
lines(x,predict(fit,data.frame(x=x),type="response"),col=2)
summary(fit)
confint(fit)
predict(fit,data.frame(x=15),type="response")

# Backward Selection
set.seed(12345)
defaultsmall = default[sample(1:nrow(default),500),]
fit = glm(default~log(downpmt+1),pmttype+age+gender,binomial,defaultsmall)
step(fit)

# ROC Curve
a = (0:100)/100
tpr = rep(NA,101)
fpr = rep(NA,101)
denom = table(Default$default)
for(i in 1:101){
    num = table(Default$default[fit$fitted.values>a[i]])
    fpr[i] = num[1]/denom[1]
    tpr[i] = num[2]/denom[2]
}
plot(fpr,tpr,type="l",xlab="False Positive Rate",ylab="True Positive Rate")
abline(0,1)
b=(0:10)*10+1
points(fpr[b],tpr[b],pch=16)
text(fpr[b[-1]]+0.01,tpr[b[-1]],paste("c=",as.character(a[b[-1]])),adj=0)
text(1,.98,"c=0")

library(pROC)
plot.roc(Default$default,fit$fitted.values)

gpa = read.csv("Desktop/gmatgpa.csv")
names(gpa)
plot(gpa$GPA, gpa$GMAT, col=gpa$admit, pch=gpa$admit, xlab="GPA", ylab="GMAT")
# fit LDA model
library(MASS)
fit = lda(admit~GPA+GMAT,data=gpa,CV=T)
names(fit)
table(gpa$admit,fit$class)
sum(diag(table(gpa$admit, fit$class))) / nrow(gpa) # classification rate

# fit QDA model
fit2 = qda(admit~GPA+GMAT,data=gpa,CV=T)
table(gpa$admit,fit2$class)
sum(diag(table(gpa$admit, fit2$class))) / nrow(gpa) # classification rate
