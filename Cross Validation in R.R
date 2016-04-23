# The idea is to use all the data as training data and all the data as test data

install.packages("ISLR")
install.packages("boot")
library(ISLR)

# LOOCV approach
attach(Auto)
View(Auto)

# fit a linear model
model = glm(mpg~horsepower,data = Auto)
summary(model)

library(boot)
MSE_LOOCV = cv.glm(Auto,model)
MSE_LOOCV$delta[1] # MSE for the model

MSE_LOOCV = NULL
for (i in 1:10){
    model = glm(mpg~poly(horsepower,i),data=Auto)
    MSE_LOOCV[i] = cv.glm(Auto,model)$delta[1]
}

MSE_LOOCV  # poly from 1 to 10, MSE


### K-Fold
K_MSE_LOOCV = NULL
for (i in 1:10){
    model_k = glm(mpg~poly(horsepower,i),data=Auto)
    K_MSE_LOOCV[i] = cv.glm(Auto,model_k, K = 5)$delta[1]
}

K_MSE_LOOCV  # poly from 1 to 10, MSE








