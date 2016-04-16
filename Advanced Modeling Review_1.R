# Supervised Learning: observe dependent variable or outcome y, identify relationship between x and y, Regression problem and Classification problem
# Unsupervised Learning: find interesting pattern in x, Clustering problem, Factor problem

# Click Ball Point Pens Example
# Read in data
click = data.frame(sales=c(260.3,286.1,279.4,410.8,438.2,315.3,565.1,570.0,426.1,315.0,
                           403.6,220.5,343.6,644.6,520.4,329.5,426.0,343.2,450.4,421.8,245.6,503.3,375.7,265.5,
                           620.6,450.5,270.1,368.0,556.1,570.0,318.5,260.2,667.0,618.3,525.3,332.2,393.2,283.5,
                           376.2,481.8), ad=c(5,7,6,9,12,8,11,16,13,7,10,4,9,17,19,9,11,8,13,14,7,16,9,5,18,18,
                                              5,7,12,13,8,6,16,19,17,10,12,8,10,12), reps=c(3,5,3,4,6,3,7,8,4,3,6,4,4,8,7,3,6,3,5,
                                                                                            5,4,6,5,3,6,5,3,6,7,6,4,3,8,8,7,4,5,3,5,5), eff=c(4,2,3,4,1,4,3,2,3,4,1,1,3,4,2,2,4,
                                                                                                                                             3,4,2,4,3,3,3,4,3,2,2,1,4,3,2,2,2,4,3,3,3,4,2))
# correlation
round(cor(click),4)
plot(click)

# Estimate the model
fit = lm(sales~ad, click)
plot(click$ad,click$sales,xlab="# TV spots", ylab = "Sales")
abline(fit)
summary(fit)
plot(fit)
confint(fit)
predict(fit,data.frame(ad=5))
predict(fit,data.frame(ad=5),interval = "prediction")
predict(fit,data.frame(ad=5),interval = "confidence")

# Multiple Linear Regression
# Process for Building a Regression Model
# 1. Inspect data for : outliers, typos, missing values, etc.
    # Generate n, means, mins, and maxs of each variables
    # Generate boxplot of each variable
    # Generate a scatterplot matrix to assess correlations between predictor variables and pairwise correlations with dependent variables
# 2. Estimate model and check normality and shape of residuals
# 3. Test overall significance of the model
    # H0: b1 = b2 = b3 =.....= bp = 0
    # H1: at least one bj != 0
# 4. Test significance of individual coefficients

fit = lm(sales~ad+reps+eff,click)
summary(fit)
# we cannot conclude that the eff affects sales since p-value > 0.05
predict(fit,data.frame(ad=4,reps=3,eff=1))

# Comparing regression coefficients
# Compare when convert to commensurate units, or use standardized regression coeffcients (all variable standardized before the analysis to have mean of 0 and variance of 1), the unit of measurement is now the standard deviation
Zclick = as.data.frame(scale(click[1:4]))
fit = lm(sales~ad+reps+eff -1,Zclick) # -1 drops intercept
summary(fit)

#newfood data
newfood = data.frame(
    sales=c(225,323,424,268,224,331,254,492,167,226,210,289,204,288,245,161,161,
            246,128,154,163,151,180,150),
    price=c(24,24,24,24,24,24,24,24,29,29,29,29,29,29,29,29,34,34,34,34,34,34,34,34),
    ad=c(0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1),
    loc=c(0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1),
    income=c(7.3,8.3,6.9,6.5,7.3,8.3,6.9,6.5,6.5,8.4,6.5,6.2,6.5,8.4,6.5,6.2,
             7.2,8.1,6.6,6.1,7.2,8.1,6.6,6.1),
    volume=c(34,41,32,28,34,41,23,37,33,39,30,27,37,43,30,19,32,42,29,24,32,36,29,24),
    city=c(3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2))
# Multicollinearity: unstable coefficient, incorrect signs
# Detecting multicollinearity: compute a correlation matrix of the predictor variables and scatterplot
# VIF(Variance inflation factor): 1/tolerance(1-R^2)
install.packages("car")
library(car)
fit = lm(sales~price+ad+loc+volume+income,newfood)
vif(fit)
summary(fit)

# better off using forward/backward/stepwise regression, ridge regression, principal components regression
scatterplotMatrix(~temp+density+rate+am+defect,quality)

# deviance
deviance(lm(sales~1,newfood)) # null model leave 184k unexplained
deviance(lm(sales~ad,newfood)) # model with ad leaves 182k unexplained, ad explains 2521
deviance(lm(sales~ad+volume,newfood)) # model with both leaves 87k unexplained, ad and volume expalin 96k

# anova and drop1
fit = lm(sales~ad+volume,newfood)
anova(fit)
drop1(fit) 
fit2 = lm(sales~volume+ad,newfood)
anova(fit2)
drop1(fit2)

# The f-test of "overall significance"
# Residual plot helps us to understand how well the model fits the data
# plot(fit), plot(fit,which=1), plot(fit,which=2), 1 residual, 2 QQ plot
machine = data.frame(age=c(2,5,9,3,8), cost=c(6,13,23,5,22))
fit = lm(cost ~ age, machine)
par(mfrow=c(2,2)) # show 2*2 grid of plots
plot(fit)

# Leverage in R
fit = lm(cost~age,machine)
plot(fit,which=c(4,5))
lm.influence(fit)$hat
sum(lm.influence(fit)$hat)








