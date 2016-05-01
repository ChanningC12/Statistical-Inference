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
par(mfrow=c(1,2))
fit = tree(GPA~Verbal+Math,gpa,mindev=1e-6)
plot(fit)
text(fit,cex=.8)
fit
partition.tree(fit, cex=.8)
fit2 = prune.tree(fit,best=5)
plot(fit2)
text(fit2,cex=.8)
fit2
partition.tree(fit2, cex=.8)


