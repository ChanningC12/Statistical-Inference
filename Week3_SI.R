sp = sqrt((7*15.34^2 + 20*18.23^2)/(8+21-2))
132.86 - 127.44 + c(-1,1)*qt(.975,27)*sp*(1/8 + 1/21)^0.5

g1 = c(112,142,134,128,131,134,129,140)
g2 = c(123,136,129,125,124,128,134,134)

n1 = length(g1)
n2 = length(g2)
sp = sqrt(((n-1)*sd(x1)^2) + (n2-1)*sd(x2)^2) / (n1+n2-2))
mean(g2)
mean(g1)
md = mean(g2) - mean(g1)
semd = sp * sqrt(1/n1 + 1/n2)
rbind(
md + c(-1,1)*qt(0.975,n1+n2-2)*semd,
t.test(g2,g1,paired = FALSE, var.equal = TRUE)$conf,
t.test(g2,g1,paired = TRUE)$conf
    )

library(datasets)
data(ChickWeight)
library(reshape2)
library(dplyr)
wideCW = dcast(ChickWeight,Diet + Chick ~ Time, value.var = "weight") # reshape, long to wide
names(wideCW)[(-(1:2))] = paste("time",names(wideCW)[-(1:2)],sep="")
wideCW = mutate(wideCW,
                gain = time21 - time0
                )

wideCW14 = subset(wideCW,Diet %in% c(1,4))
rbind(
t.test(gain~Diet,paired = F, var.equal=T,data=wideCW14)$conf,
t.test(gain~Diet,paired = F, var.equal=F,data=wideCW14)$conf
    )






