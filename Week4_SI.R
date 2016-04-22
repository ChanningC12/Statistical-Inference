# Calculating Power
mu0 = 30
mua = 32
sigma = 4
n = 16
alpha = 0.05
z = qnorm(1-alpha)
pnorm(mu0 + z*sigma/sqrt(n), mean = mu0, sd = sigma/sqrt(n), lower.tail = F)
pnorm(mu0 + z*sigma/sqrt(n), mean = mua, sd = sigma/sqrt(n), lower.tail = F)

# T-test power
power.t.test(n=16,delta=2,sd=4,type="one.sample",alt="one.side")$power  # delta is the difference of means
power.t.test(n=16,power=0.8,sd=4,type="one.sample",alt="one.side")$delta


# Bootstrap, use the distribution by the observed data to approximate its sampling distribution
x = father.son$sheight
n = length(x)
B = 10000
resample = matrix(sample(x,n*B,replace=T),B,n)
resampledMedians = apply(resample,1,median)
plot(density(resampledMedians),col="red")
abline(v=median(resampledMedians))
sd(resampledMedians)
quantile(resampledMedians,c(0.025,0.975))

library(ggplot2)
# Estimate median distribution of the sampling
g = ggplot(data.frame(medians = resampledMedians),aes(x=medians))
g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g

# Group Comparison, Permutation Test
subdata = InsectSprays[InsectSprays$spray %in% c("B","C"),]
y = subdata$count
group = as.character(subdata$spray)
testStat = function(w,g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat = testStat(y,group)
permutations = sapply(1:10000,function(i) testStat(y,sample(group)))
observedStat
