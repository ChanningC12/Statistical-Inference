# more than 7 out of 8 kids are girls
pbinom(q=6,size=8,prob = 0.5,lower.tail = F) # we want the upper tail

# standard normal distribution: u=0, sd=1, v=1
# 1.28, 1.645, 1.96, 2.33 - 90th, 95th, 97.5th, 99th

qnorm(0.95,mean=0,sd=1)
pnorm(1.96,mean=0,sd=1,lower.tail=F)
pnorm(1160,mean=1020,sd=50,lower.tail=F)
# of (1160-1020)/50 = 2.8
pnorm(2.8,lower.tail=F)
qnorm(0.75,mean=1020,sd=50)

# Poisson Distribution, Used to model counts
# P(X = x, lambda) = (lambda**x * e**(-lambda)) / x!
# The number of people show up at a bus stop is Poisson distribution with mean of 2.5 per hour
# Watching 4 hours, what is the probability that 3 or fewer people show up for the whole time
ppois(3,lambda = 2.5*4)

# Asymptotics
# Law of Large Number
n = 1000
means = cumsum(rnorm(n))/(1:n)
means = cumsum(sample(0:1,n,replace = T))/(1:n)
plot(means)

# The CLT, the distribution of averages of iid variables (properly normalized) becomes that of a standard normal as the sample size increses
# roll n dice
0.56 + c(-1,1)*qnorm(0.975)*sqrt(0.56*0.44/100)
