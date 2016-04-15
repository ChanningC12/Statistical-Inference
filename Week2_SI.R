# more than 7 out of 8 kids are girls
pbinom(q=6,size=8,prob = 0.5,lower.tail = F) # we want the upper tail

# standard normal distribution: u=0, sd=1, v=1
# 1.28, 1.645, 1.96, 2.33 - 90th, 95th, 97.5th, 99th

qnorm(0.95,mean=0,sd=1)
pnorm(1.96,mean=0,sd=1,lower.tail=F)
pnorm(1160,mean=1020,sd=50,lower.tail=F)
# of (1160-1020)/50 = 2.8
pnorm(2.8,lower.tail=F)
