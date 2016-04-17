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
