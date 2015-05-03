
require(depmixS4)
require(TTR)

sample.dat = dat[1:2000,]
sample.dat$value = EMA(sample.dat$value, n=5)

states = c("Blower", "Drier", "Unknown")

trans = matrix(c(
    c(0.5, 0.49, 0.01),
    c(0.49, 0.5, 0.01),
    c(0.4, 0.4, 0.2)
  ), c(length(states), length(states)), byrow=TRUE)

model = depmix(value ~ 1, data=sample.dat, nstates=length(states), trstart=trans, family=Gamma())
model.fit = fit(model)

plot.ts(scale(sample.dat$value), ylim=c(-5, 5))
lines(scale(model.fit@posterior$state), col="green")

