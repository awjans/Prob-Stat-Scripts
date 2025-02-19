# Proportion Small Sample Hypothesis Test
# N < 30
source("MiscFuncs.R")

# N < 30

p <- 0.05

alpha <- 0.01

n <- 1164
phat <- 49/n

# H null
#p = phat
# H alt
#p < phat

rr <- rejectionRegionZ(alpha)

z = (phat - p)/sqrt((p*(1-p))/n)
rr
pValueZ(z, FALSE)
