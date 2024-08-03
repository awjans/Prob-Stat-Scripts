# Proportion Small Sample Hypothesis Test
# N < 30
source("MiscFuncs.R")

# N < 30

p <- 0.14

alpha <- 0.05

n <- 100
phat <- 0.13

# H null
#p = phat
# H alt
#p < phat

RRlo <- qnorm(alpha)
RRhi <- qnorm(1 - alpha)

z = (phat - p)/sqrt((p*(1-p))/n)
