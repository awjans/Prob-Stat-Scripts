# Proportion Large Sample Confidence Interval
# N >= 30
source("Scripts/MiscFuncs.R")

n <- 420
p <- 278/420

ci <- 0.95

q <- ci + (1 - ci)/2
z <- qnorm(q)

se <- proportionSE(p,n)

me <- z*se

CI <- c(p - me, p + me)
CI
