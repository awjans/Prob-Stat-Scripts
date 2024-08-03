# Mean Differenece Large Sample Confidence Interval
# N >= 30
source("MiscFuncs.R")

n1 <- 50
y1 <- 167.3
s1 <- 24.5

n2 <- 50
y2 <- 142.6
s2 <- 17.6

ci <- 0.95

q <- ci + (1 - ci)/2
z <- qnorm(q)

y_diff <- (y1 - y2)
me <- z*meanDiffME(s1, n1, s2, n2)

CI <- c(y_diff - me, y_diff + me)
CI
