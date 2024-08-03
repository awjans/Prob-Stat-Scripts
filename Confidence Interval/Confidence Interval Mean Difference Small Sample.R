# Mean Differenece Small Sample Confidence Interval
# N < 30
source("MiscFuncs.R")

n1 <- 14
y1 <- 445 
s1 <- 43

n2 <- 14
y2 <- 533
s2 <- 46

ci <- 0.95

q <- ci + (1 - ci)/2
z <- qt(q, n1 + n2 - 2)

sp <- sqrt((((n1 - 1)*(s1^2))+((n2 - 1)*(s2^2)))/(n1 + n2 - 2))

y_diff <- (y1 - y2)
me <- z*sp*sqrt(1/n1 + 1/n2)

CI <- c(y_diff - me, y_diff + me)
CI
