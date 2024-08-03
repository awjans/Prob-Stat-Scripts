# Mean Difference Large Sample Hypothesis Test
# N >= 30
source("MiscFuncs.R")

n1 <- 30
y1 <- 1.64
s1 <- 0.25

n2 <- 35
y2 <- 1.43
s2 <- 0.27

a <- 0.01

#H0 <- (y1 - y2) = 0
#H1 <- (y1 - y2) > 0

RRlo <- qnorm(a/2)
RRhi <- qnorm(1 - a/2)

z <- (y1 - y2)/sqrt((s1^2/n1)+(s2^2/n2))


ci <- 1 - a
q <- ci + (1 - ci)/2
z <- qnorm(q)

y_diff <- (y1 - y2)
me <- z*sqrt((s1^2/n1) + (s2^2/n2))

CI <- c(y_diff - me, y_diff + me)
CI
