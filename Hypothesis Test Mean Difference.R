# N >= 30
n1 <- 100
ybar1 <- 13


n2 <- 80
ybar2 <- 5853
s2 <- 1961

ci <- 0.95
q <- ci + (1 - ci)/2
z <- qnorm(q)

ybar_diff <- (ybar1 - ybar2)
me <- z*sqrt((s1^2/n1) + (s2^2/n2))

CI <- c(ybar_diff - me, ybar_diff + me)
CI
