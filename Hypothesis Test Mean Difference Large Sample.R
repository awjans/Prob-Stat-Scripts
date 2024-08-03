# N >= 30
n1 <- 30
ybar1 <- 1.64
s1 <- 0.25

n2 <- 35
ybar2 <- 1.43
s2 <- 0.27

a <- 0.01

#H0 <- (ybar1 - ybar2) = 0
#H1 <- (ybar1 - ybar2) > 0

RRlo <- qnorm(a/2)
RRhi <- qnorm(1 - a/2)

z <- (ybar1 - ybar2)/sqrt((s1^2/n1)+(s2^2/n2))


ci <- 1 - a
q <- ci + (1 - ci)/2
z <- qnorm(q)

ybar_diff <- (ybar1 - ybar2)
me <- z*sqrt((s1^2/n1) + (s2^2/n2))

CI <- c(ybar_diff - me, ybar_diff + me)
CI
