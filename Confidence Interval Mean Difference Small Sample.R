# N < 30
n1 <- 14
ybar1 <- 445 
s1 <- 43

n2 <- 14
ybar2 <- 533
s2 <- 46

ci <- 0.95

q <- ci + (1 - ci)/2
z <- qt(q, n1 + n2 - 2)

sp <- sqrt((((n1 - 1)*(s1^2))+((n2 - 1)*(s2^2)))/(n1 + n2 - 2))

ybar_diff <- (ybar1 - ybar2)
me <- z*sp*sqrt(1/n1 + 1/n2)

CI <- c(ybar_diff - me, ybar_diff + me)
CI
