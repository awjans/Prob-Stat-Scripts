# Variance Small Sample Confidence Interval
# N < 30
source("MiscFuncs.R")

data <- c(2050, 2352, 1901, 2017, 2365, 1833, 2151, 2196, 2228, 2069, 1874, 2133, 2027, 2342, 2336, 2451, 1996, 2111, 2303, 1777)

n <- length(data)
y <- mean(data)
s <- sd(data)
v <- s^2

ci <- 0.95

qlo <- (1 - ci)/2
qhi <- 1 - qlo

zlo <- qchisq(qlo, n - 1)
zhi <- qchisq(qhi, n - 1)

CI <- c(((n - 1)*v)/zhi, ((n - 1)*v)/zlo)
CI
