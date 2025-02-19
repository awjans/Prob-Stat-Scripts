# Variance Small Sample Confidence Interval
# N < 30
source("MiscFuncs.R")

data <- c(794, 801, 789, 787, 812)

n <- length(data)
y <- mean(data)
s <- sd(data)
v <- s^2

ci <- 0.90

qlo <- (1 - ci)/2
qhi <- 1 - qlo

zlo <- qchisq(qlo, n - 1)
zhi <- qchisq(qhi, n - 1)

CI <- c(((n - 1)*v)/zhi, ((n - 1)*v)/zlo)
CI
