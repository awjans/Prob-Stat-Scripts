# Mean Small Sample P-Value
# N < 30
source("MarginOfError.R")

alpha <- 0.05

n <- 10
y <- 7.1
s <- 0.12

mu <- 7

#Hnul: y = mu
#Halt: y != mu

rrhi <- qt(1 - alpha/2, n - 1)
rrlo <- qt(alpha/2, n - 1)

tstat <- (y - mu)/meanME(s, n)
pValue <- 2*(pt(-tstat,n-1))

rejectNull <- pValue < alpha
