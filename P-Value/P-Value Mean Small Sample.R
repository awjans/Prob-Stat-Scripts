# Mean Small Sample P-Value
# N < 30
source("MiscFuncs.R")

alpha <- 0.05

n <- 10
y <- 7.1
s <- 0.12

mu <- 7

df <- degreeOfFreedom(n)

#Hnul: y = mu
#Halt: y < mu

se = meanSE(s, n)

tstat <- (y - mu)/se

pValue <- pValueT(tstat, df)

rejectNull <- pValue < alpha

