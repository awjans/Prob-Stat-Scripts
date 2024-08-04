# Mean Difference Small Sample P-Value
# N < 30
source("MiscFuncs.R")

alpha <- 0.05

n1 <- 10
y1 <- 0.041
s1 <- 0.017
v1 <- s1^2

n2 <- 13
y2 <- 0.026
s2 <- 0.006
v2 <- s2^2

df <- degreesOfFreedom(n1, n2)

#Hnul: y1 - y2 = 0
#Halt: y1 - y2 > 0

sp = pooledVarianceEstimator(v1, n1, v2, n2)
se = pooledMeanDiffSE(n1, n2)

tstat <- (y1 - y2)/(sp*se)

pValue <- pValueT(tstat, df)

rejectNull <- pValue < alpha
