# Mean Difference Large Sample P-Value
# N >= 30
source("MiscFuncs.R")

alpha <- 0.05

n1 <- 40
y1 <- 0.041
s1 <- 0.017
v1 <- s1^2

n2 <- 43
y2 <- 0.026
s2 <- 0.006
v2 <- s2^2

df <- degreesOfFreedom(n1, n2)

#Hnul: y1 - y2 = 0
#Halt: y1 - y2 > 0

se = meanDiffSE(s1, n1, s2, n2)

zstat <- (y1 - y2)/(se)

pValue <- pValueZ(zstat)

rejectNull <- pValue < alpha
