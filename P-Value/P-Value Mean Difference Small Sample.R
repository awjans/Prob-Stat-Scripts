# Mean Difference Small Sample P-Value
# N < 30
source("MiscFuncs.R")

N1 <- 10
Ybar1 <- 0.041
S1 <- 0.017
V1 <- S1^2

N2 <- 13
Ybar2 <- 0.038
S2 <- 0.006
V2 <- S2^2

df <- degreesOfFreedom(c(N1, N2))

H0 <- mu1 - mu2 == 0
H1 <- mu1 - mu2 > 0 # Statistic is One-Tailed Test if H1 is less than or greater than

nom = (Ybar1 - Ybar2) # - [mu1 - mu2] # If H0 is not equal to 0, then subtract the difference from the numerator
se = pooledMeanDiffSE(S1, N1, S2, N2)

zstat <- nom/se

pValue <- pValueZ(zstat)

alpha <- 0.05

rejectNull <- pValue < alpha
