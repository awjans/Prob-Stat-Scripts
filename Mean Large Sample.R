# Mean Large Sample
# N >= 30
source("MiscFuncs.R")

alpha <- 0.05 # 1 - ci
ci <- 1 - alpha # 0.90

N <- 32
Ybar <- 113.5
V <- 100
S <- sqrt(V) # 10

mu = 110

H0 <- Ybar == mu
H1 <- Ybar != mu

rr <- rejectionRegionZ(alpha)

zstat <- (Ybar - mu)/meanSE(S, N) # 1.98

pValue <- pValueZ(zstat, TRUE)    # 0.024

rejectNull <- pValue < alpha

CI <- confidenceIntervalZ(Ybar, meanSE(S, N), ci)
CI

