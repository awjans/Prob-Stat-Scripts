# Mean Small Sample
# N < 30
source("MiscFuncs.R")

alpha <- 0.05 # 1 - ci
ci <- 1 - alpha # 0.90

N <- 10
Ybar <- 113.5
V <- 100
S <- sqrt(V) # 10

mu = 110

H0 <- Ybar == mu
H1 <- Ybar != mu

df <- degreesOfFreedom(N)

rr <- rejectionRegionT(alpha, df)

tstat <- (Ybar - mu)/meanSE(S, N)

pValue <- pValueT(tstat, TRUE) 

rejectNull <- pValue < alpha

CI <- confidenceIntervalT(Ybar, meanSE(S, N), df, ci)
CI
