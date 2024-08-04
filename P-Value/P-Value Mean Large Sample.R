# Mean Large Sample P-Value
# N >= 30
source("MiscFuncs.R")

alpha <- 0.10

n = 32
y = 113.5
v = 100
s = sqrt(v) # 10

mu = 110

#Hnul: y = mu
#Halt: y != mu

se <- meanSE(s, n)

zstat <- (y - mu)/(se) # 1.98

pValue <- pValueZ(zstat)    # 0.024

rejectNull <- pValue < alpha

