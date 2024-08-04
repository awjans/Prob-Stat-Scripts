# Proportion Small Sample P-Value
# N >= 30
source("MiscFuncs.R")

alpha <- 0.10

n <- 21
phat <- 0.5

p <- 0.60

#Hnul: phat = p
#Halt: phat != p

se <- proportionSE(phat, n)

tstat <- (phat - p)/(se) # 1.98

pValue <- pValueT(tstat, TRUE)    # 0.024

rejectNull <- pValue < alpha

