# Mean Large Sample P-Value
# N >= 30
source("Scripts/MiscFuncs.R")

alpha <- 0.10

n = 32
y = 113.5
v = 100
s = sqrt(v) # 10

mu = 110

#Hnul: y = mu
#Halt: y != mu

rrhi <- qnorm(1 - alpha/2)    # 1.96 @ 5%, 1.64 @ 10%
rrlo <- qnorm(alpha/2)        # -1.96 @ 5%, -1.64 @ 10%

zstat <- (y - mu)/(s/sqrt(n)) # 1.98

rrhi <- qnorm(1 - alpha/2)    # 1.96 @ 5%, 1.64 @ 10%
rrlo <- qnorm(alpha/2)        # -1.96 @ 5%, -1.64 @ 10%

#Reject @ 5: TRUE
#Reject @ 10: TRUE

pValue <- 1 - pnorm(zstat)    # 0.024

rejectNull <- pValue < alpha

