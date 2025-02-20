# Standard Deviation Large Sample P-Value
# N >= 30
source("MiscFuncs.R")

alpha <- 0.05

n = 45
y = 835
v = 1820
s = sqrt(v)

S = 35
V = S^2
df = degreesOfFreedom(n)

#Hnul: s = S
#Halt: s > S

cstat <- ((n - 1)*v)/V

pValue <- pValueC(cstat, df)

rejectNull <- pValue < alpha
