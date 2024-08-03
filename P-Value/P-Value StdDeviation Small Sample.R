# Standard Deviation Large Sample P-Value
# N >= 30
source("Scripts/MiscFuncs.R")

alpha <- 0.05

n = 45
y = 835
v = 1820
s = sqrt(v)

S = 35
V = S^2
df = n - 1

#Hnul: s = S
#Halt: s > S

rrhi <- qchisq(1 - alpha/2, df)
rrlo <- qchisq(alpha/2, df)

cstat <- ((n - 1)*v)/V
pValue <- pchisq(cstat, df)

rejectNull <- pValue < alpha
