# Dual Variance Small Sample Hypothesis Test
# N < 30
source("Scripts/MiscFuncs.R")

alpha <- 0.02

n1 = 14
y1 = 3.63
v1 = 2.74095
s1 = sqrt(v1)

n2 = 14
y2 = 4.7
v2 = 0.17443
s2 = sqrt(v2)

df1 = degreeOfFreedom(n1)
df2 = degreeOfFreedom(n2)

rrhi <- qf(1 - alpha/2, df1, df2)
rrlo <- qf(alpha/2, df1, df2)

fstat <- v1/v2
pValue = 2*(1 - pf(fstat, df1, df2))

rejectNull <- pValue < alpha
