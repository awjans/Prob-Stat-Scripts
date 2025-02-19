# Dual Variance Small Sample Hypothesis Test
# N < 30
source("MiscFuncs.R")

alpha <- 0.10

n1 = 10
y1 = 1.87
v1 = 0.272
s1 = sqrt(v1)

n2 = 10
y2 = 1.84
v2 = 0.099
s2 = sqrt(v2)

df1 = degreesOfFreedom(n1)
df2 = degreesOfFreedom(n2)

rr <- rejectionRegionF(alpha, df1, df2)

fstat <- v1/v2
pValue <- pValueF(fstat, df1, df2, TRUE)

rejectNull <- pValue < alpha

ci <- 0.90

qlo <- (1 - ci)/2
qhi <- 1 - qlo

zlo <- qchisq(qlo, df2)
zhi <- qchisq(qhi, df2)

CI <- c(((n2 - 1)*v2)/zhi, ((n2 - 1)*v2)/zlo)
CI
