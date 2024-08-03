# Mean Difference Small Sample Hypothesis Test
# N < 30
source("MiscFuncs.R")

n1 <- 11
y1 <- 0.043
#v1 <- 195.56
#s1 <- sqrt(v1)
s1 <- 0.018
v1 <- s1^2

n2 <- 15
y2 <- 0.027
#v2 <- 160.22
#s2 <- sqrt(v2)
s2 <- 0.014
v2 <- s2^2

m1 <- 0.00
m2 <- 0.00

df = degreesOfFreedom(n1, n2)

a = 0.05

rr <- qt(a, df)

sp <- sqrt(pooledVarianceEstimator(v1, n1, v2, n2))
se <- pooledMeanDiffSE(n1, n2)

t <- ((y1 - y2) - (m1 - m2))/(sp*se)

pValue <- pt(-t,df)

rejectNull <- pValue < a

