# Mean Hypothesis Test Small Sample
# N < 30
source("Scripts/MiscFuncs.R")

n <- 8
y <- 2959
s <- 39.1

m <- 3000

df = n - 1

a = 0.025

rr <- qt(a, df)

se <- meanSE(s, n)

t <- (y - m)/se

pValue <- pt(t,n-1)

rejectNull <- pValue < alpha
