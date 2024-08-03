# N < 30
# Mean-Difference Small Sample P-Value

alpha <- 0.05

n1 <- 10
y1 <- 0.041
s1 <- 0.017

n2 <- 13
y2 <- 0.026
s2 <- 0.006

#Hnul: y1 - y2 = 0
#Halt: y1 - y2 > 0

sp = sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 - 2))

tstat <- (y1 - y2)/(sp*sqrt((1/n1)+(1/n2)))
pValue <- pt(-tstat, n1 + n2 - 2)
