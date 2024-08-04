options(scipen=100,digits=4) #this is to avoid scientific notation

degreeOfFreedom <- function(n) {
  # Calculate the degrees of freedom for the one sample
  df <- n - 1
  df
}

degreesOfFreedom <- function(n1,n2) {
  # Calculate the degrees of freedom for two samples
  df <- n1 + n2 - 2
  df
}

pooledVarianceEstimator <- function(v1,n1,v2,n2) {
  # Calculate the pooled variance
  w1 <- (n1-1)*(v1)
  w2 <- (n2-1)*(v2)
  df <- degreesOfFreedom(n1,n2)
  pv <- (w1+w2)/df
  pv
}

meanSE <- function(s,n) {
  # Calculate the mean Margin of Error
  s/sqrt(n)
}

meanDiffSE <- function(s1,n1,s2,n2) {
  # Calculate the Margin of Error for the difference of two means
  sqrt((s1^2/n1)+(s2^2/n2))
}

pooledMeanDiffSE <- function(n1,n2) {
  # Calculate the Margin of Error for the difference of two means
  sqrt((1/n1)+(1/n2))
}

proportionSE <- function(p,n) {
  # Calculate the population Margin of Error
  sqrt(p*(1-p)/n)
}

proportionDiffSE <- function(p1,n1,p2,n2) {
  # Calculate the Margin of Error for the difference of two proportions
  sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
}

stddevSE <- function(s,n) {
  # Calculate the Margin of Error for a standard deviation
  s/sqrt(2*(n-1))
}

varianceSE <- function(s,n) {
  # Calculate the Margin of Error for a variance
  sqrt(2*s^2/(n-1))
}

rejectionRegionZ <- function(a) {
  # Calculate the rejection region for a z-test
  c(qnorm(a/2),qnorm(1 - a/2))
}

rejectionRegionT <- function(a,df) {
  # Calculate the rejection region for a t-test
  c(qt(a/2,df),qt(1 - a/2,df))
}

rejectionRegionC <- function(a,df) {
  # Calculate the rejection region for a chi-squared test
  c(qchisq(a/2,df),qchisq(1 - a/2,df))
}

rejectionRegionF <- function(a,df1,df2) {
  # Calculate the rejection region for an F-test
  c(qf(a/2,df1,df2),qf(1 - a/2,df1,df2))
}

pValueZ <- function(zstat, twotailed = FALSE) {
  if (zstat >= 0)
    pv <- 1 - pnorm(zstat)
  if (zstat < 0)
    pv <- pnorm(zstat)
  if (twotailed == TRUE)
    pv <- 2*(1 - pnorm(abs(zstat)))
  pv
}

pValueT <- function(tstat, df, twotailed = FALSE) {
  if (tstat >= 0)
    pv <- pt(-tstat, df)
  if (tstat < 0)
    pv <- pt(tstat, df)
  if (twotailed == TRUE)
    pv <- 2*pt(abs(tstat))
  pv
}

pValueC <- function(cstat, df, twotailed = FALSE) {
  if (cstat >= 0)
    pv <- 1 - pchisq(cstat, df)
  if (cstat < 0)
    pv <- pchisq(cstat, df)
  if (twotailed == TRUE)
    pv <- 2*pchisq(abs(cstat), df)
  pv
}

pValueF <- function(fstat, df1, df2, twotailed = FALSE) {
  if (fstat >= 0)
    pv <- 1 - pf(fstat, df1, df2)
  if (fstat < 0)
    pv <- pf(fstat, df1, df2)
  if (twotailed == TRUE)
    pv <- 2*pf(abs(fstat), df1, df2)
  pv
}
